tool_exec <- function(in_params, out_params) {
  
  # ----- Install packages ---------------------------------------------------
  
  if (!requireNamespace("devtools", quietly = TRUE))
    install.packages("devtools", quiet = TRUE)
  if (!requireNamespace("WetlandTools", quietly = TRUE))
    devtools::install_github("tabrasel/WetlandTools")
  if (!requireNamespace("randomForest", quietly = TRUE))
    install.packages("randomForest", quiet = TRUE)
  if (!requireNamespace("ROCR", quietly = TRUE))
    install.packages("ROCR", quiet = TRUE)
  if (!requireNamespace("terra", quietly = TRUE))
    install.packages("terra", quiet = TRUE)
  
  # ----- Define helper functions --------------------------------------------
  
  baseFilename <- function(file) {
    return(gsub(basename(file), pattern="\\..*$", replacement=""))
  }
  
  # ----- Set input/output parameters ----------------------------------------
  
  # TODO: Try indexing in_params using the parameter names specified in the ArcGIS tool.
  # Ex: in_params[["Working_Directory"]]
  # Indexing by name returns NULL when missing instead of causing an error when indexing by number.
  
  workingDir <- in_params[[1]]       # Working directory where model files will be stored
  modelFile <- in_params[[2]]        # Filename of the model
  inputRasterFiles <- in_params[[3]] # List of input raster filenames
  testDataFile <- in_params[[4]]     # Points to run the model on
  fieldName <- in_params[[5]]        # Classification field name within the point feature class
  isWetLabel <- in_params[[6]]       # Classwetland
  notWetLabel <- in_params[[7]]
  calcStats <- in_params[[8]]
  probRasterName <- out_params[[1]]
  
  # Setup --------------------------------------------------------------------
  
  setwd(workingDir)
  
  modelName <- baseFilename(modelFile)
  
  # Set up logging
  logFilename <- paste0(modelName, "_run.log")
  file.create(logFilename)
  
  cat(paste0("Current working directory: ", workingDir, "\n"), file = logFilename)
  
  # Validate parameters ------------------------------------------------------
  
  if (!file.exists(modelFile)) {
    errMsg <- paste0("Error: Could not find model file: '", modelFile, "'\n")
    cat(errMsg, file = logFilename, append = TRUE)
    stop(errMsg)
  }
  
  if (length(inputRasterFiles) < 1) {
    errMsg <- "Error: Must provide at least one input raster.\n"
    cat(errMsg, file = logFilename, append = TRUE)
    stop(errMsg)
  }
  
  lapply(inputRasterFiles, function(inputRasterFile) {
    if (!file.exists(inputRasterFile)) {
      errMsg <- paste0("Error: Could not find input raster: '", inputRasterFile, "'\n")
      cat(errMsg, file = logFilename, append = TRUE)
      stop(errMsg)
    }
  })
  
  if (!file.exists(testDataFile)) {
    errMsg <- paste0("Error: Could not find test dataset: '", testDataFile, "'\n")
    cat(errMsg, file = logFilename, append = TRUE)
    stop(errMsg)
  }
  
  # Load Random Forest model -------------------------------------------------
  
  # Load the model
  load(modelFile)
  cat(paste0("Loaded model: ", modelFile), file = logFilename, append = TRUE)
  
  # Load input rasters -------------------------------------------------------
  
  # Make sure the input rasters match those expected by the model
  modelRastersFile <- sub(".RFmodel", ".RasterList", modelFile)
  if (!file.exists(modelRastersFile)) {
    errMsg <- paste0("Error: Could not find model raster list: '", modelRastersFile, "'\n")
    cat(errMsg, file = logFilename, append = TRUE)
    stop(errMsg)
  }
  modelVarNames <- sort(readLines(modelRastersFile))
  inputVarNames <- sort(baseFilename(unlist(inputRasterFiles)))
  if (any(inputVarNames != modelVarNames)) {
    errMsg <- paste0("Input raster names do not match the expected model variables (listed in: '", modelName, ".RasterList')")
    cat(errMsg, file = logFilename, append = TRUE)
    stop(errMsg)
  }
  
  # Load each input raster individually
  rasterList <- lapply(
    inputRasterFiles,
    function(file) terra::rast(file)
  )
  
  # Align all the input rasters (using the first as a reference)
  rasterList <- WetlandTools::alignRasters(rasterList[[1]], rasterList)
  
  # Combine input rasters into a stack
  rasterStack <- c(rasterList[[1]])
  for (i in 2:length(rasterList)) {
    rasterStack <- c(rasterStack, rasterList[[i]])
  }
  
  # Predict test data --------------------------------------------------------
  
  if (!is.null(testDataFile) && !is.na(testDataFile)) {
    
    # Load the points training dataset
    allPoints <- terra::vect(testDataFile)
    
    # Keep only the column with the input field that holds the wetland Class
    classPoints <- allPoints[, fieldName]
    
    # Rename the column heading to Class
    names(classPoints)[1] <- "class"
    
    # Sample variable readings at point locations
    pointValues <- terra::extract(rasterStack, classPoints, method = "simple")
    
    # Include point classification values (as factors, not strings)
    pointValues["class"] <- terra::values(classPoints)
    
    # Remove point "ID" column
    pointValues <- pointValues[,-1]
    
    # Remove points with NA values
    pointValues <- na.omit(pointValues)
    
    # Make sure there are at least some test dataset points classified with
    # the given wetland/non-wetland labels
    correctlyLabeledRows <- pointValues$class == isWetLabel | pointValues$class == notWetLabel
    if (sum(correctlyLabeledRows) == 0) {
      errMsg <- paste0("Error: Found no points in the test dataset with the 
                       specified wetland/non-wetland classes: '", isWetLabel,
                       "'/'", notWetLabel, "'\n")
      cat(errMsg, file = logFilename, append = TRUE)
      stop(errMsg)
    }
    pointValues <- pointValues[correctlyLabeledRows,]
    
    # Convert class values to factors since Random Forest can't use strings as 
    # predictor variables
    pointValues$class <- factor(pointValues$class)
    
    # Run model on these data
    testDataPredictions <- predict(
      rfModel,
      type = "response",
      newdata = pointValues[,-which(names(pointValues) == "class")]
    )
    
    # Log confusion matrix for predicted test data classifications
    capture.output(table(testDataPredictions, pointValues$class), file = logFilename, append = TRUE)
    
  }
  
  # Generate wetland probability raster --------------------------------------
  
  if (!is.null(probRasterName) && !is.na(probRasterName)) {
    # Predict probability raster
    probRaster <- terra::predict(
      rasterStack,
      rfModel,
      na.rm = TRUE,
      type = "prob"
    )
    
    wetlandProbRaster <- probRaster[[isWetLabel]]
    
    # Save probability raster
    terra::writeRaster(
      wetlandProbRaster,
      filename = paste0(probRasterName, ".tif"),
      overwrite = TRUE
    )
    
    cat(paste0("Created probability raster: ", paste0(probRasterName, ".tif"), "\n"), file = logFilename, append = TRUE)
    
    if (calcStats) {
      
      # Sample probability raster readings at point locations
      pointValues <- terra::extract(wetlandProbRaster, classPoints, method = "simple")
      
      # Include point classification values
      pointValues["class"] <- terra::values(classPoints)
      
      # Remove point "ID" column
      pointValues <- pointValues[,-1]
      
      # Remove points with NA values
      pointValues <- na.omit(pointValues)
      
      # Remove points that aren't labeled either "wetland" or "non-wetland"
      correctlyLabeledRows <- pointValues$class == isWetLabel | pointValues$class == notWetLabel
      pointValues <- pointValues[correctlyLabeledRows,]
      
      # Convert class values to factors since Random Forest can't use strings as 
      # predictor variables
      pointValues$class <- factor(pointValues$class)
      
      names(pointValues)[1] <- "prob"
      
      # Calculate ROC statistics
      pred <- ROCR::prediction(pointValues$prob, pointValues$class, label.ordering = c(notWetLabel, isWetLabel))
      roc <- ROCR::performance(pred, measure = "tpr", x.measure = "fpr")
      auc <- ROCR::performance(pred, measure = "auc")
      precision <- ROCR::performance(pred, measure = "prec", x.measure = "rec")
      accuracy <- ROCR::performance(pred, measure = "acc")
      
      cat(paste0("AUROC: ", auc@y.values, "\n"), file = logFilename, append = TRUE)
      
      idx <- which.max(slot(precision, "y.values")[[1]])
      prbe <- slot(precision, "y.values")[[1]][idx]
      cutoff <- slot(precision, "x.values")[[1]][idx]
      capture.output(c(PRBE = prbe, cutoff = cutoff), file = logFilename, append = TRUE)
      
      idx <- which.max(slot(accuracy, "y.values")[[1]])
      maxacc <- slot(accuracy, "y.values")[[1]][idx]
      cutoff <- slot(accuracy, "x.values")[[1]][idx]
      capture.output(c(accuracy = maxacc, cutoff = cutoff), file = logFilename, append = TRUE)
      
      # Plot ROC
      dev.new()
      ROCR::plot(roc, main = paste0(modelName, "_roc"))
      abline(a = 0, b = 1)
      dev.copy(win.metafile, paste0(modelName, "_roc.wmf"))
      dev.off()
      
      # Plot precision
      dev.new()
      ROCR::plot(precision, main = paste0(modelName, "_prc"))
      dev.copy(win.metafile, paste0(modelName, "_prc.wmf"))
      dev.off()
      
      # Plot accuracy
      dev.new()
      ROCR::plot(accuracy, main = paste0(modelName, "_acc"))
      dev.copy(win.metafile, paste0(modelName, "_acc.wmf"))
      dev.off()
    }
  }
  
  # Return -------------------------------------------------------------------
  
  return(out_params)
  
}

# Tests
if (FALSE) {
  
  # Test Puyallup model in Puyallup region (BIGLAPTOP)
  tool_exec(
    in_params = list(
      workingDir = "C:/Work/netmapdata/Puyallup",
      modelFile = "puy.RFmodel",
      inputRasterFiles = list("grad_300.flt", "dev_300.flt", "plan_300.flt", "prof_300.flt"),
      testDataFile <- "wetlandPnts.shp",
      fieldName <- "NEWCLASS",
      isWetLabel <- "WET",
      notWetLabel <- "UPL",
      calcStats <- TRUE
    ),
    out_params = list(probRasterName = "puy_prob")
  )
  
  # Test Puyallup model in Mashel region (BIGLAPTOP)
  tool_exec(
    in_params = list(
      workingDir = "C:/Work/netmapdata/Mashel",
      modelFile = "puy.RFmodel",
      inputRasterFiles = list("grad_300.tif", "dev_300.tif", "plan_300.tif", "prof_300.tif"),
      testDataFile <- "PtAllPuy.shp",
      fieldName <- "NEWCLASS",
      isWetLabel <- "WET",
      notWetLabel <- "UPL",
      calcStats <- TRUE
    ),
    out_params = list(probRasterName = "mas_prob")
  )
  
  tool_exec(
    in_params = list(
      workingDir = "E:/NetmapData/Puyallup",
      modelFile = "puy.RFmodel",
      inputRasterFiles = list("dev_300.flt", "grad_300.flt", "plan_300.flt", "prof_300.flt"),
      testDataFile <- "wetlandPnts.shp",
      fieldName <- "NEWCLASS",
      isWetLabel <- "WET",
      notWetLabel <- "UPL",
      calcStats <- TRUE
    ),
    out_params = list(probRasterName = "puy_prob")
  )
  
  # Test Puyallup model in Mashel region (WORK2 desktop)
  tool_exec(
    in_params = list(
      workingDir = "E:/NetmapData/Mashel",
      modelFile = "puy.RFmodel",
      inputRasterFiles = list("grad_60.flt", "dev_60.tif", "plan_60.flt", "prof_60.flt"),
      testDataFile <- "mashelPoints.shp",
      fieldName <- "NEWCLASS",
      isWetLabel <- "WET",
      notWetLabel <- "UPL",
      calcStats <- FALSE
    ),
    out_params = list(probRasterName = NULL)
  )
  
}
