tool_exec <- function(in_params, out_params) {

  # ----- Install packages ---------------------------------------------------

  if (!requireNamespace("devtools", quietly = TRUE))
    install.packages("devtools", quiet = TRUE)
  if (!requireNamespace("TerrainWorksUtils", quietly = TRUE))
    devtools::install_github("tabrasel/TerrainWorksUtils")
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
  rasterList <- TerrainWorksUtils::alignRasters(rasterList[[1]], rasterList)

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

  # Calculate ROC statistics -------------------------------------------------

  if (calcStats) {

    # Calculate wetland probability for each test point
    wetProb <- predict(
      rfModel,
      type = "prob",
      newdata = pointValues[,-which(names(pointValues) == "class")]
    )[,isWetLabel]

    # Calculate ROC stats
    # rocStats <- TerrainWorksUtils::calcRocStats(
    #   classes = pointValues$class,
    #   probs = wetProb,
    #   isWetLabel,
    #   notWetLabel
    # )

    prediction <- ROCR::prediction(
      predictions = wetProb,
      labels = pointValues$class,
      label.ordering = c(notWetLabel, isWetLabel)
    )

    # Calculate the ROC curve
    roc <- ROCR::performance(prediction, measure = "tpr", x.measure = "fpr")

    # Calculate the area under the ROC curve (AUC)
    auc <- ROCR::performance(prediction, measure = "auc")

    # Calculate the max precision and its cutoff point
    precision <- ROCR::performance(prediction, measure = "prec", x.measure = "rec")
    maxPrecisionIndex <- which.max(methods::slot(precision, "y.values")[[1]])
    prbe <- methods::slot(precision, "y.values")[[1]][maxPrecisionIndex]
    maxPrecisionCutoff <- methods::slot(precision, "x.values")[[1]][maxPrecisionIndex]

    # Calculate the max accuracy and its cutoff point
    accuracy <- ROCR::performance(prediction, measure = "acc")
    maxAccuracyIndex <- which.max(methods::slot(accuracy, "y.values")[[1]])
    maxAccuracy <- methods::slot(accuracy, "y.values")[[1]][maxAccuracyIndex]
    maxAccuracyCutoff <- methods::slot(accuracy, "x.values")[[1]][maxAccuracyIndex]

    # Store ROC stats in a list
    rocStats <- list(
      roc = roc,
      precision = precision,
      accuracy = accuracy,
      prbe = prbe,
      maxPrecisionCutoff = maxPrecisionCutoff,
      maxAccuracy = maxAccuracy,
      maxAccuracyCutoff = maxAccuracyCutoff,
      auc = auc
    )

    # Log ROC statistics
    cat(paste0("AUROC: ", rocStats$auc@y.values, "\n"), file = logFilename, append = TRUE)
    capture.output(c(PRBE = rocStats$prbe, cutoff = rocStats$maxPrecisionCutoff), file = logFilename, append = TRUE)
    capture.output(c(accuracy = rocStats$maxAccuracy, cutoff = rocStats$maxAccuracyCutoff), file = logFilename, append = TRUE)

    # Display ROC plot
    dev.new()
    ROCR::plot(rocStats$roc, main = paste0(modelName, "_roc"))
    abline(a = 0, b = 1)
    dev.copy(win.metafile, paste0(modelName, "_roc.wmf"))
    dev.off()

    # Display precision-recall plot
    dev.new()
    ROCR::plot(rocStats$precision, main = paste0(modelName, "_prc"))
    dev.copy(win.metafile, paste0(modelName, "_prc.wmf"))
    dev.off()

    # Display accuracy plot
    dev.new()
    ROCR::plot(rocStats$accuracy, main = paste0(modelName, "_acc"))
    dev.copy(win.metafile, paste0(modelName, "_acc.wmf"))
    dev.off()
  }

  # Generate wetland probability raster --------------------------------------

  if (!is.null(probRasterName) && !is.na(probRasterName)) {
    # Predict probability rasters for wetland and non-wetland
    probRaster <- terra::predict(
      rasterStack,
      rfModel,
      na.rm = TRUE,
      type = "prob"
    )

    # Save the wetland probability raster
    wetProbRaster <- probRaster[[isWetLabel]]
    terra::writeRaster(
      wetProbRaster,
      filename = paste0(probRasterName, ".tif"),
      overwrite = TRUE
    )

    cat(paste0("Created probability raster: ", paste0(probRasterName, ".tif"), "\n"), file = logFilename, append = TRUE)
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
      modelFile = "puy_dev300_grad15_plan15_prof15.RFmodel",
      inputRasterFiles = list("dev_300.tif", "grad_15.tif", "plan_15.tif", "prof_15.tif"),
      testDataFile <- "wetlandPoints.shp",
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
