tool_exec <- function(in_params, out_params) {

  # ----- Install required packages ------------------------------------------

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

  # ----- Set input/output parameters ----------------------------------------

  # TODO: Try indexing in_params like in_params[["workingDir"]] so it returns
  # NULL if missing rather than stopping the program.

  workingDir <- in_params[[1]]       # Working directory where model files will be stored
  inputRasterFiles <- in_params[[2]] # List of input raster filenames
  trainingDatasetFile <- in_params[[3]]  # Filename of point feature classified by wetland type
  fieldName <- in_params[[4]]        # Classification field name within the point feature class
  isWetLabel <- in_params[[5]]       # Classification label for is-a-wetland
  notWetLabel <- in_params[[6]]      # Classification label for not-a-wetland. Could be expanded to allow multiple names
  modelName <- in_params[[7]]        # Name for the generated RF model
  calcStats <- in_params[[8]]
  probRasterName <- out_params[[1]]

  # Setup --------------------------------------------------------------------

  if (!file.exists(workingDir)) {
    stop("Working directory: '", workingDir, "' does not exist")
  }

  setwd(workingDir)

  # Set up logging
  logFilename <- paste0(modelName, "_build.log")
  file.create(logFilename)

  cat(paste0("Current working directory: ", workingDir, "\n"), file = logFilename)

  # Validate parameters ------------------------------------------------------

  if (length(inputRasterFiles) < 1) {
    errMsg <- "Error: Must provide at least one input raster.\n"
    cat(errMsg, file = logFilename, append = TRUE)
    stop(errMsg)
  }

  lapply(inputRasterFiles, function(inputRasterFile) {
    if (!file.exists(inputRasterFile)) {
      errMsg <- paste0("Input raster: '", inputRasterFile, "' does not exist.\n")
      cat(errMsg, file = logFilename, append = TRUE)
      stop(errMsg)
    }
  })

  if (!file.exists(trainingDatasetFile)) {
    errMsg <- paste0("Training dataset: '", trainingDatasetFile, "' does not exist.\n")
    cat(errMsg, file = logFilename, append = TRUE)
    stop(errMsg)
  }

  # Load classification points -----------------------------------------------

  # Load the points training dataset
  allPoints <- terra::vect(trainingDatasetFile)

  # Keep only the column with the input field that holds the wetland Class
  classPoints <- allPoints[, fieldName]

  # Rename the column heading to class
  names(classPoints)[1] <- "class"

  # Load input rasters -------------------------------------------------------

  # Load each raster individually
  rasterList <- lapply(
    inputRasterFiles,
    function(file) terra::rast(file)
  )

  # Make sure rasters are aligned (with the first input raster)
  rasterList <- WetlandTools::alignRasters(rasterList[[1]], rasterList)

  # Combine individual rasters into a stack
  rasterStack <- c(rasterList[[1]])
  for (i in 2:length(rasterList)) {
    rasterStack <- c(rasterStack, rasterList[[i]])
  }

  # Save input raster names
  rasterNames <- names(rasterStack)
  rasterNamesFilename <- paste0(modelName, ".RasterList")
  writeLines(rasterNames, rasterNamesFilename)

  cat(paste0("Raster names saved in ", rasterNamesFilename, "\n"), file = logFilename, append = TRUE)

  # Extract point values -----------------------------------------------------

  # Sample variable readings at point locations
  pointValues <- terra::extract(rasterStack, classPoints, method = "simple")

  # Include point classification values
  pointValues["class"] <- terra::values(classPoints)

  # Remove point "ID" column
  pointValues <- pointValues[,-1]

  # Remove points with NA values
  pointValues <- na.omit(pointValues)

  # Make sure there are at least some training dataset points classified with
  # the given wetland/non-wetland labels
  correctlyLabeledRows <- pointValues$class == isWetLabel | pointValues$class == notWetLabel
  if (sum(correctlyLabeledRows) == 0) {
    errMsg <- paste0("Error: No points in the training dataset with the
                     specified wetland/non-wetland classes: '", isWetLabel,
                     "'/'", notWetLabel, "'.")
    cat(errMsg, file = logFilename, append = TRUE)
    stop(errMsg)
  }

  # Remove points that aren't labeled either "wetland" or "non-wetland"
  pointValues <- pointValues[correctlyLabeledRows,]

  # Convert class values to factors since Random Forest can't use strings as
  # predictor variables
  pointValues$class <- factor(pointValues$class)

  cat("Ground-truth classifications:\n", file = logFilename, append = TRUE)
  capture.output(summary(pointValues$class), file = logFilename, append = TRUE)

  # Build Random Forest model ------------------------------------------------

  # Build a model that predicts "class" from all input variables
  rfModel <- randomForest::randomForest(
    class ~ .,
    data = pointValues,
    ntree = 200,
    importance = TRUE
  )

  # Log model information
  capture.output(rfModel, file = logFilename, append = TRUE)
  capture.output(randomForest::importance(rfModel), file = logFilename, append = TRUE)

  # Save the model
  modelFilename <- paste0(modelName, ".RFmodel")
  save(rfModel, file = modelFilename)
  cat(paste0("Model saved in: ", modelFilename, "\n"), file = logFilename, append = TRUE)

  # Plot model statistics ----------------------------------------------------

  # Model error rates
  dev.new()
  plot(rfModel, main = paste0(modelName, "_rfclass"))
  dev.copy(win.metafile, paste0(modelName, "_rfclass.wmf"))
  dev.off()

  # Model variable importance
  dev.new()
  randomForest::varImpPlot(rfModel, sort = TRUE, main = paste0(modelName, "_importance"))
  dev.copy(win.metafile, paste0(modelName, "_importance.wmf"))
  dev.off()

  # Generate probability raster ----------------------------------------------

  if (!is.null(probRasterName) && nchar(probRasterName) > 0) {

    # For faster debugging: shrink the area to predict
    #rasterStack <- terra::crop(rasterStack, terra::ext(553800, 561200, 5224100, 5231100)) # Puyallup
    #rasterStack <- terra::crop(rasterStack, terra::ext(550800, 558700, 5185900, 5191500)) # Mashel

    # Predict class probability raster
    probRaster <- terra::predict(
      rasterStack,
      rfModel,
      na.rm = TRUE,
      type = "prob"
    )

    wetlandProbRaster <- probRaster[[isWetLabel]]

    # Write to disk; file type based on extension used for probRasterName
    terra::writeRaster(wetlandProbRaster,probRasterName,overwrite=TRUE)

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
      pred <- ROCR::prediction(pointValues$prob, pointValues$class, label.ordering = c(isWetLabel, notWetLabel))
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

  # Test in Puyallup region (BIGLAPTOP)
  tool_exec(
    in_params = list(
      workingDir = "C:/Work/netmapdata/Puyallup",
      inputRasterFiles = list("grad_300.flt", "dev_300.flt", "plan_300.flt", "prof_300.flt"),
      trainingDatasetFile = "wetlandPnts.shp",
      fieldName = "NEWCLASS",
      isWetLabel = "WET",
      notWetLabel = "UPL",
      modelName = "puy",
      calcStats = TRUE
    ),
    out_params = list(probRasterName = "puy_prob")
  )

  # Test in Puyallup region (WORK2 desktop)
  tool_exec(
    in_params = list(
      workingDir = "E:/NetmapData/Puyallup",
      inputRasterFiles = list("grad_300.flt", "dev_300.flt", "plan_300.flt", "prof_300.flt"),
      trainingDatasetFile = "wetlandPnts.shp",
      fieldName = "NEWCLASS",
      isWetLabel = "WET",
      notWetLabel = "UPL",
      modelName = "puy",
      calcStats = TRUE
    ),
    out_params = list(probRasterName = "puy_prob")
  )

  # Test in Mashel region
  tool_exec(
    in_params = list(
      workingDir = "C:/Work/netmapdata/Mashel",
      inputRasterFiles = list("grad_50.flt", "dev_50.flt", "plan_50.flt", "prof_50.flt"),
      trainingDatasetFile = "PtAllPUY_version_02.shp",
      fieldName = "NEWCLASS",
      isWetLabel = "WET",
      notWetLabel = "UPL",
      modelName = "mas"
    ),
    out_params = list(probRasterName="")
  )

 }
