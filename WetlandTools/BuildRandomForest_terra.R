tool_exec <- function(in_params, out_params) {

  # ----- Install required packages ------------------------------------------

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

  # ----- Set input/output parameters ----------------------------------------

  # TODO: Try indexing in_params like in_params[["workingDir"]] so it returns
  # NULL if missing rather than stopping the program.

  workingDir <- in_params[[1]]          # Working directory where model files will be stored
  inputRasterFiles <- in_params[[2]]    # List of input raster filenames
  inputShapeFiles <- in_params[[3]]     # List of input shape filenames
  trainingDatasetFile <- in_params[[4]] # Filename of point feature classified by wetland type
  classFieldName <- in_params[[5]]      # Name of the class field in the training dataset
  wetlandClass <- in_params[[6]]        # Class name for wetlands
  nonwetlandClass <- in_params[[7]]     # Class name for non-wetlands
  modelName <- in_params[[8]]           # Name for the Random Forest model
  calcStats <- in_params[[9]]
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

  # Load input data ------------------------------------------------------------

  ## Load input rasters --------------------------------------------------------

  # Load each raster individually
  rasterList <- lapply(
    inputRasterFiles,
    function(rasterFile) terra::rast(rasterFile)
  )

  # Make sure rasters are aligned (with the first input raster)
  rasterList <- TerrainWorksUtils::alignRasters(rasterList[[1]], rasterList)

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

  ## Load input shapes ---------------------------------------------------------

  shapeList <- lapply(
    inputShapeFiles,
    function(shapeFile) terra::vect(shapeFile)
  )

  # Load training dataset ------------------------------------------------------

  # Load the points training dataset
  allPoints <- terra::vect(trainingDatasetFile)

  # Keep only the column with the input field that holds the wetland Class
  classPoints <- allPoints[, classFieldName]

  # Rename the column heading to class
  names(classPoints)[1] <- "class"

  # Extract raster values at point locations
  pointValues <- terra::extract(rasterStack, classPoints, method = "simple")

  # Extract shape values at point locations
  for (shape in shapeList) {
    pointCoords <- allPoints[,-seq_len(ncol(allPoints))]
    projectedPoints <- terra::project(pointCoords, shape)
    intersectionPoints <- terra::intersect(projectedPoints, shape)
    intersectionValues <- terra::values(intersectionPoints)

    # Include each field in the shape
    for (field in names(intersectionValues)) {
      # Convert string fields to factors
      if (is.character(intersectionValues[[field]]))
        intersectionValues[[field]] <- factor(intersectionValues[[field]])
      pointValues[[field]] <- intersectionValues[[field]]
    }
  }

  # Remove point "ID" column
  pointValues <- pointValues[,-1]

  # Include point classification values
  pointValues["class"] <- terra::values(classPoints)

  # Make sure there are at least some training dataset points classified with
  # the given wetland/non-wetland labels
  correctlyLabeledRows <- pointValues$class == wetlandClass | pointValues$class == nonwetlandClass
  if (sum(correctlyLabeledRows) == 0) {
    errMsg <- paste0("Error: No points in the training dataset with the
                     specified wetland/non-wetland classes: '", wetlandClass,
                     "'/'", nonwetlandClass, "'.")
    cat(errMsg, file = logFilename, append = TRUE)
    stop(errMsg)
  }

  # Remove points that aren't labeled either "wetland" or "non-wetland"
  pointValues <- pointValues[correctlyLabeledRows,]

  # Remove points with NA values
  pointValues <- na.omit(pointValues)

  # Convert class field to factor
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
  errorRateNames <- colnames(rfModel$err.rate)
  dev.new()
  plot(rfModel, main = paste0(modelName, "_rfclass"))
  legend("topright", errorRateNames, col= seq_along(errorRateNames), cex=0.8, fill = seq_along(errorRateNames))
  dev.copy(win.metafile, paste0(modelName, "_rfclass.wmf"))
  dev.off()

  # Model variable importance
  dev.new()
  randomForest::varImpPlot(rfModel, sort = TRUE, main = paste0(modelName, "_importance"))
  dev.copy(win.metafile, paste0(modelName, "_importance.wmf"))
  dev.off()

  # Calculate ROC statistics -------------------------------------------------

  if (calcStats) {

    # Calculate wetland probability for each test point
    wetProb <- predict(
      rfModel,
      type = "prob",
      newdata = pointValues[,-which(names(pointValues) == "class")]
    )[,wetlandClass]

    # Calculate ROC stats
    rocStats <- TerrainWorksUtils::calcRocStats(
      classes = pointValues$class,
      probs = wetProb,
      wetlandClass,
      nonwetlandClass
    )

    # Log ROC statistics
    cat(paste0("AUROC: ", rocStats$auc@y.values, "\n"), file = logFilename, append = TRUE)
    capture.output(c(PRBE = rocStats$prbe, cutoff = rocStats$maxPrecisionCutoff), file = logFilename, append = TRUE)
    capture.output(c(accuracy = rocStats$maxAccuracy, cutoff = rocStats$maxAccuracyCutoff), file = logFilename, append = TRUE)

    # Display ROC plot
    dev.new()
    ROCR::plot(rocStats$roc, colorize = TRUE, main = paste0(modelName, "_roc"))
    abline(a = 0, b = 1)
    dev.copy(win.metafile, paste0(modelName, "_roc.wmf"))
    dev.off()

    # Display precision-recall plot
    dev.new()
    ROCR::plot(rocStats$precision, colorize = TRUE, main = paste0(modelName, "_prc"))
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
    wetProbRaster <- probRaster[[wetlandClass]]
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

  # Test in Puyallup region (BIGLAPTOP)
  tool_exec(
    in_params = list(
      workingDir = "C:/Work/netmapdata/Puyallup",
      inputRasterFiles = list("grad_15.tif", "dev_300.tif"),
      inputShapeFiles = list("geo.shp"),
      trainingDatasetFile = "wetlandPnts.shp",
      classFieldName = "NEWCLASS",
      wetlandClass = "WET",
      nonwetlandClass = "UPL",
      modelName = "puy_grad15_dev300_geo",
      calcStats = TRUE
    ),
    out_params = list(probRasterName = NULL)
  )

  # Test in Puyallup region (WORK2)
  tool_exec(
    in_params = list(
      workingDir = "E:/NetmapData/Puyallup",
      inputRasterFiles = list("grad_300.flt", "dev_300.flt", "plan_300.flt", "prof_300.flt"),
      inputShapeFiles = list(),
      trainingDatasetFile = "wetlandPnts.shp",
      classFieldName = "NEWCLASS",
      wetlandClass = "WET",
      nonwetlandClass = "UPL",
      modelName = "puy",
      calcStats = TRUE
    ),
    out_params = list(probRasterName = NULL)
  )

 }
