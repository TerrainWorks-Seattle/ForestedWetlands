tool_exec <- function(in_params, out_params) {

  # Install required packages --------------------------------------------------

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
  
  # Define helper functions ----------------------------------------------------
  
  logAndStop <- function(errMsg) {
    cat(errMsg, file = logFilename, append = TRUE)
    stop(errMsg)
  }

  # Set input/output parameters ------------------------------------------------

  # TODO: Try indexing in_params like in_params[["workingDir"]] so it returns
  # NULL if missing rather than stopping the program.

  workingDir <- in_params[[1]]          # Working directory where model files will be stored
  inputRasterFiles <- in_params[[2]]    # List of input raster filenames
  inputShapeFiles <- in_params[[3]]     # List of input shape filenames
  trainingPointsFile <- in_params[[4]] # Filename of point feature classified by wetland type
  classFieldName <- in_params[[5]]      # Name of the class field in the training dataset
  wetlandClass <- in_params[[6]]        # Class name for wetlands
  nonwetlandClass <- in_params[[7]]     # Class name for non-wetlands
  modelName <- in_params[[8]]           # Name for the Random Forest model
  calcStats <- in_params[[9]]
  probRasterName <- out_params[[1]]

  # Setup ----------------------------------------------------------------------

  if (!file.exists(workingDir))
    stop("Working directory: '", workingDir, "' does not exist")

  setwd(workingDir)

  # Set up logging
  logFilename <- paste0(modelName, "_build.log")
  file.create(logFilename)

  cat(paste0("Current working directory: ", workingDir, "\n"), file = logFilename)

  # Validate parameters --------------------------------------------------------

  if (length(inputRasterFiles) == 0 && length(inputShapeFiles) == 0)
    logAndStop("Must provide at least one input raster or shape.\n")

  lapply(inputRasterFiles, function(inputRasterFile) {
    if (!file.exists(inputRasterFile))
      logAndStop(paste0("Input raster: '", inputRasterFile, "' does not exist.\n"))
  })

  if (!file.exists(trainingPointsFile))
    logAndStop(paste0("Training dataset: '", trainingPointsFile, "' does not exist.\n"))

  # Load input data ------------------------------------------------------------

  ## Load input rasters --------------------------------------------------------

  # Create a file that lists the input rasters
  rasterNamesFile <- paste0(modelName, ".RasterList")
  file.create(rasterNamesFile)

  if (length(inputRasterFiles) > 0) {
    if (length(inputRasterFiles) == 1) {
      rasterStack <- terra::rast(inputRasterFiles[[1]])
    } else {
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
    }

    # Write raster names
    rasterNames <- names(rasterStack)
    writeLines(rasterNames, rasterNamesFile)
  }

  cat(paste0("Raster names saved in ", rasterNamesFile, "\n"), file = logFilename, append = TRUE)

  ## Load input shapes ---------------------------------------------------------

  # Create a file that lists the input shapes
  shapeNamesFile <- paste0(modelName, ".ShapeList")
  file.create(shapeNamesFile)

  if (length(inputShapeFiles) > 0) {
    # Load each shape individually
    shapeList <- lapply(
      inputShapeFiles,
      function(shapeFile) terra::vect(shapeFile)
    )

    # Write shape names
    shapeNames <- sub("\\..*$", "", inputShapeFiles)
    writeLines(shapeNames, shapeNamesFile)
  }

  cat(paste0("Shape names saved in ", shapeNamesFile, "\n"), file = logFilename, append = TRUE)

  # Build training dataset -----------------------------------------------------

  # Load the training points
  trainingPoints <- terra::vect(trainingPointsFile)

  # Define the training dataset, which will store both the predictor variables
  # and the response variable (class)
  trainingDf <- data.frame(
    class = terra::values(trainingPoints)[[classFieldName]]
  )

  # If any rasters were input, extract their value(s) at each training point and
  # add those to the training dataset
  if (length(inputRasterFiles) > 0) {
    rasterValues <- terra::extract(rasterStack, trainingPoints, method = "simple")[,-1]
    if (length(inputRasterFiles) == 1) {
      trainingDf[[names(rasterStack)[1]]] <- rasterValues
    } else {
      trainingDf <- cbind(trainingDf, rasterValues)
    }
  }
  
  # If any shapes were input, extract their value(s) at each training point and 
  # add those to the training dataset
  if (length(inputShapeFiles) > 0) {
    for (shape in shapeList) {
      # Project the training points into the same CRS as the shape
      projectedPoints <- terra::project(trainingPoints, shape)

      # Extract shape value(s) at each training point
      shapeValues <- terra::extract(shape, projectedPoints)[,-1]

      if (ncol(shape) == 1) {
        if (is.character(shapeValues))
          shapeValues <- factor(shapeValues)
        trainingDf[[names(shape)]] <- shapeValues 
      } else {
        # Add value(s) to training data
        for (field in names(shapeValues)) {
          # Convert string fields to factors
          if (is.character(shapeValues[[field]]))
            shapeValues[[field]] <- factor(shapeValues[[field]])
          trainingDf[[field]] <- shapeValues[[field]]
        }   
      }
    }
  }

  # Make sure there are at least some training dataset points classified with
  # the given wetland/non-wetland class names
  correctlyLabeledPointIndices <- trainingDf$class == wetlandClass | trainingDf$class == nonwetlandClass
  if (sum(correctlyLabeledPointIndices) == 0)
    logAndStop(paste0("No points in the training dataset with the specified
                     wetland/non-wetland classes: '", wetlandClass, "'/'",
                     nonwetlandClass, "'."))

  # Convert class field to factor
  trainingDf$class <- factor(trainingDf$class)

  # Remove points that aren't labeled wetland/non-wetland
  trainingDf <- trainingDf[correctlyLabeledPointIndices,]

  # Remove points with NA values
  trainingDf <- na.omit(trainingDf)

  cat("Ground-truth classifications:\n", file = logFilename, append = TRUE)
  capture.output(summary(trainingDf$class), file = logFilename, append = TRUE)

  # Build Random Forest model ------------------------------------------------

  # Build a model that predicts "class" from all input variables
  rfModel <- randomForest::randomForest(
    class ~ .,
    data = trainingDf,
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
  predictorVarCount <- ncol(trainingDf) - 1
  if (predictorVarCount > 1) {
    dev.new()
    randomForest::varImpPlot(rfModel, sort = TRUE, main = paste0(modelName, "_importance"))
    dev.copy(win.metafile, paste0(modelName, "_importance.wmf"))
    dev.off()
  }

  # Calculate ROC statistics -------------------------------------------------

  if (calcStats) {

    # Calculate wetland probability for each test point
    wetProb <- predict(
      rfModel,
      type = "prob",
      newdata = trainingDf
    )[,wetlandClass]

    # Calculate ROC stats
    rocStats <- TerrainWorksUtils::calcRocStats(
      classes = trainingDf$class,
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
      inputRasterFiles = list("grad_15.tif", "dev_300.flt"),
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

 }
