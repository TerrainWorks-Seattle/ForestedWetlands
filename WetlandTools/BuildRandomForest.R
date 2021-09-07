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

  workingDir <- in_params[[1]]         # Working directory where model files will be stored
  inputRasterFiles <- in_params[[2]]   # List of input raster filenames
  inputShapeFiles <- in_params[[3]]    # List of input shape filenames
  trainingPointsFile <- in_params[[4]] # Filename of point feature classified by wetland type
  classFieldName <- in_params[[5]]     # Name of the class field in the training dataset
  wetlandClass <- in_params[[6]]       # Class name for wetlands
  nonwetlandClass <- in_params[[7]]    # Class name for non-wetlands
  modelName <- in_params[[8]]          # Name for the Random Forest model
  calcStats <- in_params[[9]]          # Whether or not to calculate ROC statistics for the built model
  probRasterName <- out_params[[1]]    # Filename of the generated wetland probability raster

  # Setup ----------------------------------------------------------------------

  if (!file.exists(workingDir))
    stop("Could not find working directory: '", workingDir, "'\n")

  setwd(workingDir)

  # Set up logging
  logFilename <- paste0(modelName, "_build.log")
  file.create(logFilename)

  cat(paste0("Current working directory: ", workingDir, "\n"), file = logFilename)

  # Validate parameters --------------------------------------------------------

  # Make sure at least one input raster or shape was given
  if (length(inputRasterFiles) == 0 && length(inputShapeFiles) == 0)
    logAndStop("Must provide at least one input raster or shape\n")

  # Make sure all input raster files exist
  lapply(inputRasterFiles, function(inputRasterFile) {
    if (!file.exists(inputRasterFile))
      logAndStop(paste0("Could not find input raster: '", inputRasterFile, "'\n"))
  })

  # Make sure all input shape files exist
  lapply(inputShapeFiles, function(inputShapeFile) {
    if (!file.exists(inputShapeFile))
      logAndStop(paste0("Could not find input shape: '", inputShapeFile, "'\n"))
  })

  # Make sure training points file exists
  if (!file.exists(trainingPointsFile))
    logAndStop(paste0("Could not find training points dataset: '", trainingPointsFile, "'\n"))

  # Load input data ------------------------------------------------------------

  # Load rasters
  rasterList <- lapply(inputRasterFiles, function(file) terra::rast(file))

  # Load shapes
  shapeList <- lapply(inputShapeFiles, function(file) terra::vect(file))

  # Build training dataset -----------------------------------------------------

  # Load the training points
  trainingPoints <- terra::vect(trainingPointsFile)

  # Define the training dataset. This will store all the predictor variables as
  # well as the response class variable
  trainingDf <- data.frame(
    class = terra::values(trainingPoints)[[classFieldName]]
  )

  # Add predictor variables from input rasters
  for (raster in rasterList) {
    rasterValues <- TerrainWorksUtils::extractRasterValues(raster, trainingPoints, stringsAsFactors = FALSE)
    trainingDf <- cbind(trainingDf, rasterValues)
  }

  # Add predictor variables from input shapes
  for (shape in shapeList) {
    # Project the points into the same CRS as the shape
    projectedPoints <- terra::project(trainingPoints, shape)

    # Extract shape value(s) at each point
    shapeValues <- terra::extract(shape, projectedPoints)[,-1]

    if (ncol(shape) == 1) {
      # Add single variable to training dataset (converting to factor if char)
      trainingDf[[names(shape)]] <- shapeValues
    } else {
      # Add multiple variables to training dataset (converting to factor if char)
      for (varName in names(shapeValues)) {
        trainingDf[[varName]] <- shapeValues[[varName]]
      }
    }
  }

  # Convert all character variables to factor
  for (varName in names(trainingDf)) {
    if (is.character(trainingDf[[varName]])) {
      trainingDf[[varName]] <- factor(trainingDf[[varName]])
    }
  }

  # Remove training data with NA values
  trainingDf <- na.omit(trainingDf)

  # Check that at least some training data are classified with the given
  # wetland/non-wetland class names
  correctlyLabeledPointIndices <- trainingDf$class == wetlandClass | trainingDf$class == nonwetlandClass
  if (sum(correctlyLabeledPointIndices) == 0)
    logAndStop(paste0("No entries in the training dataset have the given
                     wetland/non-wetland classes: '", wetlandClass, "'/'",
                      nonwetlandClass, "'."))

  # Remove training data that aren't classified with the given
  # wetland/non-wetland class names
  trainingDf <- trainingDf[correctlyLabeledPointIndices,]

  cat("Ground-truth classifications:\n", file = logFilename, append = TRUE)
  capture.output(summary(trainingDf$class), file = logFilename, append = TRUE)

  # Build Random Forest model --------------------------------------------------

  # Build a model that predicts wetland/non-wetland class from all input variables
  rfModel <- randomForest::randomForest(
    formula = class ~ .,
    data= trainingDf,
    ntree = 200,
    importance = TRUE
  )

  # Record all input variables and their levels (if factor)
  inputVars <- list()
  for (inputVar in names(trainingDf)) {
    if (inputVar == "class") next
    inputVars[[inputVar]] <- NA
    if (is.factor(trainingDf[[inputVar]]))
      inputVars[[inputVar]] <- levels(trainingDf[[inputVar]])
  }

  # Store the model and its required input variables
  modelInfo <- list(
    model = rfModel,
    inputVars = inputVars
  )

  # Save the model information to a file
  modelFilename <- paste0(modelName, ".RFmodel")
  save(modelInfo, file = modelFilename)

  # Log model information
  capture.output(rfModel, file = logFilename, append = TRUE)
  capture.output(randomForest::importance(rfModel), file = logFilename, append = TRUE)
  cat(paste0("Model saved in: ", modelFilename, "\n"), file = logFilename, append = TRUE)

  # Plot model statistics ----------------------------------------------------

  # Display model error rates plot
  errorRateNames <- colnames(rfModel$err.rate)
  dev.new()
  plot(rfModel, main = paste0(modelName, "_rfclass"))
  legend("topright", errorRateNames, col = seq_along(errorRateNames), cex = 0.8, fill = seq_along(errorRateNames))
  dev.copy(jpeg, paste0(modelName, "_rfclass.jpg"))
  dev.off()

  # Display model variable importance plot (if multiple variables were given)
  varCount <- ncol(trainingDf) - 1 # Don't count the class variable
  if (varCount > 1) {
    dev.new()
    randomForest::varImpPlot(rfModel, sort = TRUE, main = paste0(modelName, "_importance"))
    dev.copy(jpeg, paste0(modelName, "_importance.jpg"))
    dev.off()
  }

  # Calculate ROC statistics ---------------------------------------------------

  if (calcStats) {

    testDf <- trainingDf

    # Calculate wetland probability for each test dataset entry
    wetProb <- predict(
      rfModel,
      type = "prob",
      newdata = testDf
    )[,wetlandClass]

    # Calculate ROC stats
    rocStats <- TerrainWorksUtils::calcRocStats(
      classes = testDf$class,
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
    abline(a = 0, b = 1, lty = 2)
    dev.copy(jpeg, paste0(modelName, "_roc.jpg"))
    dev.off()

    # Display precision-recall plot
    dev.new()
    ROCR::plot(rocStats$precision, colorize = TRUE, main = paste0(modelName, "_prc"))
    dev.copy(jpeg, paste0(modelName, "_prc.jpg"))
    dev.off()

    # Display accuracy plot
    dev.new()
    ROCR::plot(rocStats$accuracy, main = paste0(modelName, "_acc"))
    dev.copy(jpeg, paste0(modelName, "_acc.jpg"))
    dev.off()
  }

  # Generate wetland probability raster ----------------------------------------

  if (!is.null(probRasterName) && !is.na(probRasterName)) {

    if (length(rasterList) == 0)
      stop("Must provide at least one input raster to use as a reference for
           building the probability raster.")

    # Rasterize all input variables
    referenceRaster <- if (length(rasterList) > 0) {
      rasterList[[1]]
    } else {
      extList <- lapply(shapeList, terra::ext)
      terra::rast(
        xmin = min(sapply(extList, terra::xmin)),
        xmax = max(sapply(extList, terra::xmax)),
        ymin = min(sapply(extList, terra::ymin)),
        ymax = max(sapply(extList, terra::ymax)),
        resolution = 1,
        crs = terra::crs(shapeList[[1]])
      )
    }

    inputRasters <- c()

    if (length(rasterList) > 0) {
      alignedRasters <- TerrainWorksUtils::alignRasters(referenceRaster, rasterList)
      for (raster in alignedRasters) {
        inputRasters <- c(inputRasters, raster)
      }
    }

    for (shape in shapeList) {
      #raster <- terra::rasterize(shape, )
    }

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

  # Return ---------------------------------------------------------------------

  return(out_params)

}

# Tests
if (FALSE) {

  # Test in Puyallup region (BIGLAPTOP)
  tool_exec(
    in_params = list(
      workingDir = "C:/Work/netmapdata/Puyallup",
      inputRasterFiles = list("grad_15.tif", "dev_300.tif", "geo_unit.tif"),
      inputShapeFiles = list("lithology.shp"),
      trainingDatasetFile = "wetlandPnts.shp",
      classFieldName = "NEWCLASS",
      wetlandClass = "WET",
      nonwetlandClass = "UPL",
      modelName = "puy_grad15_dev300_geounit_litho",
      calcStats = TRUE
    ),
    out_params = list(probRasterName = NULL)
  )

  # Test in Puyallup region (WORK2)
  tool_exec(
    in_params = list(
      workingDir = "E:/NetmapData/Puyallup",
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

}
