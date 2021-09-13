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
  trainingPointsFile <- in_params[[3]] # Filename of point feature classified by wetland type
  classFieldName <- in_params[[4]]     # Name of the class field in the training dataset
  wetlandClass <- in_params[[5]]       # Class name for wetlands
  nonwetlandClass <- in_params[[6]]    # Class name for non-wetlands
  modelName <- in_params[[7]]          # Name for the Random Forest model
  calcStats <- in_params[[8]]          # Whether or not to calculate ROC statistics for the built model
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

  # Make sure at least one input raster was given
  if (length(inputRasterFiles) == 0)
    logAndStop("Must provide at least one input raster\n")

  # Make sure all input raster files exist
  lapply(inputRasterFiles, function(inputRasterFile) {
    if (!file.exists(inputRasterFile))
      logAndStop(paste0("Could not find input raster: '", inputRasterFile, "'\n"))
  })

  # Make sure training points file exists
  if (!file.exists(trainingPointsFile))
    logAndStop(paste0("Could not find training points dataset: '", trainingPointsFile, "'\n"))

  # Load input variables -------------------------------------------------------

  # Load input rasters
  rasterList <- lapply(inputRasterFiles, function(file) terra::rast(file))

  # Make sure factor rasters are factored correctly (chararacter level names
  # rather than numeric level names)
  for (i in seq_len(length(rasterList))) {
    raster <- rasterList[[i]]
    if (terra::is.factor(raster)) {
      rasterList[[i]] <- TerrainWorksUtils::fixFactorRaster(raster)
    }
  }

  # Record input variable metadata ---------------------------------------------

  # A place to store metadata
  inputVars <- list()

  # Record raster input variables
  for (raster in rasterList) {
    for (varName in names(raster)) {
      layer <- raster[[varName]]
      if (terra::is.factor(layer)) {
        inputVars[[varName]] <- list(
          levels = terra::levels(layer)[[1]],
          cats = terra::cats(layer)[[1]]
        )
      } else {
        inputVars[[varName]] <- NA
      }
    }
  }

  # Build training dataset -----------------------------------------------------

  # Load the training points
  trainingPoints <- terra::vect(trainingPointsFile)

  # Define the training dataset. This will store all the predictor variables as
  # well as the response class variable
  trainingDf <- data.frame(
    class = terra::values(trainingPoints)[[classFieldName]],
    stringsAsFactors = TRUE
  )

  # Add input raster variables
  for (raster in rasterList) {
    rasterValues <- TerrainWorksUtils::extractRasterValues(raster, trainingPoints)
    trainingDf <- cbind(trainingDf, rasterValues)
  }

  # Apply original levels to factor variables
  for (varName in names(trainingDf)) {
    if (varName != "class" && is.factor(trainingDf[[varName]])) {
      trainingDf[[varName]] <- factor(trainingDf[[varName]], levels = inputVars[[varName]]$levels)
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
    data = trainingDf,
    ntree = 200,
    importance = TRUE
  )

  # Log model information
  capture.output(rfModel, file = logFilename, append = TRUE)
  capture.output(randomForest::importance(rfModel), file = logFilename, append = TRUE)

  # Save model file ------------------------------------------------------------

  # Store the model and its input variables
  modelInfo <- list(
    model = rfModel,
    inputVars = inputVars
  )

  # Save the model information to a file
  modelFilename <- paste0(modelName, ".RFmodel")
  save(modelInfo, file = modelFilename)

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

    # Align input rasters
    alignedRasters <- TerrainWorksUtils::alignRasters(rasterList[[1]], rasterList)

    # Combine input rasters into a single multi-layered raster
    inputRaster <- alignedRasters[[1]]
    if (length(alignedRasters) > 1) {
      for (i in 2:length(alignedRasters)) {
        inputRaster <- c(inputRaster, alignedRasters[[i]])
      }
    }

    # Generate probability rasters for wetland and non-wetland
    probRaster <- terra::predict(
      inputRaster,
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

  # Test in Pack Forest region (BIGLAPTOP)
  tool_exec(
    in_params = list(
      workingDir = "C:/Work/netmapdata/pack_forest",
      inputRasterFiles = list("geo_unit.tif"),
      trainingDatasetFile = "trainingPoints.shp",
      classFieldName = "class",
      wetlandClass = "WET",
      nonwetlandClass = "UPL",
      modelName = "pf_geounit",
      calcStats = FALSE
    ),
    out_params = list(probRasterName = "prob")
  )

  # Test in Puyallup region (WORK2)
  tool_exec(
    in_params = list(
      workingDir = "E:/NetmapData/Puyallup",
      inputRasterFiles = list("grad_15.tif", "dev_300.tif"),
      trainingDatasetFile = "wetlandPnts.shp",
      classFieldName = "NEWCLASS",
      wetlandClass = "WET",
      nonwetlandClass = "UPL",
      modelName = "puy_grad15_dev300_geo",
      calcStats = TRUE
    ),
    out_params = list(probRasterName = "prob")
  )

  # Test in Pack Forest region (WORK2)
  tool_exec(
    in_params = list(
      workingDir = "C:/Work/Data/pack_forest",
      inputRasterFiles = list("grad_15.tif", "geounit.tif"),
      trainingDatasetFile = "pf_training.shp",
      classFieldName = "class",
      wetlandClass = "WET",
      nonwetlandClass = "UPL",
      modelName = "pf_grad15_geounit_litho",
      calcStats = TRUE
    ),
    out_params = list(probRasterName = NULL)
  )

  # Test in Pack Forest region with wetland points only in OEvba geounits (BIGLAPTOP)
  tool_exec(
    in_params = list(
      workingDir = "C:/Work/netmapdata/pack_forest",
      inputRasterFiles = list("geo_unit.tif"),
      trainingDatasetFile = "GeoUnitTestPoints.shp",
      classFieldName = "Class",
      wetlandClass = "WET",
      nonwetlandClass = "UPL",
      modelName = "pf_geounit",
      calcStats = FALSE
    ),
    out_params = list(probRasterName = "prob")
  )

}
