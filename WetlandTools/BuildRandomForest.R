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

  workingDir <- in_params[[1]]          # Working directory where model files will be stored
  referenceRasterFile <- in_params[[2]] # Raster to use as a grid reference
  inputRasterFiles <- in_params[[3]]    # List of input raster filenames
  inputPolygonFiles <- in_params[[4]]   # List of input polygon filenames
  trainingPointsFile <- in_params[[5]]  # Filename of point feature classified by wetland type
  classFieldName <- in_params[[6]]      # Name of the class field in the training dataset
  wetlandClass <- in_params[[7]]        # Class name for wetlands
  nonwetlandClass <- in_params[[8]]     # Class name for non-wetlands
  modelName <- in_params[[9]]           # Name for the Random Forest model
  calcStats <- out_params[[1]]          # Whether or not to calculate ROC statistics for the built model
  probRasterName <- out_params[[2]]     # Filename of the generated wetland probability raster

  # Setup ----------------------------------------------------------------------

  if (!file.exists(workingDir))
    stop("Could not find working directory: '", workingDir, "'\n")

  setwd(workingDir)

  # Set up logging
  logFilename <- paste0(modelName, "_build.log")
  file.create(logFilename)

  cat(paste0("Current working directory: ", workingDir, "\n"), file = logFilename)

  # Validate parameters --------------------------------------------------------

  # Make sure reference raster file exists
  if (!file.exists(referenceRasterFile))
    logAndStop(paste0("Could not find reference raster: '", referenceRasterFile, "'"))

  # Make sure at least one input raster or polygon was given
  if (length(inputRasterFiles) == 0 && length(inputPolygonFiles) == 0)
    logAndStop("Must provide at least one input raster or polygon\n")

  # Make sure all input raster files exist
  lapply(inputRasterFiles, function(file) {
    if (!file.exists(file))
      logAndStop(paste0("Could not find input raster: '", file, "'\n"))
  })

  # Make sure all input polygon files exist
  lapply(inputPolygonFiles, function(file) {
    if (!file.exists(file))
      logAndStop(paste0("Could not find input polygon: '", file, "'\n"))
  })

  # Make sure training points file exists
  if (!file.exists(trainingPointsFile))
    logAndStop(paste0("Could not find training points dataset: '", trainingPointsFile, "'\n"))

  # Load reference raster ------------------------------------------------------

  referenceRaster <- terra::rast(referenceRasterFile)

  # Load input variables -------------------------------------------------------

  # NOTE: Polygon rasterization must occur BEFORE aligning rasters. Otherwise
  # a bug sometimes appears which assigns the reference raster name and
  # variable type to the rasterized polygon values.
  # Ex:
  # working directory:      C:/Work/netmapdata/Mashel
  # reference raster:       elev_mashel.flt
  # input rasters:          grad_15.tif, plan_15.tif
  # input polygon:          lithology.shp
  # output polygon raster:  non-factor raster with name "elev_mashel"

  # Load input polygons
  polygonList <- lapply(inputPolygonFiles, function(file) terra::vect(file))

  # Rasterize each polygon
  polygonRasterList <- list()
  for (polygon in polygonList) {
    polygon <- terra::project(polygon, referenceRaster)
    for (i in seq_along(names(polygon))) {
      varName <- names(polygon)[i]
      raster <- terra::rasterize(polygon, referenceRaster, field = varName)
      polygonRasterList[[i]] <- raster
    }
  }

  # Load input rasters
  rasterList <- lapply(inputRasterFiles, function(file) terra::rast(file))

  # Align rasters with the reference raster
  rasterList <- TerrainWorksUtils::alignRasters(referenceRaster, rasterList)

  # Make sure factor rasters are factored correctly (use character level names
  # rather than numeric level names)
  for (i in seq_along(rasterList)) {
    raster <- rasterList[[i]]
    if (terra::is.factor(raster)) {
      rasterList[[i]] <- TerrainWorksUtils::fixFactorRaster(raster)
    }
  }

  rasterList <- c(rasterList, polygonRasterList)

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

    # Combine individual input rasters into a single multi-layered raster
    inputRaster <- rasterList[[1]]
    if (length(rasterList) > 1) {
      for (i in 2:length(rasterList)) {
        inputRaster <- c(inputRaster, rasterList[[i]])
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
      referenceRasterFile = "pf_dem.tif",
      inputRasterFiles = list("grad_15.tif", "geounit.tif"),
      inputPolygonFiles = list("lithology.shp"),
      trainingDatasetFile = "trainingPoints.shp",
      classFieldName = "class",
      wetlandClass = "WET",
      nonwetlandClass = "UPL",
      modelName = "pf_grad15_geounit_lithology",
      
    ),
    out_params = list(
      calcStats = FALSE,
      probRasterName = "prob"
    )
  )

  # Test in Pack Forest region (WORK2)
  tool_exec(
    in_params = list(
      workingDir = "C:/Work/Data/pack_forest",
      referenceRasterFile = "pf_elev.tif",
      inputRasterFiles = list("grad_15.tif", "geounit.tif"),
      inputPolygonFiles = list("lithology.shp"),
      trainingDatasetFile = "training_points.shp",
      classFieldName = "class",
      wetlandClass = "WET",
      nonwetlandClass = "UPL",
      modelName = "pf_grad_geounit_lithology"
    ),
    out_params = list(
      calcStats = FALSE,
      probRasterName = "prob"
    )
  )
  
  # Test in Mashel region (WORK2)
  tool_exec(
    in_params = list(
      workingDir = "E:/NetmapData/Mashel",
      referenceRasterFile = "elev_mashel.flt",
      inputRasterFiles = list("grad_15.tif", "geounit.tif"),
      inputPolygonFiles = list("lithology.shp"),
      trainingDatasetFile = "training_points.shp",
      classFieldName = "class",
      wetlandClass = "WET",
      nonwetlandClass = "UPL",
      modelName = "pf_grad15_geounit_lithology"
    ),
    out_params = list(
      calcStats = FALSE,
      probRasterName = "prob"
    )
  )

}
