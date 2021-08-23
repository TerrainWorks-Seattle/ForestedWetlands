tool_exec <- function(in_params, out_params) {

  # Install packages -----------------------------------------------------------

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

  baseFilename <- function(file) {
    if (is.null(file)) return(NULL)
    return(gsub(basename(file), pattern="\\..*$", replacement=""))
  }
  
  logAndStop <- function(errMsg) {
    cat(errMsg, file = logFilename, append = TRUE)
    stop(errMsg)
  }

  # Set input/output parameters ------------------------------------------------

  # TODO: Try indexing in_params using the parameter names specified in the ArcGIS tool.
  # Ex: in_params[["Working_Directory"]]
  # Indexing by name returns NULL when missing instead of causing an error when indexing by number.

  workingDir <- in_params[[1]]       # Working directory where model files can be found and output files will be saved in
  modelFile <- in_params[[2]]        # Filename of the model
  inputRasterFiles <- in_params[[3]] # List of input raster filenames
  inputShapeFiles <- in_params[[4]]  # List of input shape filenames
  testPointsFile <- in_params[[5]]   # Filename of point feature classified by wetland type
  classFieldName <- in_params[[6]]   # Name of the class field in the test dataset
  wetlandClass <- in_params[[7]]     # Class name for wetlands
  nonwetlandClass <- in_params[[8]]  # Class name for non-wetlands
  calcStats <- in_params[[9]]
  probRasterName <- out_params[[1]]

  # Setup ----------------------------------------------------------------------

  setwd(workingDir)

  modelName <- baseFilename(modelFile)

  # Set up logging
  logFilename <- paste0(modelName, "_run.log")
  file.create(logFilename)

  cat(paste0("Current working directory: ", workingDir, "\n"), file = logFilename)

  # Validate parameters --------------------------------------------------------

  if (!file.exists(modelFile))
    logAndStop(paste0("Could not find model file: '", modelFile, "'\n"))

  if (length(inputRasterFiles) == 0 && length(inputShapeFiles) == 0)
    logAndStop("Must provide at least one input raster or shape\n")

  lapply(inputRasterFiles, function(inputRasterFile) {
    if (!file.exists(inputRasterFile))
      logAndStop(paste0("Could not find input raster: '", inputRasterFile, "'\n"))
  })
  
  lapply(inputShapeFiles, function(inputShapeFile) {
    if (!file.exists(inputShapeFile))
      logAndStop(paste0("Could not find input shape: '", inputShapeFile, "'\n"))
  })

  if (!file.exists(testPointsFile))
    logAndStop(paste0("Could not find test points dataset: '", testPointsFile, "'\n"))

  # Load model -----------------------------------------------------------------
  
  load(modelFile)
  cat(paste0("Loaded model: ", modelFile, "\n"), file = logFilename, append = TRUE)

  # Load input data ------------------------------------------------------------

  ## Load input rasters --------------------------------------------------------

  # Make sure the input rasters match those expected by the model
  # TODO: Match variable names instead of filenames
  rasterNamesFile <- sub(".RFmodel", ".RasterList", modelFile)
  if (!file.exists(rasterNamesFile))
    logAndStop(paste0("Could not find model raster list: '", rasterNamesFile, "'\n"))
  expectedRasterNames <- sort(readLines(rasterNamesFile))
  inputRasterNames <- sort(baseFilename(unlist(inputRasterFiles)))
  if (any(inputRasterNames != expectedRasterNames))
    logAndStop(paste0("Input raster names (",
                      paste0(inputRasterNames, collapse = ", "), 
                      ") do not match those expected by the model (", 
                      paste0(expectedRasterNames, collapse = ", "), ")\n"))

  # Load rasters
  if (length(inputRasterFiles) > 0) {
    if (length(inputRasterFiles) == 1) {
      rasterStack <- terra::rast(inputRasterFiles[[1]])
    } else {
      # Load each raster individually
      rasterList <- lapply(
        inputRasterFiles,
        function(rasterFile) terra::rast(rasterFile)
      )

      # Align all rasters with the first given
      rasterList <- TerrainWorksUtils::alignRasters(rasterList[[1]], rasterList)

      # Combine individual rasters into a stack
      rasterStack <- c(rasterList[[1]])
      for (i in 2:length(rasterList)) {
        rasterStack <- c(rasterStack, rasterList[[i]])
      }
    }
  }

  ## Load input shapes ---------------------------------------------------------

  # Make sure the input shapes match those expected by the model
  shapeNamesFile <- sub(".RFmodel", ".ShapeList", modelFile)
  if (!file.exists(shapeNamesFile))
    logAndStop(paste0("Could not find model shape list: '", shapeNamesFile, "'\n"))
  expectedShapeNames <- sort(readLines(shapeNamesFile))
  inputShapeNames <- sort(baseFilename(unlist(inputShapeFiles)))
  if (length(inputShapeNames) != length(expectedShapeNames) || any(inputShapeNames != expectedShapeNames))
    logAndStop(paste0("Input shape names (",
                      paste0(inputShapeNames, collapse = ", "), 
                      ") do not match those expected by the model (", 
                      paste0(expectedShapeNames, collapse = ", "), ")\n"))

  if (length(inputShapeFiles) > 0) {
    # Load each shape individually
    shapeList <- lapply(
      inputShapeFiles,
      function(shapeFile) terra::vect(shapeFile)
    )
  }

  # Build test dataset ---------------------------------------------------------

  # Load the expected factor levels for the model
  modelFactorLevelsFile <- sub(".RFmodel", ".Levels", modelFile)
  load(modelFactorLevelsFile)
  
  # Load the test points
  testPoints <- terra::vect(testPointsFile)

  # Define the test dataset, which will store both the predictor variables
  # and the response variable (class)
  testDf <- data.frame(
    class = terra::values(testPoints)[[classFieldName]]
  )

  # Extract raster value(s) at each point and add them to the dataset
  if (length(inputRasterFiles) == 1) {
    rasterVar <- terra::extract(rasterStack, testPoints, method = "simple")[,-1]
    testDf[[names(rasterStack)[1]]] <- rasterVar
  } else if (length(inputRasterFiles) > 1) {
    rasterVarsDf <- terra::extract(rasterStack, testPoints, method = "simple")[,-1]
    testDf <- cbind(testDf, rasterVarsDf)
  }

  # Extract shape value(s) at each point and add them to the test dataset
  if (length(inputShapeFiles) > 0) {
    for (shape in shapeList) {
      # Project the points into the same CRS as the shape
      projectedPoints <- terra::project(testPoints, shape)

      # Extract shape value(s) at each point
      shapeValues <- terra::extract(shape, projectedPoints)[,-1]
      
      # Add values to the test dataset (converting char variables to factor)
      if (ncol(shape) == 1) {
        # Add single variable
        if (is.character(shapeValues)) {
          shapeValues <- factor(shapeValues, levels = modelFactorLevels$shapes[[names(shape)]])
        }
        testDf[[names(shape)]] <- shapeValues 
      } else {
        # Add multiple variables
        for (varName in names(shapeValues)) {
          if (is.character(shapeValues[[varName]])) {
            shapeValues[[varName]] <- factor(shapeValues[[varName]], levels = modelFactorLevels$shapes[[varName]])
          }
          testDf[[varName]] <- shapeValues[[varName]]
        }   
      }
    }
  }

  # Make sure there are at least some points classified with the given
  # wetland/non-wetland class names
  correctlyLabeledPointIndices <- testDf$class == wetlandClass | testDf$class == nonwetlandClass
  if (sum(correctlyLabeledPointIndices) == 0) {
    errMsg <- paste0("No points in the dataset with the given
                     wetland/non-wetland classes: '", wetlandClass, "'/'",
                     nonwetlandClass, "'.")
    cat(errMsg, file = logFilename, append = TRUE)
    stop(errMsg)
  }
  
  # Remove points that aren't labeled wetland/non-wetland
  testDf <- testDf[correctlyLabeledPointIndices,]

  # Remove points with NA values
  testDf <- na.omit(testDf)
  
  # Convert class field to factor
  testDf$class <- factor(testDf$class)

  cat("Ground-truth classifications:\n", file = logFilename, append = TRUE)
  capture.output(summary(testDf$class), file = logFilename, append = TRUE)

  # Predict classes with model -------------------------------------------------

  # Run model on test dataset
  testDataPredictions <- predict(
    rfModel,
    type = "response",
    newdata = testDf
  )

  # Log confusion matrix for test data class predictions
  capture.output(table(testDataPredictions, testDf$class), file = logFilename, append = TRUE)

  # Calculate ROC statistics ---------------------------------------------------

  if (calcStats) {

    # Calculate wetland probability for each test point
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

  # Generate probability raster ------------------------------------------------

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

  # Return ---------------------------------------------------------------------

  return(out_params)

}

# Tests
if (FALSE) {

  # Test Puyallup model in Mashel region (BIGLAPTOP)
  tool_exec(
    in_params = list(
      workingDir = "C:/Work/netmapdata/Mashel",
      modelFile = "puy_grad15_dev300_geo.RFmodel",
      inputRasterFiles = list("grad_15.tif", "dev_300.tif"),
      inputShapeFiles = list("geo.shp"),
      testPointsFile = "wetlandPoints.shp",
      classFieldName = "NEWCLASS",
      wetlandClass = "WET",
      nonwetlandClass = "UPL",
      calcStats = TRUE
    ),
    out_params = list(probRasterName = NULL)
  )

  # Test Puyallup model in Mashel region (WORK2)
  tool_exec(
    in_params = list(
      workingDir = "E:/NetmapData/Mashel",
      modelFile = "hoh_grad15_geo.RFmodel",
      inputRasterFiles = list("grad_15.tif"),
      inputShapePoints = list("geo.shp"),
      testPointsFile = "mashelPoints.shp",
      classFieldName = "NEWCLASS",
      wetlandClass = "WET",
      nonwetlandClass = "UPL",
      calcStats = TRUE
    ),
    out_params = list(probRasterName = NULL)
  )

}
