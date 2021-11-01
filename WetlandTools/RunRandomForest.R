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

  workingDir <- in_params[[1]]          # Working directory where model files can be found and output files will be saved in
  modelFile <- in_params[[2]]           # Filename of the model
  referenceRasterFile <- in_params[[3]] # Raster to use as a grid reference
  inputRasterFiles <- in_params[[4]]    # List of input raster filenames
  inputPolygonFiles <- in_params[[5]]   # List of input polygon filenames
  testPointsFile <- in_params[[6]]      # Filename of point feature classified by wetland type
  classFieldName <- in_params[[7]]      # Name of the class field in the test dataset
  wetlandClass <- in_params[[8]]        # Class name for wetlands
  nonwetlandClass <- in_params[[9]]     # Class name for non-wetlands
  calcStats <- out_params[[1]]          # Whether or not to calculate ROC statistics for the built model
  probRasterName <- out_params[[2]]     # Filename of the generated wetland probability raster

  # Setup ----------------------------------------------------------------------

  setwd(workingDir)

  modelName <- baseFilename(modelFile)

  # Set up logging
  logFilename <- paste0(modelName, "_run.log")
  file.create(logFilename)

  cat(paste0("Current working directory: ", workingDir, "\n"), file = logFilename)

  # Validate parameters --------------------------------------------------------

  # Make sure reference raster file exists
  if (!file.exists(referenceRasterFile))
    logAndStop(paste0("Could not find reference raster: '", referenceRasterFile, "'"))

  # Make sure model file exists
  if (!file.exists(modelFile))
    logAndStop(paste0("Could not find model file: '", modelFile, "'\n"))

  # Make sure at least one input raster or polygon was given
  if (length(inputRasterFiles) == 0 && length(inputPolygonFiles) == 0)
    logAndStop("Must provide at least one input raster or polygon\n")

  # Make sure all input raster files exist
  lapply(inputRasterFiles, function(inputRasterFile) {
    if (!file.exists(inputRasterFile))
      logAndStop(paste0("Could not find input raster: '", inputRasterFile, "'\n"))
  })

  # Make sure test points file exists
  if (!file.exists(testPointsFile))
    logAndStop(paste0("Could not find test points dataset: '", testPointsFile, "'\n"))

  # Load model -----------------------------------------------------------------

  # Load the "modelInfo" object
  load(modelFile)
  rfModel <- modelInfo$model

  cat(paste0("Loaded model: ", modelFile, "\n"), file = logFilename, append = TRUE)

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

  # Load rasters
  rasterList <- lapply(inputRasterFiles, function(file) terra::rast(file))

  # Align rasters with the reference raster
  rasterList <- TerrainWorksUtils::alignRasters(referenceRaster, rasterList)

  # Add rasterized polygons to raster list
  rasterList <- c(polygonRasterList, rasterList)

  # Make sure factor rasters are factored correctly
  for (i in seq_along(rasterList)) {
    raster <- rasterList[[i]]
    if (terra::is.factor(raster)) {
      if (ncol(terra::cats(raster)[[1]]) > 2)
        raster <- TerrainWorksUtils::fixFactorRaster(raster)
      expectedCats <- modelInfo$inputVars[[names(raster)]]$cats
      rasterList[[i]] <- TerrainWorksUtils::applyCats(raster, expectedCats)
    }
  }

  # Build test dataset ---------------------------------------------------------

  # Load the test points
  testPoints <- terra::vect(testPointsFile)

  # Define the test dataset. This will store all the predictor variables as
  # well as the response variable (wetland/non-wetland class)
  testDf <- data.frame(
    class = terra::values(testPoints)[[classFieldName]],
    stringsAsFactors = TRUE
  )

  # Add predictor variables from input rasters
  for (raster in rasterList) {
    rasterValues <- TerrainWorksUtils::extractRasterValues(raster, testPoints)
    testDf <- cbind(testDf, rasterValues)
  }

  # Apply expected levels to factor variables
  for (varName in names(testDf)) {
    if (varName != "class" && is.factor(testDf[[varName]])) {
      testDf[[varName]] <- factor(testDf[[varName]], levels = modelInfo$inputVars[[varName]]$levels)
    }
  }

  # Make sure there are at least some points classified with the given
  # wetland/non-wetland class names
  correctlyLabeledPointIndices <- testDf$class == wetlandClass | testDf$class == nonwetlandClass
  if (sum(correctlyLabeledPointIndices) == 0)
    logAndStop(paste0("No points in the dataset with the given
                      wetland/non-wetland classes: '", wetlandClass, "'/'",
                      nonwetlandClass, "'."))

  # Remove points that aren't labeled wetland/non-wetland
  testDf <- testDf[correctlyLabeledPointIndices,]

  # Remove points with NA values
  testDf <- na.omit(testDf)

  cat("Ground-truth classifications:\n", file = logFilename, append = TRUE)
  capture.output(summary(testDf$class), file = logFilename, append = TRUE)

  # Predict classes with model -------------------------------------------------

  # Make sure the model has been given all its expected input variables
  expectedInputVars <- sort(names(modelInfo$inputVars))
  givenInputVars <- sort(colnames(testDf)[-1])

  if (!all(expectedInputVars == givenInputVars))
    logAndStop(paste0("Input variables (",
                      paste0(givenInputVars, collapse = ", "),
                      ") do not match those expected by the model (",
                      paste0(expectedInputVars, collapse = ", "), ")\n"))

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
    abline(a = 0, b = 1, lty = 2)
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

  # Test Pack Forest model in Mashel region (BIGLAPTOP)
  tool_exec(
    in_params = list(
      workingDir = "C:/Work/netmapdata/Mashel",
      modelFile = "C:/Work/netmapdata/pack_forest/pf_grad15_geounit_lithology.RFmodel",
      referenceRaster = "elev_mashel.flt",
      inputRasterFiles = list("grad_15.tif", "geounit_projected.tif"),
      inputPolygonFiles = list("lithology.shp"),
      testPointsFile = "wetlandPoints.shp",
      classFieldName = "NEWCLASS",
      wetlandClass = "WET",
      nonwetlandClass = "UPL"
    ),
    out_params = list(
      calcStats = FALSE,
      probRasterName = "prob"
    )
  )

  # Test Pack Forest model in Pack Forest region (BIGLAPTOP)
  tool_exec(
    in_params = list(
      workingDir = "C:/Work/Data/pack_forest",
      modelFile = "pf_dev300_geounit_grad15_lithology.RFmodel",
      referenceRaster = "pf_elev.tif",
      inputRasterFiles = list("dev_300.tif", "grad_15.tif", "plan_15.tif"),
      inputPolygonFiles = list("lithology.shp"),
      testPointsFile = "training_points.shp",
      classFieldName = "class",
      wetlandClass = "WET",
      nonwetlandClass = "UPL"
    ),
    out_params = list(
      calcStats = FALSE,
      probRasterName = "prob"
    )
  )

}
