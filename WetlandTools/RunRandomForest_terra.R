tool_exec <- function(in_params, out_params) {
  
  # ----- Install packages ---------------------------------------------------
  
  if (!requireNamespace("randomForest", quietly = TRUE))
    install.packages("randomForest", quiet = TRUE)
  if (!requireNamespace("terra", quietly = TRUE))
    install.packages("terra", quiet = TRUE)
  
  # ----- Define helper functions --------------------------------------------
  
  alignRasters <- function(refRaster, inputRasters) {
    
    # Align each input raster with the reference raster
    alignedRasters <- list()
    for (i in seq_along(inputRasters)) {
      inputRaster <- inputRasters[[i]]
      
      # Reproject the input raster if it doesn't align with the reference
      if (
        terra::ext(inputRaster) != terra::ext(refRaster) ||
        !all(dim(inputRaster) == dim(refRaster)) || 
        !all(terra::res(inputRaster) == terra::res(refRaster)) ||
        !all(terra::origin(inputRaster) == terra::origin(refRaster)) ||
        terra::crs(inputRaster) != terra::crs(refRaster)
      ) {
        inputRaster <- terra::project(inputRaster, refRaster)
      }
      
      # Store the result
      alignedRasters[[i]] <- inputRaster
    }
    
    return(alignedRasters)
    
  }
  
  baseFilename <- function(file) {
    return(gsub(basename(file), pattern="\\..*$", replacement=""))
  }
  
  # ----- Set input/output parameters ----------------------------------------
  
  # TODO: Try indexing in_params like in_params[["workingDir"]] so it returns
  # NULL if missing rather than stopping the program.
  
  workingDir <- in_params[[1]]       # Working directory where model files will be stored
  modelFile <- in_params[[2]]        # Filename of the model
  inputRasterFiles <- in_params[[3]] # List of input raster filenames
  testDataFile <- in_params[[4]]
  fieldName <- in_params[[5]]
  isWetLabel <- in_params[[6]]       # Class for is-a-wetland
  notWetLabel <- in_params[[7]]
  calcStats <- in_params[[8]]
  probRasterName <- out_params[[1]]
  
  # Validate parameters ------------------------------------------------------
  
  # TODO: Make sure no passed in args are illegal
  
  # Load Random Forest model -------------------------------------------------
  
  # Set the working directory
  setwd(workingDir)
  print(paste0("Current working directory: ", workingDir))
  
  # Load the model
  load(modelFile)
  
  # Load input rasters -------------------------------------------------------
  
  # Make sure the input rasters store the same variables as those used to 
  # build the model
  modelName <- sub("_model", "", baseFilename(modelFile))
  modelVarNames <- sort(readLines(paste0(modelName, "_rasters.txt")))
  inputVarNames <- sort(baseFilename(unlist(inputRasterFiles)))
  
  if (any(inputVarNames != modelVarNames))
    stop("Input raster files do not match the the model variables")
  
  # Load each raster individually
  rasterList <- lapply(
    inputRasterFiles,
    function(file) terra::rast(file)
  )
  
  # Align rasters (with the first input raster)
  rasterList <- alignRasters(rasterList[[1]], rasterList)
  
  # Combine individual rasters into a stack
  rasterStack <- c(rasterList[[1]])
  for (i in 2:length(rasterList)) {
    rasterStack <- c(rasterStack, rasterList[[i]])
  }
  
  # Predict test data --------------------------------------------------------
  
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
  
  # Remove points that aren't labeled either "wetland" or "non-wetland"
  pointValues <- pointValues[
    pointValues$class == isWetLabel | pointValues$class == notWetLabel,
  ]
  
  # Convert class values to factors since Random Forest can't use strings as 
  # predictor variables
  pointValues$class <- factor(pointValues$class)
  
  # Run model on these data
  library(randomForest)
  testDataPredictions <- predict(
    rfModel,
    type = "response",
    newdata = pointValues[,-which(names(pointValues) == "class")]
  )
  
  print(table(testDataPredictions, pointValues$class))
  
  # Generate wetland probability raster --------------------------------------
  
  if (!is.null(probRasterName) && !is.na(probRasterName)) {
    rasterStack <- terra::crop(rasterStack, terra::ext(555600, 562400, 5224000, 5230000))
    
    # Predict probability raster
    probRaster <- terra::predict(
      rasterStack,
      rfModel,
      na.rm = TRUE,
      type = "prob"
    )
    
    # Save probability raster
    terra::writeRaster(
      probRaster[[isWetLabel]],
      filename = paste0(probRasterName, ".tif"),
      overwrite = TRUE
    )
  }
  
  # Return -------------------------------------------------------------------
  
  return(out_params)
  
}

# Tests
if (FALSE) {
  
  # Test in Puyallup region
  tool_exec(
    in_params = list(
      workingDir = "C:/Work/netmapdata/Puyallup",
      modelFile = "puy_model.RData",
      inputRasterFiles = list("grad_300.flt", "dev_300.flt", "plan_300.flt", "prof_300.flt"),
      testDataFile <- "wetlandPnts.shp",
      fieldName <- "NEWCLASS",
      isWetLabel <- "WET",
      notWetLabel <- "UPL",
      calcStats <- FALSE
    ),
    out_params = list(probRasterName = "puy_prob")
  )
  
  # Test in Puyallup region (WORK2 desktop)
  tool_exec(
    in_params = list(
      workingDir = "E:/NetmapData/Puyallup",
      modelFile = "puy_model.RData",
      inputRasterFiles = list("grad_300.flt", "dev_300.flt", "plan_300.flt", "prof_300.flt"),
      testDataFile <- "wetlandPnts.shp",
      fieldName <- "NEWCLASS",
      isWetLabel <- "WET",
      notWetLabel <- "UPL",
      calcStats <- FALSE
    ),
    out_params = list(probRasterName = "puy_prob")
  )
  
}
