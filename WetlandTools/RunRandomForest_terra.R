tool_exec <- function(in_params, out_params) {
  
  # ----- Install packages ---------------------------------------------------
  
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
  
  # ----- Set input/output parameters ----------------------------------------
  
  # TODO: Try indexing in_params like in_params[["workingDir"]] so it returns
  # NULL if missing rather than stopping the program.
  
  workingDir <- in_params[[1]]       # Working directory where model files will be stored
  modelFile <- in_params[[2]]        # Filename of the model
  inputRasterFiles <- in_params[[3]] # List of input raster filenames
  isWetLabel <- in_params[[4]]       # Class for is-a-wetland
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
  rastersFile <- sub("model", "rasters", modelFile)
  load(rastersFile)
  if (length(inputRasterFiles) != length(rasterNames))
    stop(paste0("Must use the same number of rasters as used to build the model (", length(rasterNames), ")"))
  
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
  
  # Generate wetland probability raster --------------------------------------
  
  rasterStack <- terra::crop(rasterStack, terra::ext(562420, 566350, 5206820, 5210140))
  
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
      isWetLabel = "WET"
    ),
    out_params = list(probRasterName = "puy_prob")
  )
  
}
