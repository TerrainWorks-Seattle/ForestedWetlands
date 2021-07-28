tool_exec <- function(in_params, out_params) {
  
  # ----- Install required packages ------------------------------------------
  
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
  inputRasterFiles <- in_params[[2]] # List of input raster filenames
  inputPointsFile <- in_params[[3]]  # Filename of point feature classified by wetland type
  fieldName <- in_params[[4]]        # Classification filed name within the point feature class
  isWetLabel <- in_params[[5]]       # Classification label for is-a-wetland
  notWetLabel <- in_params[[6]]      # Classification label for not-a-wetland, thes could be expanded to allow multiple names
  modelName <- in_params[[7]]        # Name for the generated RF model
  probRasterName <- out_params[[1]]
  
  # Validate parameters ------------------------------------------------------
  
  # TODO: Make sure no passed in args are illegal
  
  # Load data ----------------------------------------------------------------
  
  ## Load classification points ----------------------------------------------
  
  setwd(workingDir)
  print(paste0("Current working directory: ", workingDir))
  
  # Load the points training dataset
  allPoints <- terra::vect(inputPointsFile)
  
  # Keep only the column with the input field that holds the wetland Class
  classPoints <- allPoints[, fieldName]
  
  # Rename the column heading to Class
  names(classPoints)[1] <- "class"
  
  ## Load input rasters ------------------------------------------------------
  
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
  
  # Save input raster names
  rasterNames <- names(rasterStack)
  save(rasterNames, file = paste0(modelName, "_rasters.RData"))
  writeLines(rasterNames, paste0(modelName, "_rasters.txt"))
  
  # Extract point values -----------------------------------------------------
  
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
  
  # Build Random Forest model ------------------------------------------------
  
  # Build model to predict "class" from all input variables
  rfModel <- randomForest::randomForest(
    class ~ .,
    data = pointValues,
    ntree = 200,
    importance = TRUE
  )
  
  # Save the model
  save(rfModel, file = paste0(modelName, "_model.RData"))
  
  # Plot model statistics ----------------------------------------------------
  
  # Model error rates
  dev.new()
  plot(rfModel, main = paste0(modelName, "_error"))
  dev.copy(jpeg, paste0(modelName, "_error.jpg"))
  dev.off()
  
  # Model variable importance
  dev.new()
  randomForest::varImpPlot(rfModel, sort = TRUE, main = paste0(modelName, "_importance"))
  dev.copy(jpeg, paste0(modelName, "_importance.jpg"))
  dev.off()
  
  # Return -------------------------------------------------------------------
  
  return(out_params)
  
}

# Tests
if (FALSE) {
  
  # Test in Puyallup region (laptop)
  tool_exec(
    in_params = list(
      workingDir = "C:/Work/netmapdata/Puyallup",
      inputRasterFiles = list("grad_300.flt", "dev_300.flt", "plan_300.flt", "prof_300.flt"),
      inputPointsFile = "wetlandPnts.shp",
      fieldName = "NEWCLASS",
      isWetLabel = "WET",
      notWetLabel = "UPL",
      modelName = "puy"
    ),
    out_params = list(probRasterName="")
  )
  
  # Test in Puyallup region (WORK2 desktop)
  tool_exec(
    in_params = list(
      workingDir = "E:/NetmapData/Puyallup",
      inputRasterFiles = list("grad_300.flt", "dev_300.flt", "plan_300.flt", "prof_300.flt"),
      inputPointsFile = "wetlandPnts.shp",
      fieldName = "NEWCLASS",
      isWetLabel = "WET",
      notWetLabel = "UPL",
      modelName = "puy"
    ),
    out_params = list(probRasterName="")
  )
  
  # Test in Mashel region
  tool_exec(
    in_params = list(
      workingDir = "C:/Work/netmapdata/Mashel",
      inputRasterFiles = list("grad_50.flt", "dev_50.flt", "plan_50.flt", "prof_50.flt"),
      inputPointsFile = "PtAllPUY_version_02.shp",
      fieldName = "NEWCLASS",
      isWetLabel = "WET",
      notWetLabel = "UPL",
      modelName = "mas"
    ),
    out_params = list(probRasterName="")
  )
  
 }