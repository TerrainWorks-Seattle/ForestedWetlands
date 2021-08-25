tool_exec <- function(in_params, out_params) {
  
  # Set input/output parameters ------------------------------------------------
  
  workingDir <- in_params[[1]]           # Working directory where files will be read from and written to
  regionPolyFile <- in_params[[2]]       # Name of region polygon file
  wetlandPolysFile <- in_params[[3]]     # Name of wetland polygons file
  invalidWetlandTypes <- in_params[[4]]  # Names of invalid wetland types
  wetlandSampleRate <- in_params[[5]]    # Samples per km^2 of wetland
  nonwetlandSampleRate <- in_params[[6]] # Samples per km^2 of non-wetland
  trainingPointsFile <- out_params[[1]]  # Name of output training points file
  
  # Helper functions -----------------------------------------------------------
  
  samplePolys <- function(polys, sampleRate) {
    # Collect sample coordinates
    coords <- NULL
    for (i in seq_len(length(polys))) {
      poly <- polys[i]
      
      # Determine the number of samples to take from the polygon's area
      polyArea <- terra::expanse(poly, unit = "km")
      sampleSize <- ceiling(polyArea * sampleRate)
      
      # Sample the polygon
      samplePoints <- terra::spatSample(poly, size = sampleSize)
      sampleCoords <- terra::crds(samplePoints)
      
      coords <- rbind(coords, sampleCoords)
    }
    
    return(coords)
  }
  
  # Validate parameters --------------------------------------------------------
  
  # Sample wetlands ------------------------------------------------------------
  
  setwd(workingDir)
  
  # Load wetland polygons
  wetlandPolys <- terra::vect(wetlandPolysFile)
  
  # Remove invalid wetland polygons
  #wetlandPolys <- wetlandPolys[!(wetlandPolys$WETLAND_TY %in% invalidWetlandTypes)]
  
  # Crop the wetland polygons to the region
  regionPoly <- terra::vect(regionPolyFile) # TODO: Shrink the region poly a bit so no points are
  wetlandPolys <- terra::project(wetlandPolys, regionPoly)
  wetlandPolys <- terra::crop(wetlandPolys, regionPoly)
  
  # TODO: Avoid sampling close to the region's edges. Generated topo rasters 
  # sometimes have strange values at their extremities, so we shouldn't train a 
  # model with values extracted at those locations. This could be as easy as 
  # shrinking the region polygon, but only if the region is convex.
  
  # Sample wetland regions
  wetlandCoords <- samplePolys(wetlandPolys, wetlandSampleRate)
  
  # Sample non-wetlands --------------------------------------------------------
  
  # Determine non-wetland polygon(s) by subtracting wetland polygons from the 
  # whole region
  nonwetlandPolys <- terra::erase(regionPoly, wetlandPolys)
  
  # Sample non-wetland regions
  nonwetlandCoords <- samplePolys(nonwetlandPolys, nonwetlandSampleRate)
  
  # Combine sample points ------------------------------------------------------
  
  trainingCoords <- rbind(wetlandCoords, nonwetlandCoords)
  trainingAtts <- data.frame(class = factor(c(rep("WET", nrow(wetlandCoords)), rep("UPL", nrow(nonwetlandCoords)))))
  trainingPoints <- terra::vect(trainingCoords, atts = trainingAtts)
  
  #terra::plot(regionPoly)
  #terra::polys(wetlandPolys)
  #terra::points(trainingPoints, col = c(rep("blue", nrow(wetlandCoords)), rep("red", nrow(nonwetlandCoords))))
  
  # Create training points file ------------------------------------------------
  
  terra::writeVector(trainingPoints, trainingPointsFile, overwrite = TRUE)
  
  return(out_params)
  
}


if (FALSE) {
  
  # (BAKER)
  tool_exec(
    in_params = list(
      workingDir = "~/Work/Data/PackForest",
      regionPolyFile = "pf_region.shp",
      wetlandPolysFile = "wetlandPolys.shp",
      wetlandSampleRate = 1,
      nonwetlandSampleRate = 1
    ),
    out_params = NULL
  )
  
  # (DESKTOP2)
  tool_exec(
    in_params = list(
      workingDir = "C:/Work/Data/pack_forest",
      regionPolyFile = "PF_studyarea.shp",
      wetlandPolysFile = "pf_wetlandPolys.shp",
      invalidWetlandTypes = c(),
      wetlandSampleRate = 50,
      nonwetlandSampleRate = 10
    ),
    out_params = list(
      trainingPointsFile = "pf_training.shp"
    )
  )
  
}


