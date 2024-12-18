tool_exec <- function(in_params, out_params) {
  
  if (!requireNamespace("terra", quietly = TRUE))
    install.packages("terra", quiet = TRUE)
  
  # Set input/output parameters ------------------------------------------------
  
  workingDir <- in_params[[1]]           # Working directory where files will be read from and written to
  regionPolyFile <- in_params[[2]]       # Name of region polygon file
  wetlandPolysFile <- in_params[[3]]     # Name of wetland polygons file
  wetlandSampleRate <- in_params[[4]]    # Samples per km^2 of wetland
  nonwetlandSampleRate <- in_params[[5]] # Samples per km^2 of non-wetland
  regionMargin <- in_params[[6]]         # Width of region interior margin
  typeForestedAndShrubWetland <- in_params[[7]] 
  typeEmergentWetland <- in_params[[8]]
  typePond <- in_params[[9]] 
  typeEstuarineAndMarineWetland <- in_params[[10]] 
  typeRiverine <- in_params[[11]] 
  typeLake <- in_params[[12]] 
  typeEstuarineAndMarineDeepwater<- in_params[[13]] 
  typeOther <- in_params[[14]] 
  trainingPointsFile <- out_params[[1]]  # Name of output training points file
  
  # Helper functions -----------------------------------------------------------
  
  samplePolys <- function(polys, sampleRate) {
    # Collect sample coordinates
    coords <- NULL
    for (i in seq_len(length(polys))) {
      poly <- polys[i]
      
      # Determine the number of samples to take from the polygon's area
      polyArea <- terra::expanse(poly, unit = "km")
      if (polyArea == 0) next
      sampleSize <- ceiling(polyArea * sampleRate)
      
      # Sample the polygon
      samplePoints <- terra::spatSample(poly, size = sampleSize)
      sampleCoords <- terra::crds(samplePoints)
      
      coords <- rbind(coords, sampleCoords)
    }
    
    return(coords)
  }
  
  # Validate parameters --------------------------------------------------------
  
  if (!file.exists(workingDir))
    stop("Could not find working directory: '", workingDir, "'\n")
  
  setwd(workingDir)
  
  if (!file.exists(regionPolyFile))
    stop("Could not find region shape file: '", regionPolyFile, "'\n")
  
  if (!file.exists(wetlandPolysFile))
    stop("Could not find wetland shape file: '", wetlandPolysFile, "'\n")
  
  if (!(is.numeric(wetlandSampleRate) && length(wetlandSampleRate) == 1))
    stop("Wetland sample rate must be a single numeric value")
  
  if (!(is.numeric(nonwetlandSampleRate) && length(nonwetlandSampleRate) == 1))
    stop("Non-wetland sample rate must be a single numeric value")
  
  # Prepare the region ---------------------------------------------------------
  
  # Load region polygon
  regionPoly <- terra::vect(regionPolyFile)
  
  #terra::plot(regionPoly)
  
  # Shrink region by applying an interior margin. This ensures that training 
  # points will not be sampled near the region's edges
  if (regionMargin != 0)
    regionPoly <- terra::buffer(regionPoly, width = -abs(regionMargin))
  
  # Sample wetlands ------------------------------------------------------------
  
  # Load wetland polygons
  wetlandPolys <- terra::vect(wetlandPolysFile)
  
  # Filter out undesired wetland types
  wetlandTypes <- c()
  if (typeForestedAndShrubWetland)
    wetlandTypes <- c(wetlandTypes, "Freshwater Forested/Shrub Wetland")
  if (typeEmergentWetland)
    wetlandTypes <- c(wetlandTypes, "Freshwater Emergent Wetland")
  if (typePond)
    wetlandTypes <- c(wetlandTypes, "Freshwater Pond")
  if (typeEstuarineAndMarineWetland)
    wetlandTypes <- c(wetlandTypes, "Estuarine and Marine Wetland")
  if (typeRiverine) 
    wetlandTypes <- c(wetlandTypes, "Riverine")
  if (typeLake)
    wetlandTypes <- c(wetlandTypes, "Lake")
  if (typeEstuarineAndMarineDeepwater)
    wetlandTypes <- c(wetlandTypes, "Estuarine and Marine Deepwater")
  if (typeOther)
    wetlandTypes <- c(wetlandTypes, "Other")
  
  wetlandPolys <- wetlandPolys[wetlandPolys$WETLAND_TY %in% wetlandTypes]
  if (length(wetlandPolys) == 0)
    stop("No wetlands to sample")
  
  # Crop the wetland polygons to the region
  wetlandPolys <- terra::project(wetlandPolys, regionPoly)
  wetlandPolys <- terra::crop(wetlandPolys, regionPoly)
  
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
  trainingPoints <- terra::vect(trainingCoords, atts = trainingAtts, crs = terra::crs(regionPoly))
  
  #terra::polys(regionPoly, lty = 2)
  #terra::polys(wetlandPolys, border = "cyan")
  #terra::points(trainingPoints, col = c(rep("blue", nrow(wetlandCoords)), rep("red", nrow(nonwetlandCoords))))
  
  # Create training points file ------------------------------------------------
  
  if (!(endsWith(trainingPointsFile, ".shp")))
    trainingPointsFile <- paste0(trainingPointsFile, ".shp")
    
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
  
  # Pack Forest (DESKTOP2)
  tool_exec(
    in_params = list(
      workingDir = "C:/Work/Data/pack_forest",
      regionPolyFile = "PF_studyarea.shp",
      wetlandPolysFile = "pf_wetlandPolys.shp",
      wetlandSampleRate = 100,
      nonwetlandSampleRate = 20,
      regionMargin = 50
    ),
    out_params = list(
      trainingPointsFile = "pf_training.shp"
    )
  )
  
  # Mashel (DESKTOP2)
  tool_exec(
    in_params = list(
      workingDir = "E:/NetmapData/Mashel",
      regionPolyFile = "StudyAreaExtent_Mashel.shp",
      wetlandPolysFile = "mas_wetlandPolys.shp",
      wetlandSampleRate = 5,
      nonwetlandSampleRate = 1,
      regionMargin = 0,
      typeForestedAndShrubWetland = FALSE,
      typeEmergentWetland = FALSE,
      typePond = TRUE,
      typeEstuarineAndMarineWetland = FALSE,
      typeRiverine = FALSE,
      typeLake = FALSE,
      typeEstuarineAndMarineDeepwater = FALSE,
      typeOther = FALSE
    ),
    out_params = list(
      trainingPointsFile = "mas_trainingPoints.shp"
    )
  )
  
}


