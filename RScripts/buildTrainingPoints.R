#' Build Training Points for training wetland model
#' 
#' This function will create a shapefile with wetland and non-wetland 
#' points within a region. 
#' 
#' @param regionPolygonFile path to file containing a polygon outlining 
#' the analysis region
#' @param wetlandPolygonsFile path to file containing wetland polygons. 
#' Wetland shapefiles of many states and watersheds are available to 
#' download from the \href{National Wetlands Inventory website}{https://www.fws.gov/wetlands/Data/Data-Download.html}. 
#' @param sampleRate number of points to sample per km^2
#' @param regionMargin Width of the interior margin of the region where training
#' points cannot be generated. 
#' @param wetlandTypes Vector of wetland types to include. If you are using 
#' a dataset from the National Wetlands Inventory, the options are: 
#' \itemize{
#'   \item{Freshwater Forested/Shrub Wetland}
#'   \item{Freshwater Emergent Wetland}
#'   \item{Freshwater Pond}
#'   \item{Estuarine and Marine Wetland}
#'   \item{Riverine}
#'   \item{Lake}
#'   \item{Estuarine and Marine Deepwater}
#'   \item{Other}
#' }
#' 
#' @examples
#' if (file.exists("E:/NetmapData/Mashel/StudyAreaExtent_Mashel.shp")) {
#'   regionPolygonFile <- "E:/NetmapData/Mashel/StudyAreaExtent_Mashel.shp"
#'   wetlandPolygonsFile <- "E:/NetmapData/Mashel/mas_wetlandPolys.shp"
#'   wetlandTypes <- "Freshwater Pond"
#'   buildTrainingPoints(
#'     regionPolygonFile, 
#'     wetlandPolygonsFile, 
#'     sampleRate = 5, 
#'     regionMargin = 0, 
#'     wetlandTypes = wetlandTypes, 
#'     outputFilename = "E:/NetmapData/Mashel/twu_wip_rtraining.shp"
#'   )
#'   
#'   points <- terra::vect("E:/NetmapData/Mashel/twu_wip_rtraining.shp")
#'   region <- terra::vect(regionPolygonFile)
#'   terra::plot(region)
#'   points(points, col = ifelse(points$class == "UPL", "brown", "blue"))
#' }
#' 
#' 

buildTrainingPoints <- function(
  regionPolygonFile, 
  wetlandPolygonsFile, 
  sampleRate = 1,
  regionMargin = 50, 
  wetlandTypes = c("Freshwater Forested/Shrub Wetland", 
                   "Freshwater Emergent Wetland", 
                   "Freshwater Pond",
                   "Estuarine and Marine Wetland",
                   "Other"), 
  wetlandTypeClass = "WETLAND_TY",
  outputFilename = "wip_training.shp"
) {
  
  analysisRegion <- terra::vect(regionPolygonFile)
  
  wetlandPolygons <- terra::vect(wetlandPolygonsFile)
  if (!is.na(wetlandTypes)) {
    wetlandPolygons <- wetlandPolygons[wetlandPolygons[[wetlandTypeClass]][,1] %in% wetlandTypes]
  }

  if (length(wetlandPolygons) == 0)
    stop("No wetlands to sample")
  
  trainingPoints <- TerrainWorksUtils::createTrainingPointsFromPolygons(
    wetlandPolygons, 
    analysisRegion, 
    sampleRate = wetlandSampleRate, 
    regionMargin = regionMargin, 
    polygonClass = "WET", 
    nonpolygonClass = "UPL"
  )  
  
  terra::writeVector(trainingPoints, outputFilename, overwrite = TRUE)
}
