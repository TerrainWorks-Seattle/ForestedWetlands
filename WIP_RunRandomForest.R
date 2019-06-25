# WIP_RunRandomForest.R
# R script for RunRandomForest ArcGIS Pro tool.
# This script will load an existing random forest model
# and a set of rasters for running the model.
# These rasters must be of the same variables and in the same order as used to build the model.
# Options to build a probability raster and to compare model predictions to another point data set.
tool_exec<- function(in_params, out_params){
  
  #####################################################################################################  
  ### Check/Load Required Packages  
  #####################################################################################################   
  arc.progress_label("Loading packages...")
  
  if(!requireNamespace("raster", quietly = TRUE))
    install.packages("raster", quiet = TRUE)
  if(!requireNamespace("sp", quitly = TRUE))
    install.packages("sp", quite = TRUE)
  if(!requireNamespace("rgdal", quietly = TRUE))
    install.packages("rgdal", quiet = TRUE)
  if(!requireNamespace("randomForest", quietly = TRUE))
    install.packages("randomforest", quiet = TRUE)
  
  require(raster)
  require(sp)
  require(rgdal)
  require(randomForest)
  
  ##################################################################################################### 
  ### Define input/output parameters
  #####################################################################################################
  workingDir <- in_params[[1]][1]  # Working directory
  modelFile <- in_params[[2]][1]   # Random forest model name (modelFile.RFmodel, modelFile.rasterList)
  inputRasters <- in_params[[3]]   # List of input rasters, must match type and order of those used to build the model
  testData <- in_params[[4]]       # Optional input point feature class of data to run the model on
  fieldName <- in_params[[5]]      # If testData provided, specify the data field for point classification
  isWet <- in_params[[6]]          # Field value indicating is-a-wetland
  notWet <- in_params[[7]]         # Field value indicating not-a-wetland
  outProbRaster <- out_params[[1]] # Optional probability raster for the area covered by rasters in the raster list
  
  setwd(workingDir)
  cat(paste0("Current working directory: ", workingDir, "\n"))
  
  ##################################################################################################### 
  ### Load data and if testData specified, create dataframe object to feed to randomForest.predict function 
  #####################################################################################################
  arc.progress_label("Loading random forest model...")

  # Load the random forest model (file extension .RFmodel)
  load(modelFile)
  cat(paste0("Loaded model ", modelFile))
  print(rfclass)

  # Load the list of rasters used to build this model (file extension .rasterList)
  arc.progress_label("Loading rasters...")  
  rasterList <- sub(".RFmodel", ".rasterList", modelFile)
  load(rasterList)
  cat(paste0("\n"))
  cat(paste0("Rasters must be of the same elevation derivatives with the same length scales and in the same order as those used to build the model", "\n"))
  
  if (length(inputRasters) != length(rasterNames)) stop("You specified a different number of rasters than used to build the model")
  for (i in 1:length(rasterNames)) cat(paste0("Model: ",rasterNames[[i]],", Input: ",inputRasters[[i]], "\n"))
  cat(paste0("\n"))
  
  # Switch to the same generic names stored in the RFmodel file
  for (i in 1:length(inputRasters)) names(inputRasters)[i] <- paste0("Raster",i)
  rasters <- stack(inputRasters)
  
  ##################################################################################################### 
  ### If test data provided, evaluate model using new data
  #####################################################################################################
  # Open the feature class with the training dataset points as a data frame
  if (!is.null(testData) && testData != "NA") {
  arc.progress_label("Running model on test data...")
#-------------------------------------------------------------------------------
# all this could go into a sourced R script
    allPoints <- arc.open(testData)
  
    # Keep only the column with the input field that holds the wetland Class
    allPoints <- arc.select(object = allPoints, fields=fieldName)
  
    # Rename the column heading to Class
    names(allPoints)[1] <- "Class"  
  
    # Translate to a spatial dataset
    points <- arc.data2sp(allPoints)
  
    # Find the raster values at the point locations
    pointValues <- raster::extract(rasters, points, method='bilinear')
  
    # Append the class values as the first column
    pointValues <- cbind(points[,1],pointValues)
  
    # Convert to a data frame
    pointValues <- as.data.frame(pointValues)
  
    # Keep only records with one of the requested input field (class) values
    pointValues <- pointValues[pointValues$Class == isWet[1]|pointValues$Class == notWet[1],]
  
    # Eliminate rows with NA values
    pointValues <- na.omit(pointValues)
  
    # Eliminate columns with coordinate values
    coords <- names(pointValues) %in% c("coords.x1","coords.x2")
    newdata <- pointValues[!coords]
#----------------------------------------------------------------------------------
    # Change to generic column headings; the same headings will be used for using this RF model on other basins
    for (i in 2:length(newdata)) {
      names(newdata)[i] <- paste0("Raster",i-1)
    }
    print(head(newdata))
    
    # Run model on these data
    test <- predict(rfclass, type = "response", newdata = newdata[,-1])
    print(table(test, newdata$Class))
  }

  # Build a probability raster, if requested
  if (!is.null(outProbRaster) && outProbRaster != "NA") {
    arc.progress_label("Creating probability raster - this can take awhile...")    
    predict(rasters, rfclass, type = "prob", filename = outProbRaster, format = "GTiff")
    print(paste0("Created GeoTiff probability raster ",outProbRaster[1]))
  }

  return(out_params)
}