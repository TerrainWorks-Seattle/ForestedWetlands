tool_exec<- function(in_params, out_params){
  
  #####################################################################################################  
  ### Check/Load Required Packages  
  #####################################################################################################   
  arc.progress_label("Loading packages...")
  
  if(!requireNamespace("raster", quietly = TRUE))
    install.packages("raster", quiet = TRUE)
  if(!requireNamespace("sp", quietly = TRUE))
    install.packages("sp", quiet = TRUE)
  if(!requireNamespace("rgdal", quietly = TRUE))
    install.packages("rgdal", quiet = TRUE)
  if(!requireNamespace("randomForest", quietly = TRUE))
    install.packages("randomforest", quiet = TRUE)
  
  require(raster)
  require(sp)
  require(rgdal)
  require(randomForest)
  
  # Extract point data from rasters without breaking memory limits
  extractInParts <- function(rasters, points) {
    if (canProcessInMemory(rasters)) {
      m <- raster::extract(rasters, points, method='bilinear')
      return(m)
    } else {
      # Initialize an empty result matrix
      m <- matrix(0, nrow=nrow(points), ncol=nlayers(rasters))
    }
    
    cat(paste("Stack Dimensions: ", nrow(rasters), "x", ncol(rasters), "x", nlayers(rasters), "\n"))
    cat(paste("Rows per pass: "))
    # Find maximum block size based on allocated memory
    bs <- 1
    placeholder <- raster(ncol=(ncol(rasters)*nlayers(rasters)), nrow=bs)
    while (canProcessInMemory(placeholder, 10)) {
      bs = bs*2
      placeholder = raster(ncol=(ncol(rasters)*nlayers(rasters)), nrow=bs)
    }
    bs = bs/2
    cat(paste(bs, "\n"))
    
    # Extract point values from each block
    i <- 1
    startTime <- Sys.time()
    while (i+bs < nrow(rasters)) {
      arc.progress_label(paste0("Extracting Data...", ceiling(100*(i/nrow(rasters))), "%"))
      r <- i+bs
      s <- suppressWarnings(raster::extract(crop(rasters, extent(rasters, i, r, 1, ncol(rasters))), points, method='bilinear'))
      s[is.na(s)]<-0
      m <- m + s
      i = i+bs
    }
    arc.progress_label(paste0("Extracting Data...", ceiling(100*(i/nrow(rasters))), "%"))
    rows <- nrow(rasters)
    suppressWarnings(s <- raster::extract(crop(rasters, extent(rasters, i, r, 1, ncol(rasters))), points, method='bilinear'))
    arc.progress_label(paste0("Extracting Data...", 100, "%"))
    s[is.na(s)] <- 0
    m <- m + s
    m [m == 0] <- NA
    return(m)
  }
  
  ##################################################################################################### 
  ### Define input/output parameters
  #####################################################################################################
  workingDir <- in_params[[1]]   # working directory where model files will be stored
  inputRasters <- in_params[[2]] # list of input rasters
  inputPoints <- in_params[[3]]  # point feature class of points classified by wetland type
  fieldName <- in_params[[4]]    # field within the feature class with the classification
  isWet <- in_params[[5]]        # class for is-a-wetland
  notWet <- in_params[[6]]       # class for not-a-wetland, thes could be expanded to allow multiple names
  modelName <- in_params[[7]]    # file name for model stored to disk in working directory. 
  #                                Model consists of two files, modelName.RFmodel and modelName.rasterList
  outputProbRaster <- out_params[[1]]

  setwd(workingDir)
  cat(paste0("Current working directory: ", workingDir,"\n"))
  
  ##################################################################################################### 
  ### Load data and create dataframe object to feed to randomForest function 
  #####################################################################################################
  arc.progress_label("Loading data...")
  
  # Load the set of input rasters to a rasterstack
  rasters <- stack(inputRasters)
  
  # Open the feature class with the training dataset points as a data frame
  allPoints <- arc.open(inputPoints)
  
  # Keep only the column with the input field that holds the wetland Class
  allPoints <- arc.select(object = allPoints, fields=fieldName)
  
  # Rename the column heading to Class
  names(allPoints)[1] <- "Class"  
  
  # Translate to a spatial dataset
  points <- arc.data2sp(allPoints)

  arc.progress_label("Extracting Data...")
  
  # Find the raster values at the point locations
  pointValues <- extractInParts(rasters, points)
  
  arc.progress_label("Processing Data...")
  
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
  training <- pointValues[!coords]
  
  # Save the name of the rasters used to build this model
  rasterNames <- names(training)[-1]
  filename <- paste0(modelName[1],".RasterList")
  save(rasterNames,file=filename)
  cat(paste0("Raster names saved in ",filename,"\n"))
  
  # Change to generic column headings; the same headings will be used for using this RF model on other basins
  for (i in 2:length(training)) {
    cat(paste0("Raster",i-1,": ",names(training)[i],"\n"))
    names(training)[i] <- paste0("Raster",i-1)
  }
  
  # Print summaries of the independent variable values for wetland and not-a-wetland points
  #className <- in_params[[4]][1]
  #print(paste0("Class ", className))
  #print(summary(training[training$Class == in_params[[4]][1],]))
  #className <- in_params[[5]][1]
  #print(paste0("Class ", className))
  #print(summary(training[training$Class == in_params[[5]][1],]))
  
  #####################################################################################################
  ### Build Random Forest Model
  #####################################################################################################
  # Run the randomForest. 1st iteration.
  arc.progress_label("Building random forest...")  
  rfclass <- randomForest(training[,-1], as.factor(training$Class), ntree=200, importance=TRUE)

  #####################################################################################################
  ### Write Output
  ##################################################################################################### 
  # Consider additional output to a .RFlog file that gives details of the model
  modelName <- paste0(modelName[1],".RFmodel")
  save(rfclass,file=modelName)
  print(rfclass)
  #print(summary(rfclass)) # not sure most users would find this very useful
  print(importance(rfclass))
  varImpPlot(rfclass,sort=TRUE)
  cat(paste0("..Model saved as ", modelName,"\n"))    
#  plot(rfclass) # I can't get two plots to both show up

# If an output probability raster is specified, create it 
  if (!is.null(outputProbRaster) && outputProbRaster != "NA") {
    arc.progress_label("Creating probability raster...")
    
  # Rename layer names in the rasterstack to match the column names in the data frame used to train the model 
    for (i in 1:nlayers(rasters)) {names(rasters)[i] <- paste0("Raster",i)}
    
    predict(rasters, rfclass, type = "prob", filename = outputProbRaster, format = "GTiff")
    print(paste0("Created GeoTiff probability raster ",outputProbRaster[1]))
  }
  
  return(out_params)
 }  
