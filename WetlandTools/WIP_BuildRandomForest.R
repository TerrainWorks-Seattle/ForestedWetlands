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
  
  # Packages for foreach/dopar method
  if(!requireNamespace("parallel", quietly = TRUE))
    install.packages("parallel", quiet = TRUE)
  if(!requireNamespace("doParallel", quietly = TRUE))
    install.packages("doParallel", quiet = TRUE)
  if(!requireNamespace("foreach", quietly = TRUE))
    install.packages("foreach", quiet = TRUE)
  
  require(raster)
  require(sp)
  require(rgdal)
  require(randomForest)
  require(parallel)
  require(doParallel)
  require(foreach)
  
  # Add two matrices, ignoring NA values (treating them as 0)
  combineMatrices <- function(a, b) {
    combined <- ifelse(is.na(a),
                      ifelse(is.na(b),
                            NA,
                            b),
                      ifelse(is.na(b),
                            a,
                            a+b))
    return(combined)
  }
  
  # Extract point data from rasters without breaking memory limits
  extractInParts <- function(rasters, points) {
    library(doParallel)
    library(parallel)
    library(foreach)
    
    # Check if Raster* can fit entirely in memory
    if (canProcessInMemory(rasters)) {
      # Extract all point data at once
      result <- raster::extract(rasters, points, method='bilinear')
      return(result)
    }
    
    # Find the suggested block size for processing
    bs <- blockSize(rasters)
    
    # Count the available cores on computer
    numCores <- detectCores()
    if (is.na(numCores)) {
      # Number unknown, execute loop sequentially
      registerDoSEQ()

    } else {
      # Execute loop in parallel
      cl <- makeCluster(numCores)
      registerDoParallel(cl)

      # Load relevant libraries to each core in the cluster
      clusterEvalQ(cl, {
        library(raster)
        library(arcgisbinding)
      })
    }
    
    # Extract point values from input rasters. Results -> the list of each iteration's resulting matrix
    result <- foreach (i = 1:bs$n) %dopar% {
      arc.progress_label(paste0("Extracting Data...", ceiling(100*(i/bs$n)), "%"))

      # Find the block's starting and ending rows
      bStart <- bs$row[i]
      bLen <- bs$nrows[i]
      bEnd <- bStart+bLen

      # Extract the point values from the block
      s <- suppressWarnings(extract(crop(rasters, extent(rasters, bStart, bEnd, 1, ncol(rasters))), points, method='bilinear'))
    }
    
    # Reduce list into one matrix containing all results' non-NA values, calling combineMatrices()
    result <- do.call('Reduce', list('combineMatrices', result))
    
    # Close the cluster connection
    if (!is.na(numCores))
      stopCluster(cl)
    
    arc.progress_label(paste0("Extracting Data...", 100, "%"))
    return(result)
  }
    
  # Predict probabilities without breaking memory limits
  predictInParts <- function(rasters, model, fname) {
    
    # Check if Raster* can fit entirely in memory
    if (canProcessInMemory(rasters)) {
      # Generate entire probability raster
      p <- predict(rasters, model, type="prob", filename=fname, format="GTiff", overwrite=TRUE)
      return(p)
      
    } else {
      # Initialize the output file to write probabilities to in parts
      out <- raster(rasters)
      out <- writeStart(out, filename=fname, overwrite=TRUE)
    }
    
    # Find the suggested block size for processing
    bs <- blockSize(rasters)
    
    # Extract point values from each block
    for (i in 1:bs$n) {
      arc.progress_label(paste0("Creating probability raster...", ceiling(100*(i/bs$n)), "%"))
      
      # Find the block's starting and ending rows
      bStart <- bs$row[i]
      bLen <- bs$nrows[i]
      bEnd <- bStart+bLen
      
      # Crop the block out of the RasterBrick
      c <- crop(rasters, extent(rasters, bStart, bEnd, 1, ncol(rasters)))
      
      # Predict probabilities of the block's extent
      p <- predict(c, model, type="prob")
      
      # Write the values to the output file
      v <- values(p)
      out <- writeValues(out, v, bStart)
    }
    
    # Finish writing to the file
    out <- writeStop(out)
    
    # Convert file to GeoTiff format
    out <- writeRaster(out, filename=fname, format="GTiff")
    arc.progress_label(paste0("Creating probability raster...", 100, "%"))
    return(out)
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
  
  #Print summaries of the independent variable values for wetland and not-a-wetland points
  className <- in_params[[4]][1]
  print(paste0("Class ", className))
  print(summary(training[training$Class == in_params[[4]][1],]))
  className <- in_params[[5]][1]
  print(paste0("Class ", className))
  print(summary(training[training$Class == in_params[[5]][1],]))
  
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
    
    predictInParts(rasters, rfclass, outputProbRaster)
    print(paste0("Created GeoTiff probability raster ",outputProbRaster[1]))
  }
  
  return(out_params)
 }  
