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
  
  # Packages for foreach/dopar method
  if(!requireNamespace("parallel", quietly = TRUE))
    install.packages("parallel", quiet = TRUE)
  if(!requireNamespace("doParallel", quietly = TRUE))
    install.packages("doParallel", quiet = TRUE)
  if(!requireNamespace("foreach", quietly = TRUE))
    install.packages("foreach", quiet = TRUE)
  
  if(!requireNamespace("ROCR", quietly = TRUE))
    install.packages("ROCR", quiet = TRUE)
  
  require(raster)
  require(sp)
  require(rgdal)
  require(randomForest)
  require(parallel)
  require(doParallel)
  require(foreach)
  require(ROCR)
  
  
  ##################################################################################################### 
  ### Helper functions ################################################################################
  #####################################################################################################
  
  #### Extracts point data from rasters without breaking memory limits ####
  extractInParts <- function(rasters, points) {
    
    # Check if Raster* can fit entirely in memory
    if (canProcessInMemory(rasters)) {
      # Extract all point data at once
      beginCluster()
      result <- extract(rasters, points, method='bilinear')
      stopCluster()
      return(result)
    }
    
    library(doParallel)
    library(parallel)
    library(foreach)
    
    # Count the available cores on computer
    numCores <- detectCores()
    if (is.na(numCores)) {
      # Number unknown, execute loop sequentially
      registerDoSEQ()
      
    } else {
      # Create and register cores to be used in parallel
      cl <- makeCluster(numCores)
      registerDoParallel(cl)
      
      # Load necessary libraries to each core in the cluster
      clusterEvalQ(cl, {
        library(raster)
        library(arcgisbinding)
        library(randomForest)
      })
    }
    
    # Find the suggested block size for processing
    bs <- blockSize(rasters)
    
    # Extract point values from input rasters. Results -> the list of each iteration's resulting matrix
    result <- foreach (i = 1:bs$n, .combine='combineMatrices') %dopar% {
      # Only runs if cluster is sequential
      arc.progress_label(paste0("Extracting Data...", ceiling(100*(i/bs$n)), "%"))
      
      # Find the block's starting and ending rows
      bStart <- bs$row[i]
      bLen <- bs$nrows[i]
      bEnd <- bStart+bLen
      
      # Extract the point values from the block
      s <- suppressWarnings(extract(crop(rasters, extent(rasters, bStart, bEnd, 1, ncol(rasters))), points, method='bilinear'))
    }
    
    # Close the cluster connection
    if (!is.na(numCores))
      stopCluster(cl)
    
    arc.progress_label(paste0("Extracting Data...", 100, "%"))
    return(result)
  }
  
  #### Adds two matrices, ignoring NA values (treating them as 0s) ####
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
  
  #### Predicts probabilities and creates raster without breaking memory limits ####
  predictInParts <- function(rasters, model, fname) {
    
    # Check if Raster* can fit entirely in memory
    if (canProcessInMemory(rasters)) {
      # Generate entire probability raster at once
      p <- predict(rasters, model, type="prob", filename=fname, format="GTiff", overwrite=TRUE)
      return(p)
    } else {
      # Initialize the output file to write probabilities to in parts
      out <- raster(rasters)
      out <- writeStart(out, filename=fname, format="GTiff", overwrite=TRUE)
    }
    
    # Find the suggested block size for processing
    bs <- blockSize(rasters)
    
    for (i in 1:bs$n) {
      arc.progress_label(paste0("Creating probability raster...", ceiling(100*(i/bs$n)), "%"))
      
      # Calculate block row bounds
      bStart <- bs$row[i]
      bLen <- bs$nrows[i]
      bEnd <- bStart+bLen
      
      # Crop raster to block size
      c <- crop(rasters, extent(rasters, bStart, bEnd, 1, ncol(rasters)))
      
      # Apply the model to the cropped raster
      p <- predict(c, model, type="prob")

      # Write the block's values to the output raster
      v <- getValues(p)
      out <- writeValues(out, v, bStart)
    }
    
    # Stop writing and close the file
    out <- writeStop(out)
    
    arc.progress_label(paste0("Creating probability raster...", 100, "%"))
    return(out)
  }
  
  # Function to plot a graph and save to specified file
  plotandsave <- function(f, filename, baseline=FALSE) {
    dev.new()
    plot(f, main=filename)
    if (baseline) {abline(a=0,b=1)}
    dev.copy(win.metafile, paste0(filename, ".wmf"))
    dev.off()
  }
  
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
  calcStats <- in_params[[8]]      # Whether model performance statistics should be calculated
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
    pointValues <- extractInParts(rasters, points)
    
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
    arc.progress_label("Creating probability raster")
    cat(paste0("Writing probabilities to ", outputProbRaster))
    probs <- suppressWarnings(predictInParts(rasters, rfclass, outProbRaster))
    cat(paste0("Created GeoTiff probability raster ",outProbRaster[1]))
    
    if (calcStats) {
      arc.progress_label("Calculating performance statistics..")
      # Process test points, same steps as earlier
      pointValues <- extractInParts(probs, points)
      pointValues <- cbind(points[,1],pointValues)
      pointValues <- as.data.frame(pointValues)
      pointValues <- pointValues[pointValues$Class == isWet[1]|pointValues$Class == notWet[1],]
      pointValues <- na.omit(pointValues)
      coords <- names(pointValues) %in% c("coords.x1","coords.x2")
      predictions <- pointValues[!coords]
      
      names(predictions)[2] <- "Prob"
      pred <- prediction(predictions$Prob, predictions$Class, label.ordering=c(isWet[1],notWet[1]))
      roc <- performance(pred, measure="tpr", x.measure="fpr")
      auc <- performance(pred, measure="auc")
      cat(paste0("AUROC: ", auc@y.values, "\n"))
      plotandsave(roc, paste0(modelName[1],'_roc'), baseline=TRUE)
      
      prc <- performance(pred, measure="prec", x.measure="rec")
      idx <- which.max(slot(prc, "y.values")[[1]])
      prbe <- slot(prc, "y.values")[[1]][idx]
      cutoff <- slot(prc, "x.values")[[1]][idx]
      print(c(PRBE=prbe, cutoff=cutoff))
      plotandsave(prc, paste0(modelName[1],'_prc'))
      
      acc <- performance(pred, measure="acc")
      idx <- which.max(slot(acc, "y.values")[[1]])
      maxacc <- slot(acc, "y.values")[[1]][idx]
      cutoff <- slot(acc, "x.values")[[1]][idx]
      print(c(accuracy=maxacc, cutoff=cutoff))
      plotandsave(acc, paste0(modelName[1],'_acc'))
    }
  }
  
  return(out_params)
}