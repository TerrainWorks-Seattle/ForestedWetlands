# 1. parse inputs
wd <- getwd()
if ( grepl("ForestedWetlands$", wd) ) {
  rscriptDir <- normalizePath("RScripts")
} else if ( grepl("RScripts$", wd)) {
  rscriptDir <- normalizePath(".")
}

source(file.path(rscriptDir, "helpers.R"))
config <- yaml::read_yaml(file.path(rscriptDir, "surfaceMetrics.config"))

# Check inputs

if (!setequal(names(config), 
              c("DEM_path", 
                "scratch_folder",
                "overwrite",
                "executable_path", 
                "output_suffix",
                "length_scale", 
                "metrics", 
                "dev_options", 
                "pca_options"))) {
  stop("Invalid config")
}
if (!all(names(config$metrics) %in% c("grad", 
                                      "plan", 
                                      "prof",
                                      "bcon",
                                      "dev", 
                                      "twi", 
                                      "pca"))) {
  stop("Invalid surface metrics specified")
}

if (!dir.exists(config$executable_path)) {
  stop("could not find ", config$executable_path)
}
executable_path <- normalizePath(config$executable_path)

# TODO: Check for executable files in executable_path
# TODO: add "overwrite" parameter, and if FALSE, don't re-calculate 
# existing grids
# TODO: Convert DEM file type if needed

for (metric in names(config$metrics)) {
  if (!isTRUE(config$metrics[[metric]])) {
    config$metrics[[metric]] <- NULL
  }
}
 
makeGrids_inputFile_path <- 
  normalizePath(file.path(config$scratch_folder, "input_makeGrids.txt"))

# Load libraries
library(terra)

# Convert input length to raster's units

dem <- terra::rast(paste0(config$DEM_path))
DEM_units <- terra::linearUnits(dem)
adjusted_length <- config$length_scale/DEM_units

# --- makeGrids ---

write_input_file_MakeGrids(
  DEM_path = config$DEM_path, 
  length_scale = adjusted_length, 
  scratch_folder = config$scratch_folder, 
  grad = !is.null(config$metrics$grad), 
  plan = !is.null(config$metrics$plan), 
  prof = !is.null(config$metrics$prof), 
  bcon = !is.null(config$metrics$bcon), 
  filename = makeGrids_inputFile_path,
  output_file_extension = config$output_suffix
)

makeGrids <- paste0(executable_path, "\\MakeGrids.exe")
command <- paste(makeGrids, makeGrids_inputFile_path, sep = " ")
# Need wd to be scratch dir because that is where files are written
setwd(config$scratch_folder)
output <- system(command, 
                 wait = TRUE)
setwd(wd)
if (output != 0) {
  warning("Problem calculating partial contributing area: error ", output)
}

# --- localRelief (DEV) ---

if (!is.null(config$metrics$dev)) {
  localRelief_inputFile_path <- 
    normalizePath(file.path(config$scratch_folder, "input_localRelief.txt"))
  
  write_input_file_localRelief(
    DEM_path = config$DEM_path, 
    length_scale = adjusted_length, 
    scratch_folder = config$scratch_folder, 
    resample = config$dev_options$resample, 
    interval = config$dev_options$sampleInterval, 
    filename = localRelief_inputFile_path, 
    output_file_extension = config$output_suffix
  )
  
  localRelief <- paste0(executable_path, "\\LocalRelief.exe")
  command <- paste(localRelief, localRelief_inputFile_path, sep = " ")
  setwd(config$scratch_folder)
  output <- system(command, 
                   wait = TRUE)
  setwd(wd)
  if (output != 0) {
    warning("Problem calculating partial contributing area: error ", output)
  }
}

# --- topographic wetness index ---

if (!is.null(config$metrics$twi)) {
  # First make sure all relevant input files are present
  # 
  missing_metrics <- c()
  grad_path <- file.path(config$scratch_folder, 
                         paste0("grad", config$output_suffix, ".flt"))
  plan_path <- file.path(config$scratch_folder, 
                         paste0("plan", config$output_suffix, ".flt"))
  bcon_path <- file.path(config$scratch_folder, 
                         paste0("bcon", config$output_suffix, ".flt"))
  if (!file.exists(grad_path)) {
    missing_metrics <- c(missing_metrics, "grad")
  }
  if (!file.exists(plan_path)) {
    missing_metrics <- c(missing_metrics, "plan")
  }
  if (!file.exists(bcon_path)) {
    missing_metrics <- c(missing_metrics, "bcon")
  }
  
  if (length(missing_metrics) > 0) {
    # Run MakeGrids for all missing metrics
    twi_makeGrids_inputFile_path <- 
      normalizePath(file.path(config$scratch_folder, "twi_input_makeGrids.txt"))
    
    write_input_file_MakeGrids(
      DEM_path = config$DEM_path, 
      length_scale = adjusted_length, 
      scratch_folder = config$scratch_folder, 
      grad = "grad" %in% missing_metrics, 
      plan = "plan" %in% missing_metrics,  
      prof = FALSE,
      bcon = "bcon" %in% missing_metrics, 
      filename = twi_makeGrids_inputFile_path,
      output_file_extension = config$output_suffix
    )
    
    makeGrids <- paste0(executable_path, "\\MakeGrids.exe")
    command <- paste(makeGrids, twi_makeGrids_inputFile_path, sep = " ")
    # Need wd to be scratch dir because that is where files are written
    setwd(config$scratch_folder)
    output <- system(command, 
                     wait = TRUE)
    setwd(wd)
  }
  
  buildGrids_inputFile_path <- 
    normalizePath(file.path(config$scratch_folder, "input_buildGrids.txt"))
  
  write_input_file_buildGrids(
    DEM_path = config$DEM_path, 
    length_scale = adjusted_length, 
    scratch_folder = config$scratch_folder, 
    grad_path = grad_path, 
    plan_path = plan_path, 
    bcon_path = bcon_path, 
    filename = buildGrids_inputFile_path, 
    output_file_extension = config$output_suffix
  )
  
  
  buildGrids <- paste0(executable_path, "\\BuildGrids.exe")
  command <- paste(buildGrids, buildGrids_inputFile_path, sep = " ")
  setwd(config$scratch_folder)
  output <- system(command, 
                   wait = TRUE)
  setwd(wd)
  if (output != 0) {
    warning("Problem calculating partial contributing area: error ", output)
  }
}

# ----- Partial Contributing Area ----- #
# TODO: Need to add Partial.exe to ExecutableFiles.zip
if (FALSE) {
  partial_inputFile_path <- 
    normalizePath(file.path(config$scratch_folder, "input_partial.txt"))
  
  write_input_file_Partial(
    DEM_path = config$DEM_path, 
    length_scale = config$length_scale, 
    duration = config$pca_options$hours, 
    conductivity = config$pca_options$conductivity,
    scratch_folder = config$scratch_folder, 
    filename = partial_inputFile_path, 
    output_file_extension = config$output_suffix
  )
  
  Partial <- paste0(executable_path, "\\Partial.exe")
  command <- paste(Partial, partial_inputFile_path, sep = " ")
  setwd(config$scratch_folder)
  output <- system(command, 
                   wait = TRUE)
  setwd(wd)
  if (output != 0) {
    warning("Problem calculating partial contributing area: error ", output)
  }
}

# TODO: Reformat .flt files as .tif


