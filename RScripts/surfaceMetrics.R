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
                "executable_path", 
                "length_scale", 
                "metrics", 
                "sample",
                "output_suffix"))) {
  stop("Invalid config")
}
if (!all(names(config$metrics) %in% c("grad", 
                                      "plan", 
                                      "prof", 
                                      "dev"))) {
  stop("Invalid surface metrics specified")
}

if (!dir.exists(config$executable_path)) {
  stop("could not find ", config$executable_path)
}
executable_path <- normalizePath(config$executable_path)

# TODO: Check for executable files in executable_path
# TODO: Convert DEM file type if needed
# TODO: Convert length
# TODO: Do smart things with downsampling

for (metric in names(config$metrics)) {
  if (!isTRUE(config$metrics[[metric]])) {
    config$metrics[[metric]] <- NULL
  }
}
 
makeGrids_inputFile_path <- 
  normalizePath(file.path(config$scratch_folder, "input_makeGrids.txt"))

# --- makeGrids ---

write_input_file_MakeGrids(
  DEM_path = config$DEM_path, 
  length_scale = config$length_scale, 
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

# --- localRelief (DEV) ---

localRelief_inputFile_path <- 
  normalizePath(file.path(config$scratch_folder, "input_localRelief.txt"))

write_input_file_localRelief(
  DEM_path = config$DEM_path, 
  length_scale = config$length_scale, 
  scratch_folder = config$scratch_folder, 
  resample = config$sample$downSample, 
  interval = config$sample$sampleInterval, 
  filename = localRelief_inputFile_path, 
  output_file_extension = config$output_suffix
)

localRelief <- paste0(executable_path, "\\LocalRelief.exe")
command <- paste(localRelief, localRelief_inputFile_path, sep = " ")
setwd(config$scratch_folder)
output <- system(command, 
                 wait = TRUE)
setwd(wd)

# TODO: Reformat .flt files as .tif
# TODO: Add topographic wetness index (BuildGrids.exe)
# TODO: Add partial contributing area (Partial.exe)
