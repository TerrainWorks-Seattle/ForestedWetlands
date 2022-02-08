# 1. parse inputs
wd <- getwd()
if ( grepl("ForestedWetlands$", wd) ) {
  configDir <- "RScripts"
} else if ( grepl("RScripts$", wd)) {
  configDir <- "."
}

config <- yaml::read_yaml(file.path(configDir, "surfaceMetrics.config"))

# Check inputs

if (!setequal(names(config), 
              c("DEM_path", 
                "scratch_folder", 
                "executable_path", 
                "length_scale", 
                "metrics", 
                "resample"))) {
  stop("Invalid config")
}
if (!all(names(config$metrics) %in% c("grad", 
                                      "plan", 
                                      "prof", 
                                      "dev"))) {
  stop("Invalid surface metrics specified")
}
 
if (!dir.exists(config$scratch_folder)) {
  stop("invalid scratch folder: ", config$scratch_folder)
}
if (!is.numeric(config$length_scale)) {
  stop("length_scale must be numeric")
}

# Create input file for executable
command_path <- file.path(
  config$executable_path, 
  "makegrids")

inputFilePath <- file.path(
  config$scratch_folder, 
  "input_makeGrids.txt"
)

write_input <- function(..., 
                        append = TRUE) {
  cat(..., "\n", 
      file = inputFilePath, 
      sep = "", 
      append = append)
}
write_input("# Input file for makeGrids\n", 
            "# Creating by surfaceMetrics.R\n",
            "# On ", as.character(Sys.time()), 
            append = FALSE)


write_input("DEM: ", normalizePath(config$DEM_path))
write_input("SCRATCH DIRECTORY: ", normalizePath(config$scratch_folder))
write_input("LENGTH SCALE: ", config$length_scale)

if ("grad" %in% names(config$metrics)) {
  if (config$metrics$grad) {
    write_input("GRID: GRADIENT, OUTPUT FILE = ", 
                paste0("grad_", config$length_scale))
  }
}

if ("plan" %in% names(config$metrics)) {
  if (config$metrics$plan) {
    write_input("GRID: PLAN CURVATURE, OUTPUT FILE = ", 
                paste0("plan_", config$length_scale))
  }
}

if ("prof" %in% names(config$metrics)) {
  if (config$metrics$prof) {
    write_input("GRID: PROFILE CURVATURE, OUTPUT FILE = ", 
                paste0("prof_", config$length_scale))
  }
}

if ("bcon" %in% names(config$metrics)) {
  if (config$metrics$bcon) {
    write_input("GRID: BCONTOUR, OUTPUT FILE = ", 
                paste0("bcon_", config$length_scale))
  }
}


