
#' Write input file for MakeGrids.exe
write_input_file_MakeGrids <- function(
  DEM_path, 
  length_scale, 
  scratch_folder = getwd(), 
  grad = TRUE, 
  plan = TRUE, 
  prof = TRUE, 
  bcon = TRUE, 
  filename = file.path(scratch_folder, "input_makeGrids.txt"), 
  overwrite = TRUE, 
  output_file_extension = paste0("_", length_scale)
) {
  
  if (!dir.exists(scratch_folder)) {
    stop("invalid scratch folder: ", scratch_folder)
  }
  if (!is.numeric(length_scale)) {
    stop("length_scale must be numeric")
  }
  if ( file.exists(filename) ) {
    if (overwrite) {
      message("overwriting ", filename)
    } else {
      stop(filename, " exists. Set overwrite = TRUE to overwrite.")
    }
  } 
  
  # Normalize paths
  DEM_path <- normalizePath(DEM_path)
  scratch_folder <- normalizePath(scratch_folder)
  
  # Do not include ".flt" in DEM_path
  if (grepl("\\.flt$", DEM_path)) {
    DEM_path <- gsub("\\.flt$", "", DEM_path)
  }
  
  write_input <- function(..., 
                          append = TRUE) {
    cat(..., "\n", 
        file = filename, 
        sep = "", 
        append = append)
  }
  
  write_input("# Input file for makeGrids\n", 
              "# Creating by surfaceMetrics.R\n",
              "# On ", as.character(Sys.time()), 
              append = FALSE)
  
  
  write_input("DEM: ", DEM_path)
  write_input("SCRATCH DIRECTORY: ", scratch_folder)
  write_input("LENGTH SCALE: ", length_scale)
  
    if (grad) {
      write_input("GRID: GRADIENT, OUTPUT FILE = ", 
                  paste0("grad", output_file_extension))
    }
  
    if (plan) {
      write_input("GRID: PLAN CURVATURE, OUTPUT FILE = ", 
                  paste0("plan", output_file_extension))
    }
  
    if (prof) {
      write_input("GRID: PROFILE CURVATURE, OUTPUT FILE = ", 
                  paste0("prof", output_file_extension))
    }
  
    if (bcon) {
      write_input("GRID: BCONTOUR, OUTPUT FILE = ", 
                  paste0("bcon", output_file_extension))
    }
  
}
