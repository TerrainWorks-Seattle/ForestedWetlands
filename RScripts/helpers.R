
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

write_input_file_localRelief <- function(
  DEM_path, 
  length_scale, 
  scratch_folder = getwd(), 
  resample = 2, 
  interval = 2,
  filename = file.path(scratch_folder, "input_localRelief.txt"), 
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
  
  write_input("# Input file for LocalRelief\n", 
              "# Creating by surfaceMetrics.R\n",
              "# On ", as.character(Sys.time()), 
              append = FALSE)
  write_input("DEM: ", DEM_path)
  write_input("SCRATCH DIRECTORY: ", scratch_folder)
  
  radius <- length_scale / 2
  write_input("RADIUS: ", radius)
  write_input("DOWN SAMPLE: ", resample)
  write_input("SAMPLE INTERVAL: ", resample)
  write_input("OUTPUT DEV RASTER: ", paste0("dev", output_file_extension))
}

write_input_file_buildGrids <- function(
  DEM_path, 
  length_scale, 
  scratch_folder = getwd(),
  grad_path, 
  plan_path, 
  bcon_path, 
  slope_lo = "30.", 
  slope_hi = "60.", 
  plan_lo = "100000.15",
  plan_hi = "100000.3",
  filename = file.path(scratch_folder, "input_buildGrids.txt"), 
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
  grad_path <- normalizePath(grad_path)
  plan_path <- normalizePath(plan_path)
  bcon_path <- normalizePath(bcon_path)
  
  # Do not include ".flt" in DEM_path
  if (grepl("\\.flt$", DEM_path)) {
    DEM_path <- gsub("\\.flt$", "", DEM_path)
  }
  # remove elev_ from DEM_path
  DEM_id <- gsub("^\\w+_", "", basename(DEM_path))
  
  # Get DEM units
  dem <- terra::rast(paste0(DEM_path, ".flt"))
  DEM_units <- ifelse(terra::linearUnits(dem) == 1, "m", "f")
  
  write_input <- function(..., 
                          append = TRUE) {
    cat(..., "\n", 
        file = filename, 
        sep = "", 
        append = append)
  }
  
  write_input("# Input file for BuildGrids\n", 
              "# Creating by surfaceMetrics.R\n",
              "# On ", as.character(Sys.time()), 
              append = FALSE)
  
  write_input("DEM: ", DEM_path)
  write_input("DEMID: ", DEM_id)
  write_input("DEM UNITS: ", DEM_units)
  write_input("LENGTH SCALE: ", length_scale)
  write_input("SCRATCH: ", scratch_folder)
  write_input("AREA SLOPE THRESHOLD LOW GRADIENT: ", slope_lo)
  write_input("AREA SLOPE THRESHOLD HIGH GRADIENT: ", slope_hi)
  write_input("PLAN CURVATURE THRESHOLD LOW GRADIENT: ", plan_lo)
  write_input("PLAN CURVATURE THRESHOLD HIGH GRADIENT: ", plan_hi)
  write_input("GRADIENT FILE: ", grad_path)
  write_input("PLAN CURVATURE FILE: ", plan_path)
  write_input("BCON FILE: ", bcon_path)
}


# Convert GDAL BIL flt header file to ESRI FLT header
# https://gis.stackexchange.com/questions/140389/converting-dem-to-floating-point-raster-flt-using-qgis
convert_hdr <- function(input_hdr,
                        output_hdr = input_hdr,
                        overwrite = TRUE) {
  
  if (file.exists(output_hdr)) {
    if (overwrite) {
      message("overwriting ", output_hdr)
    } else {
      stop(output_hdr, " exists")
    }
  }
  
  lines <- readLines(input_hdr)
  extract_value <- function(pattern) {
    val <- lines[grepl(pattern, lines)]
    # Get all non-space characters preceeding end of string
    regmatches(val, regexpr("\\S+$", val))
  }
  
  nrow <- extract_value("NROWS")
  ncol <- extract_value("NCOLS")
  nodata_value <- extract_value("NODATA")
  
  # Specified only for byteorder = I (Intel)
  byteorder <- extract_value("BYTEORDER")
  if (byteorder != "I") {
    stop("Cannot convert with bytorder ", byteorder)
  }
  byteorder <- "LSBFIRST"
  
  # assuming Intel, only one cellsize is supported so get
  # rid of XDIM, YDIM and replace with CELLSIZE
  cellsize <- extract_value("XDIM")
  
  # BIL is upper left referenced but FLT is lower left referenced so
  # you need to reduce ulymap by NROWS * CELLSIZE to obtain YLLCORNER.
  xllcorner <- extract_value("ULXMAP")
  ulymap <- extract_value("ULYMAP")
  yllcorner <- as.numeric(ulymap) - (
    (as.numeric(nrow) * as.numeric(cellsize))
  )
  
  write_line <- function(key, value, append = TRUE) {
    # key and value with space between
    cat(sprintf("%-15s%s\n", key, value),
        file = output_hdr,
        append = append)
  }
  write_line("ncols", ncol, append = FALSE)
  write_line("nrows", nrow)
  write_line("xllcorner", xllcorner)
  write_line("yllcorner", yllcorner)
  write_line("cellsize", cellsize)
  write_line("NODATA_value", nodata_value)
  write_line("BYTEORDER", byteorder)
}



write_input_file_Partial <- function(
  DEM_path, 
  length_scale, 
  duration = 48, 
  conductivity = 2,
  scratch_folder = getwd(), 
  filename = file.path(scratch_folder, "input_partial.txt"), 
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
  
  write_input("# Input file for Partial\n", 
              "# Creating by surfaceMetrics.R\n",
              "# On ", as.character(Sys.time()), 
              append = FALSE)
  write_input("DEM: ", DEM_path)
  write_input("SCRATCH DIRECTORY: ", scratch_folder)
  write_input("LENGTH SCALE: ", length_scale)
  write_input("DURATION: ", duration)
  write_input("CONDUCTIVITY: ", conductivity)
  write_input("OUTPUT RASTER: ", paste0("pca_", duration, "hr", output_file_extension))
}

write_input_file_buildGrids <- function(
  DEM_path, 
  length_scale, 
  scratch_folder = getwd(),
  grad_path, 
  plan_path, 
  bcon_path, 
  slope_lo = "30.", 
  slope_hi = "60.", 
  plan_lo = "100000.15",
  plan_hi = "100000.3",
  filename = file.path(scratch_folder, "input_buildGrids.txt"), 
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
  grad_path <- normalizePath(grad_path)
  plan_path <- normalizePath(plan_path)
  bcon_path <- normalizePath(bcon_path)
  
  # Do not include ".flt" in DEM_path
  if (grepl("\\.flt$", DEM_path)) {
    DEM_path <- gsub("\\.flt$", "", DEM_path)
  }
  # remove elev_ from DEM_path
  DEM_id <- gsub("^\\w+_", "", basename(DEM_path))
  
  # Get DEM units
  dem <- terra::rast(paste0(DEM_path, ".flt"))
  DEM_units <- ifelse(terra::linearUnits(dem) == 1, "m", "f")
  
  write_input <- function(..., 
                          append = TRUE) {
    cat(..., "\n", 
        file = filename, 
        sep = "", 
        append = append)
  }
  
  write_input("# Input file for BuildGrids\n", 
              "# Creating by surfaceMetrics.R\n",
              "# On ", as.character(Sys.time()), 
              append = FALSE)
  
  write_input("DEM: ", DEM_path)
  write_input("DEMID: ", DEM_id)
  write_input("DEM UNITS: ", DEM_units)
  write_input("LENGTH SCALE: ", length_scale)
  write_input("SCRATCH: ", scratch_folder)
  write_input("AREA SLOPE THRESHOLD LOW GRADIENT: ", slope_lo)
  write_input("AREA SLOPE THRESHOLD HIGH GRADIENT: ", slope_hi)
  write_input("PLAN CURVATURE THRESHOLD LOW GRADIENT: ", plan_lo)
  write_input("PLAN CURVATURE THRESHOLD HIGH GRADIENT: ", plan_hi)
  write_input("GRADIENT FILE: ", grad_path)
  write_input("PLAN CURVATURE FILE: ", plan_path)
  write_input("BCON FILE: ", bcon_path)
}
