.antarctic_extent <- function() {
  c(-3950000, 3950000, -3950000, 4350000)
}
.antarctic_crs <- function() {
  "+proj=stere +lat_0=-90 +lat_ts=-70 +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs" 
}
#' Read AMSRE sea ice data (6km) 
#' 
#' Using the TIF files (these are 2012 onwards)
#' 
#'  This function relies on the file-listing of [raadfiles::amsre_daily_files()]. 
#' @seealso raadfiles::amsre_daily_files read_amsr2_ice
#' @inheritParams readice
#' @export
read_amsre_ice <- function(date, xylim = NULL, latest = TRUE, ..., returnfiles = FALSE, inputfiles = NULL) {
  files <- if (is.null(inputfiles)) raadfiles::amsre_daily_files() else inputfiles
  if (returnfiles) return(files)
  if (missing(date)) {
    date <- if (latest) max(files$date) else min(files$date)
  }
  files <- .processFiles(date, files, "daily")
  ext <- raster::extent(.antarctic_extent())
  
  out <- raster::brick(purrr::map(files$fullname, .readAMSRE))
  raster::projection(out) <- .antarctic_crs()
  
  raster::setExtent(raster::setZ(out, files$date), ext)
}

#' Read AMSR2 sea ice data (6km) 
#' 
#' Using the TIF files (these are 2012 onwards)
#' 
#' This function relies on the file-listing of [raadfiles::amsr2_daily_files()]. 
#' @seealso raadfiles::amsr2_daily_files 
#' @inheritParams readice
#' @export
read_amsr2_ice <- function(date, xylim = NULL, latest = TRUE, ..., setNA = TRUE, returnfiles = FALSE, inputfiles = NULL) {
  files <- if (is.null(inputfiles)) raadfiles::amsr2_daily_files() else inputfiles
  if (returnfiles) return(files)
  if (missing(date)) {
    date <- if (latest) max(files$date) else min(files$date)
  }
  files <- .processFiles(date, files, "daily")
  ext <- raster::extent(.antarctic_extent())
  
  out <- raster::brick(purrr::map(files$fullname, .readAMSR2, ext = xylim, setNA = setNA))
  raster::projection(out) <- .antarctic_crs()
  
  raster::setZ(out, files$date)
}

#' Read AMSR2 sea ice data (3km. 
#' 
#' Using the TIF files (these are 2012 onwards)
#' 
#' This function relies on the file-listing of [raadfiles::amsr2_3k_daily_files()]
#' @seealso raadfiles::amsr2_3k_daily_files  read_amsr2_ice
#' @inheritParams readice
#' @export
read_amsr2_3k_ice <- function(date, xylim = NULL, latest = TRUE, ...,  setNA = TRUE, returnfiles = FALSE, inputfiles = NULL) {
  files <- if (is.null(inputfiles)) raadfiles::amsr2_3k_daily_files() else inputfiles
  if (returnfiles) return(files)
  if (missing(date)) {
    date <- if (latest) max(files$date) else min(files$date)
  }
  files <- .processFiles(date, files, "daily")
  ext <- raster::extent(.antarctic_extent())
  
  out <- raster::brick(purrr::map(files$fullname, .readAMSR2, ext = xylim, setNA = setNA))
  raster::projection(out) <- .antarctic_crs()
  
  raster::setZ(out, files$date)
}

#' Read CERSAT daily sea ice data. 
#' 
#' This is southern hemisphere daily 12.5km SSM/I since 1991. 
#' 
#' This function relies on the file-listing of [raadfiles::cersat_daily_files()]
#' @seealso raadfiles::cersat_daily_files read_amsr_ice
#' @inheritParams readice
#' @export
read_cersat_ice <- function(date, xylim = NULL, latest = TRUE, setNA = TRUE, ..., returnfiles = FALSE, inputfiles = NULL) {
  files <- if (is.null(inputfiles)) raadfiles::cersat_daily_files() else inputfiles
  if (returnfiles) return(files)
  if (missing(date)) {
    date <- if (latest) max(files$date) else min(files$date)
  }
  files <- .processFiles(date, files, "daily")
  ext <- raster::extent(.antarctic_extent())
  op <- options(warn = -1); on.exit(options(op), add = TRUE)
  junk <- capture.output(
  out <- raster::brick(purrr::map(files$fullname, ~raster::flip(raster::raster(.x, varname = "concentration"), "y")))
  )
  raster::projection(out) <- .antarctic_crs()
  
  out <- raster::setExtent(raster::setZ(out, files$date), ext)
  if (setNA) out[out > 100] <- NA
  if (!is.null(xylim)) {
    out <- crop(out, xylim)
  }
  out
}


#' Read AMSR sea ice data. 
#' 
#' Both eras are available in the same series, scaling is applied to the first
#' series to ensure the data is always in percentage (0, 100). 
#' 
#' This function relies on the file-listing of [raadfiles::amsr_daily_files()]
#' @seealso raadfiles::amsr_daily_files read_cersat_ice readice
#' @inheritParams readice
#' @export
read_amsr_ice <- function(date, xylim = NULL, latest = TRUE, ..., returnfiles = FALSE, inputfiles = NULL) {
  files <- if (is.null(inputfiles)) raadfiles::amsr_daily_files() else inputfiles
  if (returnfiles) return(files)
  if (missing(date)) {
     date <- if (latest) max(files$date) else min(files$date)
  }
  files <- .processFiles(date, files, "daily")
 
  out <- raster::brick(purrr::map(files$fullname, .readAMSRE))
  raster::projection(out) <- .antarctic_crs()

  raster::setZ(out, files$date)
}
# 
 .readAMSRE <- function(fname, ext = NULL) {
 
   x <- flip(raster(fname), direction = "y")
   raster::extent(x) <- raster::extent(.antarctic_extent())
   
  ## earlier files were 0,1
   if (grepl("asi.nl.s6250", basename(fname))) x <- x * 100
   if (!is.null(ext)) {
     x <- raster::crop(x, ext)
   }
    x
 }
 
 .readAMSR2 <- function(fname, ext = NULL, setNA = TRUE) {
   
    x <- raster::raster(fname)
    
   if (!is.null(ext)) {
     x <- raster::crop(x, ext)
   }
  if (setNA) {
    x[x > 100] <- NA
  }
   x
 }