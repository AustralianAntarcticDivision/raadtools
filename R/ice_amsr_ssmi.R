#' Read AMSRE sea ice data (6km) 
#' 
#' Using the TIF files (these are 2012 onwards)
#' 
#'  This function relies on the file-listing of [raadfiles::amsre_daily_files()]. 
#' @seealso raadfiles::amsre_daily_files read_amsr2_ice
#' @inheritParams readice
#' @export
read_amsre_ice <- function(date, xylim = NULL, latest = TRUE, ..., returnfiles = FALSE, inputfiles = NULL) {
  opt <- options(warn = -1)
  on.exit(options(opt), add = TRUE)
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
  opt <- options(warn = -1)
  on.exit(options(opt), add = TRUE)
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


# 
 .readAMSRE <- function(fname, ext = NULL) {
   opt <- options(warn = -1)
   on.exit(options(opt), add = TRUE)
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
   opt <- options(warn = -1)
   on.exit(options(opt), add = TRUE)
    x <- raster::raster(fname)
    
   if (!is.null(ext)) {
     x <- raster::crop(x, ext)
   }
  if (setNA) {
    x[x > 100] <- NA
  }
   x
 }