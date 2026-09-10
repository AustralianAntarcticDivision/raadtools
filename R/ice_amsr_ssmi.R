#' Read AMSRE sea ice data (6km) 
#' 
#' Using the TIF files (these are 2012 onwards)
#' 
#'  This function relies on the file-listing of [raadfiles::amsre_daily_files()]. 
#'  Concentration is returned as a fraction (0-1).
#' @seealso raadfiles::amsre_daily_files read_amsr2_ice
#' @inheritParams readice
#' @return \code{SpatRaster}
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

  out <- terra::rast(purrr::map(files$fullname, .readAMSRE, ext = xylim))
  terra::crs(out) <- .antarctic_crs()
  .utctime(out) <- files$date
  names(out) <- format(files$date, "%Y-%m-%d")
  out
}

#' Read AMSR2 sea ice data (6km) 
#' 
#' Using the TIF files (these are 2012 onwards)
#' 
#' This function relies on the file-listing of [raadfiles::amsr2_daily_files()]. 
#' Concentration is returned as a fraction (0-1).
#' @seealso raadfiles::amsr2_daily_files 
#' @inheritParams readice
#' @return \code{SpatRaster}
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

  out <- terra::rast(purrr::map(files$fullname, .readAMSR2, ext = xylim, setNA = setNA))
  terra::crs(out) <- .antarctic_crs()
  .utctime(out) <- files$date
  names(out) <- format(files$date, "%Y-%m-%d")
  out
}


# 
 .readAMSRE <- function(fname, ext = NULL) {
   opt <- options(warn = -1)
   on.exit(options(opt), add = TRUE)
   x <- terra::flip(.rast_nc(fname), direction = "vertical")
   terra::ext(x) <- terra::ext(.antarctic_extent())

  ## concentration is a fraction: the asi.nl.s6250 files already store one,
  ## the rest are percentages
   if (!grepl("asi.nl.s6250", basename(fname))) x <- x / 100
   if (!is.null(ext)) {
     x <- terra::crop(x, terra::ext(ext))
   }
    x
 }
 
 .readAMSR2 <- function(fname, ext = NULL, setNA = TRUE) {
   opt <- options(warn = -1)
   on.exit(options(opt), add = TRUE)
    ## percentages on disk, a fraction on the way out
    x <- .rast_nc(fname) / 100

   if (!is.null(ext)) {
     x <- terra::crop(x, terra::ext(ext))
   }
  if (setNA) {
    x <- terra::classify(x, matrix(c(1.0001, Inf, NA), ncol = 3, byrow = TRUE),
                         right = FALSE)
  }
   x
 }