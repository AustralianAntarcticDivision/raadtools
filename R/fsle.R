



#' Backward-in-time Finite-Size Lyapunov Exponents
#'
#' Daily files
#' @param date date or dates of data to read, see Details
#' @param time.resolution time resolution to read (only daily)
#' @param varname either `fsle_max` or `theta_max``
#' @param xylim spatial extents to crop from source data, can be anything accepted by \code{\link[raster]{extent}}, see Details
#' @param latest if TRUE and input date is missing return the latest time available, otherwise the earliest
#' @param returnfiles ignore options and just return the file names and dates
#' @param verbose print messages on progress etc.
#' @param ... passed to brick, primarily for \code{filename}
#' @param inputfiles input the files data base to speed up initialization
#' @export
#' @return data.frame
readfsle <- function (date, time.resolution = c("daily"),
                     xylim = NULL, 
                     latest = TRUE,
                     varname = c("fsle_max", "theta_max"),
                     returnfiles = FALSE, verbose = TRUE, ..., inputfiles = NULL) {
  time.resolution <- match.arg(time.resolution)
  varname <- match.arg(varname)
    out <- read_fsle_daily(date, xylim = xylim, latest = latest, returnfiles = returnfiles, varname = varname, ..., inputfiles = inputfiles) 
  
  out
}

read_fsle_daily <-  function(date, xylim = NULL, latest = TRUE, returnfiles = FALSE, varname, ..., inputfiles = NULL) {
   if (is.null(inputfiles)){
  files <- raadfiles::fsle_files()
} else {
  files <- inputfiles
}
if (returnfiles) return(files)
if (missing(date)) {
  if (latest) {
    date <- max(files$date)
  } else {
    date <- min(files$date)
  }  
}
date <- timedateFrom(date)
files <- .processFiles(date, files, "daily")
read0 <- function(x, varname) raster(x)

nfiles <- nrow(files)
## progress
pb <- progress::progress_bar$new(
  format = "  extracting [:bar] :percent in :elapsed",
  total = nfiles, clear = FALSE, width= 60)
pb$tick(0)
read_fun <- function(xfile, ext, msk, rot, varname = "", band = 1) {
  pb$tick()
  mask_if_needed(crop_if_needed(rotate_if_needed(raster(xfile, varname = varname, band = band), rot), ext), msk)
}

msk <- NULL
rot <- FALSE
files$band <- 1
op <- options(warn = -1)
r0 <- brick(stack(lapply(seq_len(nrow(files)), function(xi) 
  read_fun(files$fullname[xi], ext = xylim, msk = msk, rot = rot, varname = varname, band = files$band[xi]))), ...)
options(op)
r0 <- setZ(r0, files$date)
r0  
}
