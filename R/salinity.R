
##' Load file names and dates of SMAP sea surface salinity
##'
##' A data frame of file names and dates. 
##' 
##' Only a short period of daily data is available, from '2015-03-27' to 
##' '2018-10-21'. 
##' 
##' @title SMAP sea surface temperature files
##' @param time.resolution time resolution read
##' @param ... reserved for future use, currently ignored
##' @return data.frame of file names and dates
##' @export
salfiles <- function(time.resolution = c("daily"), ...) {
  # datadir <- getOption("default.datadir")
  time.resolution <- match.arg(time.resolution)
  if (time.resolution == "daily") {
    
    files <- raadfiles::smap_8day_files()
  } 
  files
}


##' Read SMAP sea surface salinity data
##'
##' SMAP surface salinity data read from files managed by
##' \code{\link{salfiles}}.  
##' @inheritParams raadtools
#' @inheritDotParams raadtools
##' @param varname variable to return from the data files, default is
##' "sss_smap", also available is "nobs", "sss_ref", "gland", "gice", "surtep"

##' @param readall FALSE by default
##' @export
##' @return SpatRaster, or a data.frame of files if \code{returnfiles = TRUE}
readsal <-  function (date, time.resolution = c("daily"),
                      xylim = NULL, lon180 = TRUE,
                      varname = c("sss_smap", "nobs", "sss_ref", "gland", "gice", "surtep"),
                      setNA = TRUE,
                      latest = TRUE,
                      returnfiles = FALSE,  ..., inputfiles = NULL) {
  time.resolution <- match.arg(time.resolution)
  varname <- match.arg(varname)
 
  if (is.null(inputfiles)) {
    files <- salfiles(time.resolution = time.resolution)
  } else {
    files <- inputfiles
  }
  if (returnfiles)
    return(files)
  .shim_notice("readsal")
  if (missing(date)) date <- if (latest) max(files$date) else min(files$date)
  
  date <- timedateFrom(date)
  files <- .processFiles(date, files, time.resolution)
  nfiles <- nrow(files)
  ## progress
  pb <- progress::progress_bar$new(
    format = "  extracting [:bar] :percent in :elapsed",
    total = nfiles, clear = FALSE, width= 60)
  pb$tick(0)

  ## TODO determine if we need to rotate, or just shift, or not anything
  rot <- lon180
  msk <- NULL
  ## unreachable: time.resolution is match.arg'd against "daily" alone. Kept
  ## and converted rather than deleted, in case monthly comes back.
  if (time.resolution == "monthly") {
    if (setNA) {
      msk <- crop_if_needed(rotate_if_needed(.rast_nc(file.path(dirname(files$fullname[1]), "lsmask.nc")), rot), xylim)
      msk[msk < 1] <- NA_real_
    }
  } 
  
  
  if (!"band" %in% names(files)) files$band <- 1
  
  
  r0 <- terra::rast(lapply(seq_len(nrow(files)), function(xi) {
    pb$tick()
    read_one(files$fullname[xi], ext = xylim, msk = msk, rot = rot,
             varname = varname, band = files$band[xi])
  }))
  if (!nzchar(terra::crs(r0))) terra::crs(r0) <- "EPSG:4326"
  
  if (nfiles == 1) r0 <- r0[[1L]]
  terra::time(r0) <- files$date
  names(r0) <- format(files$date, "%Y-%m-%d")
  
  .write_if_filename(r0, ...)
  
}






