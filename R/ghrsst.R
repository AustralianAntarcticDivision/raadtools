
# 
# changes in 2015?
# 
# 286
# 20151013-JPL-L4UHfnd-GLOB-v01-fv04-MUR.nc
# 317
# 20151113090000-JPL-L4_GHRSST-SSTfnd-MUR-GLOB-v02.0-fv04.1.nc


#' GHRSST L4 files
#'
#' Data frame of file names and dates. 
#' @return data.frame
#' @export
ghrsstfiles <- function() {
  raadfiles::ghrsst_daily_files()
}

#' Read GHRSST
#'
#' SST in Kelvin
#' @param date datetime
#' @param time.resolution daily
#' @param varname one of "analysed_sst", "analysis_error", "mask", "sea_ice_fraction"
#' @param setNA ignored
#' @param lon180 currently ignored
#' @param ... arguments passed to raster brick
#' @inheritParams raadtools
#' @return SpatRaster, or a data.frame of files if \code{returnfiles = TRUE}
#' @export
readghrsst  <- function (date, time.resolution = c("daily"),
                         xylim = NULL, lon180 = TRUE,
                         varname = c("analysed_sst", "analysis_error", "mask", "sea_ice_fraction"),
                         setNA = TRUE,
                         latest = TRUE,
                         returnfiles = FALSE, ..., inputfiles = NULL) {
  if (!lon180) stop("only lon180 is supported")
  time.resolution <- match.arg(time.resolution)
  varname <- match.arg(varname)

  files <- if (is.null(inputfiles))  ghrsstfiles() else inputfiles
  if (returnfiles)  return(files)
  .shim_notice("readghrsst")
  
  
  if (missing(date)) date <- if (latest) max(files$date) else min(files$date)
  date <- timedateFrom(date)
  files <- .processFiles(date, files, time.resolution)

  
xylim <- force(xylim)
  nfiles <- nrow(files)
#  if (is.null(xylim)) xylim <- extent(-180, 180, -90, 90)
  pb <- progress::progress_bar$new(
    format = "  extracting [:bar] :percent in :elapsed",
    total = nfiles, clear = FALSE, width= 60)
  pb$tick(0)
  r0 <- terra::rast(lapply(seq_len(nfiles), function(xi) {
    pb$tick()
    read_one(files$fullname[xi], ext = xylim, varname = varname)
  }))
  if (!nzchar(terra::crs(r0))) terra::crs(r0) <- "EPSG:4326"

  if (nfiles == 1) r0 <- r0[[1L]]
  terra::time(r0) <- files$date
  names(r0) <- format(files$date, "%Y-%m-%d")
  r0 <- .write_if_filename(r0, ...)
 
  #wtf
 # r0 <- setZ(r0, getZ(r0) + ISOdatetime(1981, 1, 1, 0, 0, 0, tz = "GMT")) ##1981-01-01 00:00:00)
  ## Kelvin
   r0 
  
}