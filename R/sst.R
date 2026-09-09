
##' Load file names and dates of OISST sea surface temperature data
##'
##' A data frame of file names and datres
##' @title OISST sea surface temperature files
##' @param time.resolution time resolution read
##' @param ... reserved for future use, currently ignored
##' @return data.frame of file names and dates
##' @export
sstfiles <- function(time.resolution = c("daily","monthly"), ...) {
 # datadir <- getOption("default.datadir")
  time.resolution <- match.arg(time.resolution)
  if (time.resolution == "daily") {
    
    files <- raadfiles::oisst_daily_files()
  } else {
    files <- raadfiles::oisst_monthly_files()
    ## we have to do this for raadfiles now since 0.1.4
    r <- .rast_nc(files$fullname[1])
    
    files <- files[rep(1L, terra::nlyr(r)), ]
    files$band <- 1:nrow(files)
    ## terra carries the netCDF time dimension itself. raster's stack() only
    ## put it in the layer names, as "X2000.01.01", which is what the parse
    ## below was for; it stays as a fallback in case a file has no time.
    tm <- terra::time(r)
    files$date <- if (!all(is.na(tm))) {
      timedateFrom(tm)
    } else {
      as.POSIXct(strptime(names(r), "X%Y.%m.%d"), tz  = "UTC")
    }
    files <- files[c("date", "fullname", "band", "root")]
  }
  files
}


