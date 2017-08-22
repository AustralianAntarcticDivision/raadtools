
##' Load file names and dates of AVISO SSH/SSHA data
##'
##' A data.frame of file names and dates
##' @title AVISO sea surface height / anomaly files
##' @param time.resolution set to daily only
##' @param ... reserved for future use, currently ignored
##' @seealso \code{\link{readssh}}
##' @return data.frame of file names and dates
##' @export
sshfiles <- function(time.resolution = c("daily"),  ...) {
  datadir <- getOption("default.datadir")
  time.resolution <- match.arg(time.resolution)
  
  raadfiles::altimetry_daily_files()
}




##' Sea surface height/anomaly
##'
##' Details
##' @title read SSH/A
##' @param date date or dates of data to read, see Details
##' @param time.resolution time resolution to read
##' @param xylim spatial extents to crop from source data, can be anything accepted by \code{\link[raster]{extent}}, see Details
##' @param lon180 defaults to TRUE, to "rotate" Pacific view [0, 360] data to Atlantic view [-180, 180]
##' components, in degrees (0 north, 90 east, 180 south, 270 west)
##' @param ssha logical, to optionally return anomaly or height
##' @param latest if TRUE return the latest time available, ignoring the 'date' argument
##' @param returnfiles ignore options and just return the file names and dates
##' @param verbose print messages on progress etc.
##' @param ... passed to brick, primarily for \code{filename}
##' @param inputfiles input the files data base to speed up initialization
##' @export
##' @return data.frame
readssh <- function (date, time.resolution = c("daily"),
                     xylim = NULL, lon180 = TRUE, ssha = FALSE,
                     latest = TRUE,
                     returnfiles = FALSE, verbose = TRUE, ..., inputfiles = NULL) {
  time.resolution <- match.arg(time.resolution)
  
  if (ssha) {
    out <- read_sla_daily(date, xylim = xylim, latest = latest, returnfiles = returnfiles, ..., inputfiles = inputfiles) 
  } else {
    out <- read_adt_daily(date, xylim = xylim, latest = latest, returnfiles = returnfiles, ..., inputfiles = inputfiles) 
  }
  out
}

