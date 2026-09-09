
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
  time.resolution <- match.arg(time.resolution)
  raadfiles::altimetry_daily_files()
}


