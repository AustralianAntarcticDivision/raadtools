#' Antarctic altimetry files
#'
#' A one-row-per-date catalogue of the Antarctic altimetry time series, read
#' from the time dimension of the single source file found by
#' [raadfiles::altimetry_antarctica_files()]. Each row carries the `fullname`
#' of that file and the `band` within it for that date.
#'
#' @return data.frame of `date`, `fullname`, `band` and `root`
#' @seealso [read_altimetry_antarctica_daily()]
#' @export
altimetry_antarctica_daily_files <- function() {
    files <- raadfiles::altimetry_antarctica_files()
  nc <- RNetCDF::open.nc(files$fullname[1])    
  time <- RNetCDF::var.get.nc(nc, "time")
  times <- RNetCDF::utcal.nc(RNetCDF::att.get.nc(nc, "time", "units"), value = time)
  out <- tibble::tibble(date = ISOdatetime(times[,"year"], times[, "month"], times[, "day"], times[, "hour"], times[, "minute"], times[, "second"], tz = "UTC"))
  out$fullname <- files$fullname[1L]
  out$band <- seq_along(out$date)
  out$root <- files$root[1L]
  out
}
#' Read Antarctic altimetry
#'
#' Read one or more dates from the Antarctic altimetry time series catalogued
#' by [altimetry_antarctica_daily_files()].
#'
#' The source arrives transposed and mirrored in both axes and carries no
#' georeference of its own, so the reader transposes it back and sets the
#' extent to a 8750 km square in the projection of the source. Values above
#' 1e36 are the source's fill and are set to NA.
#'
#' @inheritParams raadtools
#' @param varname variable to read from the source file. The default reads the
#'   file without naming one, which is only sensible for a file with a single
#'   variable in it.
#' @return SpatRaster with one layer per date, or a data.frame of files if
#'   `returnfiles = TRUE`
#' @seealso [altimetry_antarctica_daily_files()]
#' @export
#' @examples
#' \dontrun{
#' read_altimetry_antarctica_daily(latest = TRUE)
#' }
read_altimetry_antarctica_daily <- function(date, xylim = NULL, latest = TRUE, returnfiles = FALSE, varname = "", lon180 = FALSE, ..., inputfiles = NULL) {
  if (is.null(inputfiles)){
    files <- altimetry_antarctica_daily_files()
    
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
  
  nfiles <- nrow(files)
  ## progress
  pb <- progress::progress_bar$new(
    format = "  extracting [:bar] :percent in :elapsed",
    total = nfiles, clear = FALSE, width= 60)
  pb$tick(0)
  ## this source arrives transposed and mirrored in both axes, and carries no
  ## georeference of its own
  read_fun <- function(xfile, ext, msk, rot, varname = "", band = 1) {
    pb$tick()
    rrr <- if (nzchar(varname)) .rast_nc(xfile, subds = varname) else .rast_nc(xfile)
    if (band > 1L) rrr <- rrr[[band]]
    rrr <- terra::flip(terra::flip(terra::trans(rrr), direction = "horizontal"),
                       direction = "vertical")
    terra::ext(rrr) <- terra::ext(c(-1, 1, -1, 1) * 4375000)
    rrr <- crop_if_needed(rrr, ext)
    rrr[rrr > 1e36] <- NA
    rrr
  }
  
  msk <- NULL
  rot <- FALSE
  files$band <- 1
  op <- options(warn = -1)
  r0 <- terra::rast(lapply(seq_len(nrow(files)), function(xi)
    read_fun(files$fullname[xi], ext = xylim, msk = msk, rot = rot, varname = varname, band = files$band[xi])))
  options(op)
  
  terra::time(r0) <- files$date
  names(r0) <- format(files$date, "%Y-%m-%d")
  .write_if_filename(r0, ...)
}
