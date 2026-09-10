## The source carries no georeference of its own - GDAL invents a 0,240,0,240
## lon/lat grid - so the reader has to assert both. Michael established these
## against a coastline overlay on 2026-09-10.
.altimetry_antarctica_ext <- c(-1, 1, -1, 1) * 8750000
.altimetry_antarctica_crs <- "+proj=laea +lat_0=-90 +lon_0=-90"

#' Antarctic altimetry files
#'
#' A one-row-per-date catalogue of the Antarctic altimetry time series. Each
#' source file found by [raadfiles::altimetry_antarctica_files()] holds the
#' whole of its own series, one band per date, so this reads the time dimension
#' of each and returns a row per date with the `band` to read within its file.
#'
#' @return data.frame of `date`, `fullname`, `band` and `root`
#' @seealso [read_altimetry_antarctica_daily()]
#' @export
altimetry_antarctica_daily_files <- function() {
  files <- raadfiles::altimetry_antarctica_files()
  out <- lapply(seq_len(nrow(files)), function(ifile) {
    nc <- RNetCDF::open.nc(files$fullname[ifile])
    on.exit(RNetCDF::close.nc(nc), add = TRUE)
    time <- RNetCDF::var.get.nc(nc, "time")
    times <- RNetCDF::utcal.nc(RNetCDF::att.get.nc(nc, "time", "units"), value = time)
    tibble::tibble(date = ISOdatetime(times[, "year"], times[, "month"], times[, "day"],
                                      times[, "hour"], times[, "minute"], times[, "second"],
                                      tz = "UTC"),
                   fullname = files$fullname[ifile],
                   band = seq_along(time),
                   root = files$root[ifile])
  })
  out <- dplyr::bind_rows(out)
  out[order(out$date), ]
}

#' Read Antarctic altimetry
#'
#' Read one or more dates of gridded Antarctic sea level anomaly, catalogued by
#' [altimetry_antarctica_daily_files()].
#'
#' The source has no georeference of its own, so the extent and CRS are
#' asserted here: a square of 8750000 m about the pole, in Lambert azimuthal
#' equal area on `+lat_0=-90 +lon_0=-90`.
#'
#' @inheritParams raadtools
#' @param varname variable to read: `"sla"` (the anomaly, the default),
#'   `"std_sla"` (its standard deviation) or `"number_sla"` (the count of
#'   observations behind it)
#' @return SpatRaster with one layer per date, or a data.frame of files if
#'   `returnfiles = TRUE`
#' @seealso [altimetry_antarctica_daily_files()]
#' @export
#' @examples
#' \dontrun{
#' read_altimetry_antarctica_daily(latest = TRUE)
#' }
read_altimetry_antarctica_daily <- function(date, xylim = NULL,
                                            varname = c("sla", "std_sla", "number_sla"),
                                            latest = TRUE, returnfiles = FALSE, ...,
                                            inputfiles = NULL) {
  varname <- match.arg(varname)
  files <- if (is.null(inputfiles)) altimetry_antarctica_daily_files() else inputfiles
  if (returnfiles) return(files)
  if (missing(date)) date <- if (latest) max(files$date) else min(files$date)
  date <- timedateFrom(date)
  files <- .processFiles(date, files, "daily")
  if (is.null(files$band)) files$band <- 1L

  ## One band per date within a file, so the band is always a selection,
  ## including band 1. Nothing else is needed: the values come out the right way
  ## up, and the old transpose-and-flip pair was a leftover from raster, which
  ## read netCDF the other way up to GDAL.
  r <- terra::rast(lapply(seq_len(nrow(files)), function(ifile)
    .rast_nc(files$fullname[ifile], subds = varname)[[files$band[ifile]]]))

  terra::ext(r) <- terra::ext(.altimetry_antarctica_ext)
  terra::crs(r) <- .altimetry_antarctica_crs
  ## the source's fill, which comes through as a very large number rather than NA
  r <- terra::clamp(r, upper = 1e36, values = FALSE)
  r <- crop_if_needed(r, xylim)
  .utctime(r) <- files$date
  names(r) <- format(files$date, "%Y-%m-%d")
  .write_if_filename(r, ...)
}
