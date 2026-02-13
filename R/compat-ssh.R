# R/compat-ssh.R
# Legacy shim for readssh() - dispatches to terra-native readers
# Returns raster::brick() for backward compatibility

#' Read sea surface height data
#'
#' @description
#' `r lifecycle::badge("superseded")`
#'
#' \code{readssh} is superseded by \code{\link{read_cmems_adt_daily}} and
#' \code{\link{read_cmems_sla_daily}}, which return terra \code{SpatRaster} objects.
#'
#' @param date date or dates of data to read
#' @param time.resolution time resolution (only "daily" supported)
#' @param xylim spatial extents to crop
#' @param lon180 if TRUE (default), use Atlantic-centered longitude
#' @param ssha if TRUE, return sea level anomaly; if FALSE (default), return ADT
#' @param latest if TRUE and date missing, return latest
#' @param returnfiles if TRUE, return file catalog
#' @param verbose ignored (for backward compatibility)
#' @param ... passed to underlying reader
#' @param inputfiles optional pre-filtered file catalog
#'
#' @return \code{RasterBrick} or \code{RasterLayer}, or tibble if \code{returnfiles = TRUE}
#'
#' @seealso \code{\link{read_cmems_adt_daily}}, \code{\link{read_cmems_sla_daily}}
#'
#' @export
readssh <- function(date,
                    time.resolution = c("daily"),
                    xylim = NULL,
                    lon180 = TRUE,
                    ssha = FALSE,
                    latest = TRUE,
                    returnfiles = FALSE,
                    verbose = TRUE,
                    ...,
                    inputfiles = NULL) {

  time.resolution <- match.arg(time.resolution)

  if (isTRUE(getOption("raadtools.shim.warn", TRUE))) {
    new_fn <- if (ssha) "read_cmems_sla_daily" else "read_cmems_adt_daily"
    .Deprecated(new_fn, package = "raadtools",
      msg = paste0(
        "'readssh' is deprecated. ",
        "Use '", new_fn, "' for terra-native output.\n",
        "Set options(raadtools.shim.warn = FALSE) to suppress this warning."
      ))
  }

  # Dispatch based on ssha flag
  r <- if (ssha) {
    read_cmems_sla_daily(
      date = date,
      xylim = xylim,
      lon180 = lon180,
      latest = latest,
      returnfiles = returnfiles,
      inputfiles = inputfiles,
      ...
    )
  } else {
    read_cmems_adt_daily(
      date = date,
      xylim = xylim,
      lon180 = lon180,
      latest = latest,
      returnfiles = returnfiles,
      inputfiles = inputfiles,
      ...
    )
  }

  if (returnfiles) return(r)

  # Convert to raster for backward compat
  out <- raster::brick(r)
  raster::setZ(out, terra::time(r))
}
