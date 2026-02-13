# R/compat-sst.R
# Legacy shim for readsst() - dispatches to terra-native readers
# Returns raster::brick() for backward compatibility
#
# USAGE: This file can either:
#   1. Replace the readsst() function in sst.R
#   2. Be added alongside sst.R (remove readsst from sst.R to avoid conflict)
#
# Once users have migrated to read_oisst_daily()/read_oisst_monthly(),
# this shim can be deprecated and eventually removed.

#' Read sea surface temperature data
#'
#' @description
#' `r lifecycle::badge("superseded")`
#'
#' \code{readsst} has been superseded by \code{\link{read_oisst_daily}} and
#' \code{\link{read_oisst_monthly}}, which return terra \code{SpatRaster} objects.
#' This function is retained for backward compatibility and returns
#' \code{raster::brick} objects.
#'
#' @param date date or dates of data to read
#' @param time.resolution time resolution: "daily" or "monthly"
#' @param xylim spatial extents to crop from source data
#' @param lon180 if TRUE (default), convert Pacific (0,360) to Atlantic (-180,180) view
#' @param varname variable to read: "sst", "anom", "err", or "ice"
#' @param setNA mask out land values (ignored in new implementation, kept for API compatibility
#' @param latest if TRUE and date missing, return latest available
#' @param returnfiles if TRUE, return the file catalog instead of data
#' @param ... passed to underlying reader
#' @param inputfiles optional pre-filtered file catalog
#'
#' @return \code{RasterBrick} or \code{RasterLayer}, or tibble if \code{returnfiles = TRUE}
#'
#' @seealso
#' \code{\link{read_oisst_daily}} for the modern terra-based daily reader
#' \code{\link{read_oisst_monthly}} for the modern terra-based monthly reader
#'
#' @export
#' @examples
#' \dontrun{
#' # Legacy usage (still works)
#' sst <- readsst("2023-01-15")
#' class(sst)  # RasterLayer
#'
#' # Modern usage (preferred)
#' sst <- read_oisst_daily("2023-01-15")
#' class(sst)  # SpatRaster
#' }
readsst <- function(date,
                    time.resolution = c("daily", "monthly"),
                    xylim = NULL,
                    lon180 = TRUE,
                    varname = c("sst", "anom", "err", "ice"),
                    setNA = TRUE,
                    latest = TRUE,
                    returnfiles = FALSE,
                    ...,
                    inputfiles = NULL) {

  time.resolution <- match.arg(time.resolution)
  varname <- match.arg(varname)

  # Emit deprecation notice (can be made louder/quieter via option)
  if (isTRUE(getOption("raadtools.shim.warn", TRUE))) {
    .Deprecated(
      new = switch(time.resolution,
                   daily = "read_oisst_daily",
                   monthly = "read_oisst_monthly"),
      package = "raadtools",
      msg = paste0(
        "'readsst' is deprecated. Use '",
        switch(time.resolution, daily = "read_oisst_daily", monthly = "read_oisst_monthly"),
        "' for terra-native output.\n",
        "Set options(raadtools.shim.warn = FALSE) to suppress this warning."
      )
    )
  }

  # Dispatch to appropriate terra-native reader
  r <- switch(time.resolution,
    daily = read_oisst_daily(
      date = date,
      xylim = xylim,
      lon180 = lon180,
      varname = varname,
      latest = latest,
      returnfiles = returnfiles,
      inputfiles = inputfiles,
      ...
    ),
    monthly = read_oisst_monthly(
      date = date,
      xylim = xylim,
      lon180 = lon180,
      varname = varname,
      latest = latest,
      returnfiles = returnfiles,
      inputfiles = inputfiles,
      ...
    )
  )

  # If returnfiles, pass through the tibble unchanged
  if (returnfiles) {
    return(r)
  }

  # Convert SpatRaster to raster::brick for backward compatibility
  out <- raster::brick(r)

  # Preserve Z values (time dimension) - raster style
  out <- raster::setZ(out, terra::time(r))

  out
}


#' @rdname readsst
#' @export
readsst_daily <- function(date,
                          xylim = NULL,
                          lon180 = TRUE,
                          varname = c("sst", "anom", "err", "ice"),
                          setNA = TRUE,
                          latest = TRUE,
                          returnfiles = FALSE,
                          ...,
                          inputfiles = NULL) {

  if (isTRUE(getOption("raadtools.shim.warn", TRUE))) {
    .Deprecated("read_oisst_daily", package = "raadtools")
  }

  r <- read_oisst_daily(
    date = date,
    xylim = xylim,
    lon180 = lon180,
    varname = match.arg(varname),
    latest = latest,
    returnfiles = returnfiles,
    inputfiles = inputfiles,
    ...
  )

  if (returnfiles) return(r)

  out <- raster::brick(r)
  raster::setZ(out, terra::time(r))
}
