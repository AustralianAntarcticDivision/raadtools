# R/compat-currents.R
# Legacy shim for readcurr() - dispatches to terra-native reader
# Returns raster::brick() for backward compatibility

#' Read ocean current data
#'
#' @description
#' `r lifecycle::badge("superseded")`
#'
#' \code{readcurr} is superseded by \code{\link{read_copernicus_current_daily}},
#' which returns terra \code{SpatRaster} objects.
#'
#' @param date date or dates of data to read
#' @param xylim spatial extents to crop
#' @param lon180 if TRUE (default), use Atlantic-centered longitude
#' @param magonly return current speed only
#' @param dironly return current direction only
#' @param uonly return U component only
#' @param vonly return V component only
#' @param latest if TRUE and date missing, return latest
#' @param returnfiles if TRUE, return file catalog
#' @param ... passed to underlying reader
#' @param inputfiles optional pre-filtered file catalog
#'
#' @return \code{RasterBrick} or \code{RasterLayer}, or tibble if \code{returnfiles = TRUE}
#'
#' @seealso \code{\link{read_copernicus_current_daily}} for modern terra-based reader
#'
#' @export
readcurr <- function(date,
                     xylim = NULL,
                     lon180 = TRUE,
                     magonly = FALSE,
                     dironly = FALSE,
                     uonly = FALSE,
                     vonly = FALSE,
                     latest = TRUE,
                     returnfiles = FALSE,
                     ...,
                     inputfiles = NULL) {

  if (isTRUE(getOption("raadtools.shim.warn", TRUE))) {
    .Deprecated("read_copernicus_current_daily", package = "raadtools",
      msg = paste0(
        "'readcurr' is deprecated. ",
        "Use 'read_copernicus_current_daily' for terra-native output.\n",
        "Set options(raadtools.shim.warn = FALSE) to suppress this warning."
      ))
  }

  r <- read_copernicus_current_daily(
    date = date,
    xylim = xylim,
    lon180 = lon180,
    magonly = magonly,
    dironly = dironly,
    uonly = uonly,
    vonly = vonly,
    latest = latest,
    returnfiles = returnfiles,
    inputfiles = inputfiles,
    ...
  )

  if (returnfiles) return(r)

  # Convert to raster for backward compat
  out <- raster::brick(r)
  raster::setZ(out, terra::time(r))
}


#' @rdname readcurr
#' @export
readcurrents <- readcurr
