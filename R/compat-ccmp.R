# R/compat-ccmp.R
# Legacy shim for read_ccmp() - dispatches to terra-native reader
# Returns raster::stack for backward compatibility

#' Read CCMP wind files
#'
#' @description
#' `r lifecycle::badge("superseded")`
#'
#' \code{read_ccmp} is superseded by \code{\link{read_ccmp_wind_6hourly}},
#' which returns terra \code{SpatRaster} objects.
#'
#' @param date date or dates of data to read
#' @param time.resolution time resolution, currently only "6hourly"
#' @param xylim spatial extents to crop
#' @param lon180 if TRUE, use Atlantic-centered longitude (default FALSE)
#' @param magonly return wind speed only
#' @param dironly return wind direction only
#' @param uonly return U component only
#' @param vonly return V component only
#' @param nobsonly return number of observations only
#' @param latest if TRUE and date missing, return latest
#' @param returnfiles if TRUE, return file catalog
#' @param ... passed to underlying reader
#' @param inputfiles optional pre-filtered file catalog
#'
#' @return \code{RasterStack} or \code{RasterLayer}, or tibble if \code{returnfiles = TRUE}
#'
#' @seealso \code{\link{read_ccmp_wind_6hourly}} for modern terra-based reader
#'
#' @references \url{http://www.remss.com}
#'
#' @export
read_ccmp <- function(date,
                      time.resolution = c("6hourly"),
                      xylim = NULL,
                      lon180 = FALSE,
                      magonly = FALSE,
                      dironly = FALSE,
                      uonly = FALSE,
                      vonly = FALSE,
                      nobsonly = FALSE,
                      latest = TRUE,
                      returnfiles = FALSE,
                      ...,
                      inputfiles = NULL) {

  time.resolution <- match.arg(time.resolution)

  if (isTRUE(getOption("raadtools.shim.warn", TRUE))) {
    .Deprecated("read_ccmp_wind_6hourly", package = "raadtools",
      msg = paste0(
        "'read_ccmp' is deprecated. ",
        "Use 'read_ccmp_wind_6hourly' for terra-native output.\n",
        "Set options(raadtools.shim.warn = FALSE) to suppress this warning."
      ))
  }

  r <- read_ccmp_wind_6hourly(
    date = date,
    time.resolution = time.resolution,
    xylim = xylim,
    lon180 = lon180,
    magonly = magonly,
    dironly = dironly,
    uonly = uonly,
    vonly = vonly,
    nobsonly = nobsonly,
    latest = latest,
    returnfiles = returnfiles,
    inputfiles = inputfiles,
    ...
  )

  if (returnfiles) return(r)

  # Convert to raster for backward compat
  out <- raster::stack(r)
  raster::setZ(out, terra::time(r))
}
