# R/compat-ccmp.R
# Front door for read_ccmp() - dispatches to the terra-native reader

#' Read CCMP wind files
#'
#' @description
#' \code{read_ccmp} picks a sensible default and returns a terra
#' \code{SpatRaster}. For the historical defaults pinned explicitly, see
#' \code{\link{read_ccmp_wind_6hourly}}.
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
#' @return \code{SpatRaster}, or tibble if \code{returnfiles = TRUE}
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

  .shim_notice("read_ccmp", "read_ccmp_wind_6hourly")

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

  r
}
