#' Title
#'
#' @param time.resolution
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
ccmp_files <- function(time.resolution = "6hourly", ...) {
  files <- raadfiles::ccmp_6hourly_files()
  ## there are 6hourly for every time step
  files <- dplyr::slice(files, rep(seq_len(nrow(files)), each = 4L))
  files[["date"]] <- files[["date"]] + rep(c(0, 6, 12, 15), length.out = nrow(files)) * 3600
  files[["band"]] <- rep(1:4, length.out = nrow(files))
  files
}


#' Read CCMP wind files, from the RSS Cross-Calibrated Multi-Platform Ocean Surface Wind Project
#'
#' RSS CCMP_RT V2.1 derived surface winds (Level 3.0)
#'
#' CCMP wind data is read from files managed by
#' \code{\link{ccmp_files}}. Dates are matched to file names by
#' finding the nearest match in time within a short duration. By
#' default only one time step is returned with both U and V
#' components. Multiple dates can be returned for magnitude or
#' direction, U or V only or N-obs only.
#'
#' This is the " RSS VAM 6-hour analyses starting from the NCEP GFS wind analyses"
#'
#' See References.
#'
#' @param date date or dates of data to read, see Details
#' @param time.resolution time resolution (6hourly)
#' @param xylim spatial extents to crop from source data, can be anything accepted by \code{\link[raster]{extent}}, see Details
#' @param magonly return just the magnitude from the U and V
#' components
#' @param dironly return just the direction from the U and V, in degrees N=0, E=90, S=180, W=270
#' @param uonly return just the U component of velocity
#' @param vonly return just the V component of velocity
#' components, in degrees (0 north, 90 east, 180 south, 270 west)
#' @param latest if TRUE (and date not supplied) return the latest time available, otherwise the earliest
#' @param returnfiles ignore options and just return the file names and dates
#' @param ... passed to brick, primarily for \code{filename}
#' @export
#' @seealso \code{\link{icefiles}} for details on the repository of
#' data files, \code{\link[raster]{raster}} for the return value
# imports should not be necessary here
#' @importFrom raster t flip atan2
#' @export
#' @references \url{http://www.remss.com}
#' @examples
#' ## read a single time slice, direction only
#' x <- read_ccmp(dironly = TRUE)
#'
#'
#' ## get a local extent for a zoom plot and plot the directions [0,360) as an image with arrows
#' e <- extent(projectExtent(raster(extent(130, 150, -50, -30), crs = "+proj=longlat"), projection(x)))
#' x <- crop(read_ccmp(), e)
#' crds <- coordinates(x)
#' scale <- 0.05
#' vlen <- function(x) sqrt(x[[1]]^2 + x[[2]]^2)
#' plot(vlen(crop(x, e)))
#' x1 <- crds[,1]
#' y1 <- crds[,2]
#' x2 <- crds[,1] + values(x[[1]]) * scale
#' y2 <- crds[,2] + values(x[[2]]) * scale
#' arrows(x1, y1, x2, y2, length = 0.02)
#'
#' ## faster if we get the file list first
#' ccfiles <- ccmp_files()
#' earliest <- read_ccmp(ccfiles$date[1:16], xylim = e, magonly = TRUE, inputfiles = ccfiles)
#' plot(earliest, col = hcl.colors(64), zlim = c(0, 20))
read_ccmp <- function (date, time.resolution = c("6hourly"),
                      xylim = NULL,
                      lon180 = FALSE,
                      magonly = FALSE,
                      dironly = FALSE,
                      uonly = FALSE,
                      vonly = FALSE,
                      nobsonly = FALSE,
                      latest = TRUE,
                      returnfiles = FALSE, ..., inputfiles = NULL) {
  time.resolution <- match.arg(time.resolution)

  if (is.null(inputfiles)) {
    files <- ccmp_files(time.resolution = time.resolution)
  } else {
    files <- inputfiles
  }

  read_i_u <- function(file, xylim = NULL, lon180 = FALSE, band = 1L) {
    x <- raster(file, varname = "uwnd", band = band)
    if (lon180) x <- raadtools:::.rotate(x)
    if (!is.null(xylim)) x <- crop(x, xylim)

    x
  }
  read_i_v <- function(file, xylim = NULL, lon180 = FALSE, band = 1L) {
    x <- raster(file, varname = "vwnd", band = band)
    if (lon180) x <- raadtools:::.rotate(x)
    if (!is.null(xylim)) x <- crop(x, xylim)

    x
  }
  read_uv <- function(file, xylim = NULL, lon180 = FALSE, band = 1L) {
    stack(read_i_u(file, xylim = xylim, lon180 = lon180, band = band),
          read_i_v(file, xylim = xylim, lon180 = lon180, band = band))
  }
  read_i_dir <- function(file, xylim = NULL, lon180 = FALSE, band = 1L) {
    x <- read_uv(file, xylim = xylim, lon180 = lon180, band = band)
    overlay(x[[1]], x[[2]], fun = function(x, y) (90 - atan2(y, x) * 180/pi) %% 360)
  }
  vlen <- function(x, y) sqrt(x * x + y * y)
  read_i_mag <- function(file, xylim = NULL, lon180 = FALSE, band = 1L) {
    x <- read_uv(file, xylim = xylim, lon180 = lon180, band = band)
    vlen(x[[1]], x[[2]])
  }

  read_i_nobs <-  function(file, xylim = NULL, lon180 = FALSE, band = 1L) {
    x <- raster(file, varname = "nobs", band = band)
    if (lon180) x <- raadtools:::.rotate(x)
    if (!is.null(xylim)) x <- crop(x, xylim)

    x
  }
  thefun <- read_uv
  if (magonly) thefun <- read_i_mag
  if (dironly) thefun <- read_i_dir
  if (uonly ) thefun <- read_i_u
  if (vonly) thefun <- read_i_v
  if (nobsonly) thefun <- read_i_nobs


  if (returnfiles)
    return(files)
  if (missing(date)) date <- if (latest) max(files$date) else min(files$date)
  date <- timedateFrom(date)
  files <- .processFiles(date, files, time.resolution)

  nfiles <- nrow(files)

  ## prevent reading more than one unless mag/dironly
  if (nfiles > 1L & !magonly & !dironly & !uonly & !vonly & !nobsonly) {
    files <- files[1L,]
    nfiles <- 1L
    warning("only one time step can be read at once unless one of 'magonly', 'dironly', 'uonly' or 'vonly' is TRUE")
  }
  if ((magonly + dironly + uonly + vonly + nobsonly) > 1) stop("only one of 'magonly', 'dironly', 'uonly' or 'vonly' or 'nobsonly' may be TRUE")


  dots <- list(...)



  op <- options(warn = -1)
  on.exit(options(op))
  r0 <- stack(lapply(split(files[c("fullname", "band")], 1:nrow(files)),
                     function(.x) thefun(.x$fullname, xylim = xylim, lon180 = lon180, band = .x$band)), filename = filename)
  if (nlayers(r0) == nrow(files)) {
    r0 <- setZ(r0, files$date)
  } else {
    if (nlayers(r0) == 2 & nrow(files) == 1) {
      r0 <- setZ(r0, rep(files$date, 2))
    }
  }

  if ("filename" %in% names(dots)) {

     r0 <- writeRaster(r0, filename = dots[["filename"]])

  }


r0


}
