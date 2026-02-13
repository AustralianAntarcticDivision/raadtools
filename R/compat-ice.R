# R/compat-ice.R
# Legacy shim for readice() - dispatches to terra-native readers
# Returns raster::brick() for backward compatibility
#
# The hemisphere = "both" case with vapour warping is kept inline here
# as it's complex and not worth terra-forming yet.

#' Read sea ice concentration data
#'
#' @description
#' `r lifecycle::badge("superseded")`
#'
#' For single-hemisphere reads, \code{readice} is superseded by
#' \code{\link{read_nsidc_ice_daily}} and \code{\link{read_nsidc_ice_monthly}},
#' which return terra \code{SpatRaster} objects.
#'
#' The \code{hemisphere = "both"} functionality (warping both hemispheres to a
#' common grid) is only available through this legacy function.
#'
#' @param date date or dates of data to read
#' @param time.resolution deprecated, use \code{readice_daily} or \code{readice_monthly}
#' @param product only "nsidc" supported
#' @param hemisphere "south", "north", or "both"
#' @param xylim spatial extents to crop, or a raster template for warping
#' @param setNA mask special values as NA
#' @param rescale ignored (v2 data is already scaled)
#' @param latest if TRUE and date missing, return latest available
#' @param returnfiles if TRUE, return file catalog
#' @param ... passed to underlying reader
#' @param inputfiles optional pre-filtered file catalog
#' @param resample resampling method for warper (when hemisphere = "both")
#'
#' @return \code{RasterBrick} or \code{RasterLayer}, or tibble if \code{returnfiles = TRUE}
#'
#' @seealso
#' \code{\link{read_nsidc_ice_daily}} for modern terra-based daily reader
#' \code{\link{read_nsidc_ice_monthly}} for modern terra-based monthly reader
#'
#' @export
readice <- function(date,
                    time.resolution = "daily",
                    product = "nsidc",
                    hemisphere = c("south", "north", "both"),
                    xylim = NULL,
                    setNA = TRUE,
                    rescale = FALSE,
                    latest = TRUE,
                    returnfiles = FALSE,
                    ...,
                    inputfiles = NULL,
                    resample = "bilinear") {

  product <- match.arg(product)
  hemisphere <- match.arg(hemisphere)

  # Handle rescale deprecation
  if (rescale) {
    if (isTRUE(getOption("raadtools.message.rescale", TRUE))) {
      message("since v2 of NSIDC 25km sea ice, 'rescale' no longer has meaning, ignored")
      options(raadtools.message.rescale = FALSE)
    }
  }

  # === HEMISPHERE = "BOTH" - keep legacy vapour warper logic ===
  if (hemisphere == "both") {
    return(.readice_both_legacy(
      date = date,
      xylim = xylim,
      setNA = setNA,
      latest = latest,
      returnfiles = returnfiles,
      inputfiles = inputfiles,
      resample = resample,
      ...
    ))
  }

  # === SINGLE HEMISPHERE - dispatch to terra-native, convert to raster ===
  if (isTRUE(getOption("raadtools.shim.warn", TRUE))) {
    .Deprecated("read_nsidc_ice_daily", package = "raadtools",
      msg = paste0(
        "'readice' is deprecated for single-hemisphere reads. ",
        "Use 'read_nsidc_ice_daily' for terra-native output.\n",
        "Set options(raadtools.shim.warn = FALSE) to suppress this warning."
      ))
  }

  r <- read_nsidc_ice_daily(
    date = date,
    hemisphere = hemisphere,
    xylim = xylim,
    setNA = setNA,
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


#' @rdname readice
#' @export
readice_daily <- readice


#' @rdname readice
#' @export
readice_monthly <- function(date,
                             time.resolution = "monthly",
                             product = "nsidc",
                             hemisphere = c("south", "north"),
                             xylim = NULL,
                             setNA = TRUE,
                             rescale = TRUE,
                             latest = TRUE,
                             returnfiles = FALSE,
                             ...,
                             inputfiles = NULL) {

  product <- match.arg(product)
 hemisphere <- match.arg(hemisphere)

  if (isTRUE(getOption("raadtools.shim.warn", TRUE))) {
    .Deprecated("read_nsidc_ice_monthly", package = "raadtools")
  }

  r <- read_nsidc_ice_monthly(
    date = date,
    hemisphere = hemisphere,
    xylim = xylim,
    setNA = setNA,
    latest = latest,
    returnfiles = returnfiles,
    inputfiles = inputfiles,
    ...
  )

  if (returnfiles) return(r)

  out <- raster::brick(r)
  raster::setZ(out, terra::time(r))
}


# -----------------------------------------------------------------------------
# Legacy "both" hemisphere implementation (vapour warper)
# Kept here rather than refactored - it works, it's complex, leave it alone
# -----------------------------------------------------------------------------

.readice_both_legacy <- function(date,
                                  xylim = NULL,
                                  setNA = TRUE,
                                  latest = TRUE,
                                  returnfiles = FALSE,
                                  inputfiles = NULL,
                                  resample = "bilinear",
                                  ...) {

  # Default global grid if xylim not specified
  if (is.null(xylim)) {
    opt <- getOption("raadtools.both.hemisphere.message")
    if (is.null(opt) || !opt) {
      options("raadtools.both.hemisphere.message" = TRUE)
      message("for both hemispheres, 'xylim' may be specified - assuming global longlat at 0.25 degree")
    }
    xylim <- raster::raster()
    raster::res(xylim) <- 0.25
  }

  # Extract grid specs from xylim
  if (inherits(xylim, "SpatRaster")) {
    dimension <- dim(xylim)[2:1]
    projection <- xylim@ptr$get_crs("wkt")
    ex <- xylim@ptr$extent@.xData$vector
    xylim <- raster::raster(xylim)
  } else if (inherits(xylim, "BasicRaster")) {
    dimension <- dim(xylim)[2:1]
    projection <- comment(raster::crs(xylim))
    ex <- c(raster::xmin(xylim), raster::xmax(xylim),
            raster::ymin(xylim), raster::ymax(xylim))
  } else {
    stop("xylim must be a raster or SpatRaster template for hemisphere='both'")
  }

  # Get combined file list
  files <- inputfiles %||% .get_both_hemisphere_files()

  if (returnfiles) return(files)

  if (missing(date)) {
    date <- if (latest) max(files$date) else min(files$date)
  }
  date <- timedateFrom(date)
  files <- .processFiles(date, files, "daily")

  # Warp both hemispheres to target grid
  list_of_fullname <- lapply(files$fullname, function(.x) vapour::vapour_vrt(.x, sds = 1))

  out <- lapply(list_of_fullname, function(.x)
    vapour::vapour_warp_raster_dbl(.x,
      extent = ex,
      dimension = dimension,
      projection = projection,
      resample = resample
    ))

  # Scale and mask
  rs <- 100
  if (setNA) {
    out <- lapply(out, function(.x) {
      .x[.x > 250] <- NA
      .x[!.x > 0] <- NA
      raster::setValues(xylim[[1]], .x * rs)
    })
  } else {
    out <- lapply(out, function(.x) {
      raster::setValues(xylim[[1]], .x * rs)
    })
  }

  raster::setZ(raster::brick(out), files$date)
}


# Helper from ice.R - get combined north+south file list
.get_both_hemisphere_files <- function() {
  north <- icefiles(hemisphere = "north")
  south <- icefiles(hemisphere = "south")

  tibble::tibble(
    date = north$date,
    fullname = split(
      rbind(north$fullname, south$fullname),
      rep(seq(1, nrow(north)), each = 2L)
    )
  )
}
