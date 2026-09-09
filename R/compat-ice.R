# R/compat-ice.R
# Front door for readice() - dispatches to the terra-native readers
#
# The hemisphere = "both" case warps both grids onto one template with vapour
# and is implemented inline here.

#' Read sea ice concentration data
#'
#' @description
#' \code{readice} picks a sensible default and returns a terra
#' \code{SpatRaster}. That default is now the NOAA/NSIDC Climate Data Record
#' (G02202 V6), read by \code{\link{read_nsidc_cdr_daily}}. For the historical
#' defaults pinned explicitly, see \code{\link{read_nsidc_ice_daily}} and
#' \code{\link{read_nsidc_ice_monthly}}, which stay on NSIDC-0051 v2.
#'
#' The \code{hemisphere = "both"} functionality (warping both hemispheres to a
#' common grid) is only available through this function.
#'
#' @param date date or dates of data to read
#' @param time.resolution deprecated, use \code{readice_daily} or \code{readice_monthly}
#' @param product "cdr" (default) for the Climate Data Record, or "nsidc" for
#'   NSIDC-0051 v2
#' @param hemisphere "south", "north", or "both"
#' @param xylim spatial extents to crop, or a SpatRaster (or Raster*) template
#'   to warp onto when \code{hemisphere = "both"}
#' @param setNA mask special values as NA
#' @param rescale ignored (v2 data is already scaled)
#' @param latest if TRUE and date missing, return latest available
#' @param returnfiles if TRUE, return file catalog
#' @param ... passed to underlying reader
#' @param inputfiles optional pre-filtered file catalog
#' @param resample resampling method for warper (when hemisphere = "both")
#'
#' @return \code{SpatRaster}, or tibble if \code{returnfiles = TRUE}
#'
#' @seealso
#' \code{\link{read_nsidc_cdr_daily}} for the Climate Data Record reader
#' \code{\link{read_nsidc_ice_daily}} for the NSIDC-0051 v2 daily reader
#' \code{\link{read_nsidc_ice_monthly}} for the monthly reader
#'
#' @export
readice <- function(date,
                    time.resolution = "daily",
                    product = c("cdr", "nsidc"),
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
      product = product,
      xylim = xylim,
      setNA = setNA,
      latest = latest,
      returnfiles = returnfiles,
      inputfiles = inputfiles,
      resample = resample,
      ...
    ))
  }

  # === SINGLE HEMISPHERE - dispatch to the terra-native reader ===
  .shim_notice("readice", if (product == "cdr") "read_nsidc_cdr_daily" else "read_nsidc_ice_daily")

  reader <- if (product == "cdr") read_nsidc_cdr_daily else read_nsidc_ice_daily

  r <- reader(
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

  r
}


#' @rdname readice
#' @export
readice_daily <- readice


#' @rdname readice
#' @export
readice_monthly <- function(date,
                             time.resolution = "monthly",
                             product = "nsidc",  ## no CDR monthly lister yet
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

  .shim_notice("readice_monthly", "read_nsidc_ice_monthly")

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

  r
}


# -----------------------------------------------------------------------------
# Legacy "both" hemisphere implementation (vapour warper)
# Kept here rather than refactored - it works, it's complex, leave it alone
# -----------------------------------------------------------------------------

.readice_both_legacy <- function(date,
                                  product = "cdr",
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
    xylim <- terra::rast(resolution = 0.25)
  }

  # Normalise the template to a single-layer SpatRaster, then read the grid
  # specs off it. A Raster* template is still accepted on the way in.
  if (inherits(xylim, "SpatRaster")) {
    template <- xylim[[1]]
  } else if (inherits(xylim, "BasicRaster")) {
    template <- terra::rast(xylim)[[1]]
  } else {
    stop("xylim must be a raster or SpatRaster template for hemisphere='both'")
  }

  dimension <- c(terra::ncol(template), terra::nrow(template))
  projection <- terra::crs(template)
  ex <- as.vector(terra::ext(template))

  # Get combined file list
  files <- inputfiles %||% .get_both_hemisphere_files(product)

  if (returnfiles) return(files)

  if (missing(date)) {
    date <- if (latest) max(files$date) else min(files$date)
  }
  date <- timedateFrom(date)
  files <- .processFiles(date, files, "daily")

  # Warp both hemispheres to target grid. A CDR entry is already a DSN naming
  # the concentration subdataset, so it must not be indexed by position - band
  # 1 of a CDR file is an interpolation flag.
  list_of_fullname <- lapply(files$fullname, function(.x) {
    if (product == "cdr") vapour::vapour_vrt(.x) else vapour::vapour_vrt(.x, sds = 1)
  })

  out <- lapply(list_of_fullname, function(.x)
    vapour::vapour_warp_raster_dbl(.x,
      extent = ex,
      dimension = dimension,
      projection = projection,
      resample = resample
    ))

  # Concentration is a fraction, so nothing is scaled here. setNA only has to
  # take out the flag codes, which sit above 1 - open water is a legitimate 0
  # and must survive.
  if (setNA) {
    out <- lapply(out, function(.x) {
      .x[.x > 1] <- NA
      .x
    })
  }

  out <- terra::rast(lapply(out, function(.x) terra::setValues(template, .x)))
  terra::time(out) <- as.Date(files$date)
  names(out) <- format(files$date, "%Y-%m-%d")
  out
}


# Helper from ice.R - get combined north+south file list
.get_both_hemisphere_files <- function(product = "cdr") {
  north <- icefiles(product = product, hemisphere = "north")
  south <- icefiles(product = product, hemisphere = "south")

  ## The two hemispheres are listed independently and do not have to cover the
  ## same days, so pair them by date rather than by position - a length or
  ## offset difference would otherwise silently pair a northern grid with the
  ## wrong southern one.
  col <- if (product == "cdr") "vrt_dsn" else "fullname"
  both <- dplyr::inner_join(
    tibble::tibble(date = north$date, .north = north[[col]]),
    tibble::tibble(date = south$date, .south = south[[col]]),
    by = "date"
  )

  tibble::tibble(
    date = both$date,
    fullname = split(
      rbind(both$.north, both$.south),
      rep(seq_len(nrow(both)), each = 2L)
    )
  )
}
