# R/compat-oc.R
# Legacy shims for ocean colour readers
# Returns raster objects for backward compatibility

#' Read daily chlorophyll-a
#'
#' @description
#' `r lifecycle::badge("superseded")`
#'
#' \code{read_chla_daily} is superseded by \code{\link{read_oc_chl_daily}},
#' which returns terra \code{SpatRaster} objects.
#'
#' @inheritParams read_oc_chl_daily
#' @param setNA ignored (for backward compatibility)
#' @param time.resolution ignored, use specific functions for different resolutions
#'
#' @return \code{RasterBrick} or \code{RasterLayer}
#'
#' @seealso \code{\link{read_oc_chl_daily}}
#'
#' @export
read_chla_daily <- function(date,
                             time.resolution = "daily",
                             xylim = NULL,
                             lon180 = TRUE,
                             varname = "chlor_a",
                             setNA = TRUE,
                             latest = TRUE,
                             returnfiles = FALSE,
                             ...,
                             inputfiles = NULL) {

  if (isTRUE(getOption("raadtools.shim.warn", TRUE))) {
    .Deprecated("read_oc_chl_daily", package = "raadtools",
      msg = paste0(
        "'read_chla_daily' is deprecated. ",
        "Use 'read_oc_chl_daily' for terra-native output.\n",
        "Set options(raadtools.shim.warn = FALSE) to suppress this warning."
      ))
  }

  r <- read_oc_chl_daily(
    date = date,
    xylim = xylim,
    lon180 = lon180,
    varname = varname,
    latest = latest,
    returnfiles = returnfiles,
    inputfiles = inputfiles,
    ...
  )

  if (returnfiles) return(r)

  out <- raster::brick(r)
  if (raster::nlayers(out) == 1) out <- out[[1]]
  raster::setZ(out, terra::time(r))
}


#' @rdname read_chla_daily
#' @export
read_chla_weekly <- function(date,
                              xylim = NULL,
                              lon180 = TRUE,
                              varname = "chlor_a",
                              setNA = TRUE,
                              latest = TRUE,
                              returnfiles = FALSE,
                              ...,
                              inputfiles = NULL) {

  if (isTRUE(getOption("raadtools.shim.warn", TRUE))) {
    .Deprecated("read_oc_chl_8day", package = "raadtools")
  }

  r <- read_oc_chl_8day(
    date = date,
    xylim = xylim,
    lon180 = lon180,
    varname = varname,
    latest = latest,
    returnfiles = returnfiles,
    inputfiles = inputfiles,
    ...
  )

  if (returnfiles) return(r)

  out <- raster::brick(r)
  if (raster::nlayers(out) == 1) out <- out[[1]]
  raster::setZ(out, terra::time(r))
}


#' @rdname read_chla_daily
#' @export
read_chla_monthly <- function(date,
                               xylim = NULL,
                               lon180 = TRUE,
                               varname = "chlor_a",
                               setNA = TRUE,
                               latest = TRUE,
                               returnfiles = FALSE,
                               ...,
                               inputfiles = NULL) {

  if (isTRUE(getOption("raadtools.shim.warn", TRUE))) {
    .Deprecated("read_oc_chl_monthly", package = "raadtools")
  }

  r <- read_oc_chl_monthly(
    date = date,
    xylim = xylim,
    lon180 = lon180,
    varname = varname,
    latest = latest,
    returnfiles = returnfiles,
    inputfiles = inputfiles,
    ...
  )

  if (returnfiles) return(r)

  out <- raster::brick(r)
  if (raster::nlayers(out) == 1) out <- out[[1]]
  raster::setZ(out, terra::time(r))
}


#' @rdname read_chla_daily
#' @export
read_par <- function(date,
                     time.resolution = "8D",
                     xylim = NULL,
                     lon180 = FALSE,
                     nobsonly = FALSE,
                     latest = TRUE,
                     returnfiles = FALSE,
                     ...,
                     inputfiles = NULL) {

  if (isTRUE(getOption("raadtools.shim.warn", TRUE))) {
    .Deprecated("read_oc_par_8day", package = "raadtools")
  }

  r <- read_oc_par_8day(
    date = date,
    xylim = xylim,
    lon180 = lon180,
    latest = latest,
    returnfiles = returnfiles,
    inputfiles = inputfiles,
    ...
  )

  if (returnfiles) return(r)

  out <- raster::stack(r)
  raster::setZ(out, terra::time(r))
}


# =============================================================================
# DEPRECATED - to be removed
# =============================================================================

#' @export
readCHL_month <- function(...) {
  .Defunct("read_oc_chl_monthly", package = "raadtools",
    msg = "readCHL_month is defunct. Use read_oc_chl_monthly() instead.")
}
