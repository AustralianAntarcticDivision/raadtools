# R/compat-oc.R
# Front doors for the ocean colour readers

#' Read daily chlorophyll-a
#'
#' @description
#' \code{read_chla_daily} picks a sensible default and returns a terra
#' \code{SpatRaster}. For the historical defaults pinned explicitly, see
#' \code{\link{read_oc_chl_daily}}.
#'
#' @inheritParams read_oc_chl_daily
#' @param setNA ignored (for backward compatibility)
#' @param time.resolution ignored, use specific functions for different resolutions
#'
#' @return \code{SpatRaster}
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

  .shim_notice("read_chla_daily", "read_oc_chl_daily")

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

  r
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

  .shim_notice("read_chla_weekly", "read_oc_chl_8day")

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

  r
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

  .shim_notice("read_chla_monthly", "read_oc_chl_monthly")

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

  r
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

  .shim_notice("read_par", "read_oc_par_8day")

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

  r
}


# =============================================================================
# DEPRECATED - to be removed
# =============================================================================

#' @export
readCHL_month <- function(...) {
  .Defunct("read_oc_chl_monthly", package = "raadtools",
    msg = "readCHL_month is defunct. Use read_oc_chl_monthly() instead.")
}
