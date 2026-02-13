# R/read-cmems-ssh.R
# Terra-native CMEMS altimetry/SSH readers
#
# All variables from SEALEVEL_GLO_PHY_L4 product in one file:
# adt, sla, ugos, vgos, ugosa, vgosa, err
#
# Plus derived: current speed and direction

# =============================================================================
# Core internal reader - all the scalar vars use this
# =============================================================================

.read_cmems_ssh_var <- function(date,
                                 varname,
                                 xylim = NULL,
                                 lon180 = TRUE,
                                 latest = TRUE,
                                 returnfiles = FALSE,
                                 inputfiles = NULL) {

  # File catalog
 files <- inputfiles %||% raadfiles::altimetry_daily_files()

  if (returnfiles) return(files)

  # Date handling
  if (missing(date)) {
    date <- if (latest) max(files$date) else min(files$date)
  }
  date <- timedateFrom(date)
  files <- .processFiles(date, files, "daily")

  if (nrow(files) == 0L) {
    stop("no files found for requested dates")
  }

  # Read
  r <- terra::rast(files$fullname, subds = varname)

  # Set CRS if missing (CMEMS uses a slightly non-standard ellipsoid)
  if (is.na(terra::crs(r, proj = TRUE))) {
    terra::crs(r) <- "EPSG:4326"
  }

  # Rotation
  if (lon180) {
    needs_rotate <- .needs_rotation(r, lon180 = TRUE)
    if (needs_rotate) {
      r <- terra::rotate(r)
    }
  }

  # Crop
  if (!is.null(xylim)) {
    r <- terra::crop(r, terra::ext(xylim))
  }

  # Time
  terra::time(r) <- as.Date(files$date)
  names(r) <- format(files$date, "%Y-%m-%d")

  r
}


# =============================================================================
# Scalar variable readers - thin wrappers
# =============================================================================

#' Read CMEMS absolute dynamic topography
#'
#' Read absolute dynamic topography (ADT) from CMEMS SEALEVEL_GLO_PHY_L4 product.
#'
#' @param date date or dates to read (character, Date, or POSIXct).
#' @param xylim extent to crop, anything acceptable to \code{terra::ext()}.
#' @param lon180 logical, use Atlantic-centered longitude? Default TRUE.
#' @param latest if TRUE and date missing, return latest; if FALSE, earliest.
#' @param returnfiles if TRUE, return file catalog instead of data.
#' @param inputfiles optional pre-filtered file catalog.
#' @param ... currently ignored.
#'
#' @return \code{SpatRaster} with time dimension set.
#'
#' @seealso \code{\link{readssh}} for legacy interface
#'
#' @export
read_cmems_adt_daily <- function(date,
                                  xylim = NULL,
                                  lon180 = TRUE,
                                  latest = TRUE,
                                  returnfiles = FALSE,
                                  ...,
                                  inputfiles = NULL) {
  .read_cmems_ssh_var(
    date = date,
    varname = "adt",
    xylim = xylim,
    lon180 = lon180,
    latest = latest,
    returnfiles = returnfiles,
    inputfiles = inputfiles
  )
}


#' Read CMEMS sea level anomaly
#'
#' Read sea level anomaly (SLA) from CMEMS SEALEVEL_GLO_PHY_L4 product.
#'
#' @inheritParams read_cmems_adt_daily
#' @return \code{SpatRaster} with time dimension set.
#' @seealso \code{\link{readssh}} for legacy interface
#' @export
read_cmems_sla_daily <- function(date,
                                  xylim = NULL,
                                  lon180 = TRUE,
                                  latest = TRUE,
                                  returnfiles = FALSE,
                                  ...,
                                  inputfiles = NULL) {
  .read_cmems_ssh_var(
    date = date,
    varname = "sla",
    xylim = xylim,
    lon180 = lon180,
    latest = latest,
    returnfiles = returnfiles,
    inputfiles = inputfiles
  )
}


#' Read CMEMS geostrophic current U component
#'
#' Read eastward geostrophic current velocity (ugos) from CMEMS SEALEVEL_GLO_PHY_L4 product.
#'
#' @inheritParams read_cmems_adt_daily
#' @return \code{SpatRaster} with time dimension set.
#' @seealso \code{\link{readcurr}} for legacy interface with U/V decomposition options
#' @export
read_cmems_ugos_daily <- function(date,
                                   xylim = NULL,
                                   lon180 = TRUE,
                                   latest = TRUE,
                                   returnfiles = FALSE,
                                   ...,
                                   inputfiles = NULL) {
  .read_cmems_ssh_var(
    date = date,
    varname = "ugos",
    xylim = xylim,
    lon180 = lon180,
    latest = latest,
    returnfiles = returnfiles,
    inputfiles = inputfiles
  )
}


#' Read CMEMS geostrophic current V component
#'
#' Read northward geostrophic current velocity (vgos) from CMEMS SEALEVEL_GLO_PHY_L4 product.
#'
#' @inheritParams read_cmems_adt_daily
#' @return \code{SpatRaster} with time dimension set.
#' @seealso \code{\link{readcurr}} for legacy interface with U/V decomposition options
#' @export
read_cmems_vgos_daily <- function(date,
                                   xylim = NULL,
                                   lon180 = TRUE,
                                   latest = TRUE,
                                   returnfiles = FALSE,
                                   ...,
                                   inputfiles = NULL) {
  .read_cmems_ssh_var(
    date = date,
    varname = "vgos",
    xylim = xylim,
    lon180 = lon180,
    latest = latest,
    returnfiles = returnfiles,
    inputfiles = inputfiles
  )
}


#' Read CMEMS geostrophic current anomaly U component
#'
#' Read eastward geostrophic current anomaly (ugosa) from CMEMS SEALEVEL_GLO_PHY_L4 product.
#'
#' @inheritParams read_cmems_adt_daily
#' @return \code{SpatRaster} with time dimension set.
#' @export
read_cmems_ugosa_daily <- function(date,
                                    xylim = NULL,
                                    lon180 = TRUE,
                                    latest = TRUE,
                                    returnfiles = FALSE,
                                    ...,
                                    inputfiles = NULL) {
  .read_cmems_ssh_var(
    date = date,
    varname = "ugosa",
    xylim = xylim,
    lon180 = lon180,
    latest = latest,
    returnfiles = returnfiles,
    inputfiles = inputfiles
  )
}


#' Read CMEMS geostrophic current anomaly V component
#'
#' Read northward geostrophic current anomaly (vgosa) from CMEMS SEALEVEL_GLO_PHY_L4 product.
#'
#' @inheritParams read_cmems_adt_daily
#' @return \code{SpatRaster} with time dimension set.
#' @export
read_cmems_vgosa_daily <- function(date,
                                    xylim = NULL,
                                    lon180 = TRUE,
                                    latest = TRUE,
                                    returnfiles = FALSE,
                                    ...,
                                    inputfiles = NULL) {
  .read_cmems_ssh_var(
    date = date,
    varname = "vgosa",
    xylim = xylim,
    lon180 = lon180,
    latest = latest,
    returnfiles = returnfiles,
    inputfiles = inputfiles
  )
}


#' Read CMEMS altimetry error field
#'
#' Read formal mapping error from CMEMS SEALEVEL_GLO_PHY_L4 product.
#'
#' @inheritParams read_cmems_adt_daily
#' @return \code{SpatRaster} with time dimension set.
#' @export
read_cmems_err_daily <- function(date,
                                  xylim = NULL,
                                  lon180 = TRUE,
                                  latest = TRUE,
                                  returnfiles = FALSE,
                                  ...,
                                  inputfiles = NULL) {
  .read_cmems_ssh_var(
    date = date,
    varname = "err",
    xylim = xylim,
    lon180 = lon180,
    latest = latest,
    returnfiles = returnfiles,
    inputfiles = inputfiles
  )
}


# =============================================================================
# Aliases for existing function names (backward compat)
# =============================================================================

#' @rdname read_cmems_adt_daily
#' @export
read_adt_daily <- read_cmems_adt_daily

#' @rdname read_cmems_sla_daily
#' @export
read_sla_daily <- read_cmems_sla_daily

#' @rdname read_cmems_ugos_daily
#' @export
read_ugos_daily <- read_cmems_ugos_daily

#' @rdname read_cmems_vgos_daily
#' @export
read_vgos_daily <- read_cmems_vgos_daily

#' @rdname read_cmems_ugosa_daily
#' @export
read_ugosa_daily <- read_cmems_ugosa_daily

#' @rdname read_cmems_vgosa_daily
#' @export
read_vgosa_daily <- read_cmems_vgosa_daily

#' @rdname read_cmems_err_daily
#' @export
read_err_daily <- read_cmems_err_daily


# =============================================================================
# Derived current products - speed and direction
# =============================================================================

#' Read CMEMS geostrophic current speed
#'
#' Read geostrophic current speed (magnitude) derived from ugos and vgos
#' components of CMEMS SEALEVEL_GLO_PHY_L4 product.
#'
#' @details
#' Speed is computed as \code{sqrt(ugos^2 + vgos^2)}.
#'
#' @inheritParams read_cmems_adt_daily
#' @return \code{SpatRaster} with time dimension set. Values in m/s.
#' @seealso \code{\link{readcurr}} for legacy interface
#' @export
read_cmems_current_speed_daily <- function(date,
                                            xylim = NULL,
                                            lon180 = TRUE,
                                            latest = TRUE,
                                            returnfiles = FALSE,
                                            ...,
                                            inputfiles = NULL) {

  if (returnfiles) {
    return(.read_cmems_ssh_var(returnfiles = TRUE, inputfiles = inputfiles))
  }

  u <- read_cmems_ugos_daily(date, xylim = xylim, lon180 = lon180,
                              latest = latest, inputfiles = inputfiles)
  v <- read_cmems_vgos_daily(date, xylim = xylim, lon180 = lon180,
                              latest = latest, inputfiles = inputfiles)

  r <- sqrt(u^2 + v^2)
  terra::time(r) <- terra::time(u)
  names(r) <- paste0("speed_", names(u))

  r
}


#' Read CMEMS geostrophic current direction
#'
#' Read geostrophic current direction derived from ugos and vgos
#' components of CMEMS SEALEVEL_GLO_PHY_L4 product.
#'
#' @details
#' Direction is computed using oceanographic convention: direction the current
#' is flowing TO, in degrees where N=0, E=90, S=180, W=270.
#'
#' @inheritParams read_cmems_adt_daily
#' @return \code{SpatRaster} with time dimension set. Values in degrees (0-360).
#' @seealso \code{\link{readcurr}} for legacy interface
#' @export
read_cmems_current_direction_daily <- function(date,
                                                xylim = NULL,
                                                lon180 = TRUE,
                                                latest = TRUE,
                                                returnfiles = FALSE,
                                                ...,
                                                inputfiles = NULL) {

  if (returnfiles) {
    return(.read_cmems_ssh_var(returnfiles = TRUE, inputfiles = inputfiles))
  }

  u <- read_cmems_ugos_daily(date, xylim = xylim, lon180 = lon180,
                              latest = latest, inputfiles = inputfiles)
  v <- read_cmems_vgos_daily(date, xylim = xylim, lon180 = lon180,
                              latest = latest, inputfiles = inputfiles)

  # Oceanographic convention: direction current is flowing TO
  r <- (90 - terra::atan2(v, u) * 180 / pi) %% 360
  terra::time(r) <- terra::time(u)
  names(r) <- paste0("direction_", names(u))

  r
}
