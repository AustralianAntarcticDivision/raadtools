# R/read-amsr-ice.R
# Terra-native AMSR and CERSAT sea ice readers
#
# All Antarctic polar stereographic, different sensors/resolutions:
# - AMSR (combined AMSR-E + AMSR2): 6.25km, 2002+
# - AMSR2 3k: 3.125km, 2012+
# - CERSAT SSM/I: 12.5km, 1991+

# =============================================================================
# Shared constants
# =============================================================================

.antarctic_extent <- function() {
  c(-3950000, 3950000, -3950000, 4350000)
}

.antarctic_crs <- function() {
  "+proj=stere +lat_0=-90 +lat_ts=-70 +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs"
}


# =============================================================================
# AMSR combined 6km (AMSR-E + AMSR2)
# =============================================================================

#' Read AMSR sea ice concentration (6km)
#'
#' Read Antarctic sea ice concentration from combined AMSR-E (2002-2011) and
#' AMSR2 (2012+) sensors at 6.25km resolution.
#'
#' @details
#' Data are in Antarctic polar stereographic projection. Both sensor eras are
#' combined into a continuous time series. Earlier AMSR-E files (with values 0-1)
#' are automatically scaled to percentage (0-100).
#'
#' @param date date or dates to read (character, Date, or POSIXct).
#' @param xylim extent to crop (in polar stereographic coordinates), or NULL.
#' @param latest if TRUE and date missing, return latest; if FALSE, earliest.
#' @param returnfiles if TRUE, return file catalog instead of data.
#' @param inputfiles optional pre-filtered file catalog.
#' @param ... currently ignored.
#'
#' @return \code{SpatRaster} with time dimension set.
#'
#' @seealso \code{\link{read_amsr_ice_3k_daily}} for higher resolution AMSR2 data
#'
#' @export
read_amsr_ice_daily <- function(date,
                                 xylim = NULL,
                                 latest = TRUE,
                                 returnfiles = FALSE,
                                 ...,
                                 inputfiles = NULL) {

  files <- inputfiles %||% raadfiles::amsr_daily_files()

  if (returnfiles) return(files)

  if (missing(date)) {
    date <- if (latest) max(files$date) else min(files$date)
  }
  date <- timedateFrom(date)
  files <- .processFiles(date, files, "daily")

  if (nrow(files) == 0L) {
    stop("no files found for requested dates")
  }

  # Read and process each file
  rlist <- lapply(files$fullname, function(f) {
    r <- terra::rast(f)
    # Flip vertically (AMSR TIFs are upside down)
    r <- terra::flip(r, direction = "vertical")
    # Set extent
    terra::ext(r) <- terra::ext(.antarctic_extent())
    # Scale older AMSR-E files (0-1 range) to percentage
    if (grepl("asi.nl.s6250", basename(f))) {
      r <- r * 100
    }
    r
  })

  out <- terra::rast(rlist)
  terra::crs(out) <- .antarctic_crs()
  terra::time(out) <- as.Date(files$date)
  names(out) <- format(files$date, "%Y-%m-%d")

  if (!is.null(xylim)) {
    out <- terra::crop(out, terra::ext(xylim))
  }

  out
}

#' @rdname read_amsr_ice_daily
#' @export
read_amsr_ice <- read_amsr_ice_daily


# =============================================================================
# AMSR2 3km (high resolution)
# =============================================================================

#' Read AMSR2 sea ice concentration (3km)
#'
#' Read Antarctic sea ice concentration from AMSR2 sensor at 3.125km resolution.
#'
#' @details
#' Data are in Antarctic polar stereographic projection. This is the highest
#' resolution passive microwave sea ice product, available from 2012 onwards.
#'
#' @inheritParams read_amsr_ice_daily
#' @param setNA logical, mask values > 100 as NA? Default TRUE.
#'
#' @return \code{SpatRaster} with time dimension set.
#'
#' @seealso \code{\link{read_amsr_ice_daily}} for longer time series at 6km
#'
#' @export
read_amsr_ice_3k_daily <- function(date,
                                    xylim = NULL,
                                    setNA = TRUE,
                                    latest = TRUE,
                                    returnfiles = FALSE,
                                    ...,
                                    inputfiles = NULL) {

  files <- inputfiles %||% raadfiles::amsr2_3k_daily_files()

  if (returnfiles) return(files)

  if (missing(date)) {
    date <- if (latest) max(files$date) else min(files$date)
  }
  date <- timedateFrom(date)
  files <- .processFiles(date, files, "daily")

  if (nrow(files) == 0L) {
    stop("no files found for requested dates")
  }

  # Read files
  out <- terra::rast(files$fullname)
  terra::crs(out) <- .antarctic_crs()
  terra::time(out) <- as.Date(files$date)
  names(out) <- format(files$date, "%Y-%m-%d")

  if (setNA) {
    out <- terra::classify(out, matrix(c(100.001, Inf, NA), ncol = 3, byrow = TRUE), right = FALSE)
  }

  if (!is.null(xylim)) {
    out <- terra::crop(out, terra::ext(xylim))
  }

  out
}

#' @rdname read_amsr_ice_3k_daily
#' @export
read_amsr2_3k_ice <- read_amsr_ice_3k_daily


# =============================================================================
# CERSAT SSM/I 12.5km
# =============================================================================

#' Read CERSAT SSM/I sea ice concentration (12.5km)
#'
#' Read Antarctic sea ice concentration from CERSAT SSM/I at 12.5km resolution.
#'
#' @details
#' Data are in Antarctic polar stereographic projection. This is the longest
#' passive microwave sea ice record, available from 1991 onwards.
#'
#' @inheritParams read_amsr_ice_daily
#' @param setNA logical, mask values > 100 as NA? Default TRUE.
#'
#' @return \code{SpatRaster} with time dimension set.
#'
#' @seealso \code{\link{read_amsr_ice_daily}} for AMSR data
#'
#' @export
read_cersat_ice_daily <- function(date,
                                   xylim = NULL,
                                   setNA = TRUE,
                                   latest = TRUE,
                                   returnfiles = FALSE,
                                   ...,
                                   inputfiles = NULL) {

  files <- inputfiles %||% raadfiles::cersat_daily_files()

  if (returnfiles) return(files)

  if (missing(date)) {
    date <- if (latest) max(files$date) else min(files$date)
  }
  date <- timedateFrom(date)
  files <- .processFiles(date, files, "daily")

  if (nrow(files) == 0L) {
    stop("no files found for requested dates")
  }

  # Read and flip (CERSAT NetCDF is upside down)
  rlist <- lapply(files$fullname, function(f) {
    r <- terra::rast(f, subds = "concentration")
    terra::flip(r, direction = "vertical")
  })

  out <- terra::rast(rlist)
  terra::ext(out) <- terra::ext(.antarctic_extent())
  terra::crs(out) <- .antarctic_crs()
  terra::time(out) <- as.Date(files$date)
  names(out) <- format(files$date, "%Y-%m-%d")

  if (setNA) {
    out <- terra::classify(out, matrix(c(100.001, Inf, NA), ncol = 3, byrow = TRUE), right = FALSE)
  }

  if (!is.null(xylim)) {
    out <- terra::crop(out, terra::ext(xylim))
  }

  out
}

#' @rdname read_cersat_ice_daily
#' @export
read_cersat_ice <- read_cersat_ice_daily
