# R/read-ccmp.R
# Terra-native CCMP wind readers
#
# RSS Cross-Calibrated Multi-Platform Ocean Surface Wind Project
# Tier 2: Band-indexed with U/V as subdatasets in same file

#' Read CCMP wind data
#'
#' Read wind velocity from RSS Cross-Calibrated Multi-Platform (CCMP) products.
#'
#' @details
#' CCMP provides RSS VAM 6-hour analyses starting from NCEP GFS wind analyses.
#' Data contain U (eastward) and V (northward) wind components as subdatasets
#' in the same file. By default both components are returned as a 2-layer
#' SpatRaster. Use the component arguments to return derived quantities:
#'
#' \itemize{
#'   \item \code{magonly = TRUE}: wind speed (magnitude), sqrt(U² + V²)
#'   \item \code{dironly = TRUE}: wind direction (degrees, meteorological: N=0, E=90, S=180, W=270)
#'   \item \code{uonly = TRUE}: U component only
#'   \item \code{vonly = TRUE}: V component only
#'   \item \code{nobsonly = TRUE}: number of observations only
#' }
#'
#' Only one of these flags may be TRUE. When reading multiple dates with the
#' default (both U and V), only one timestep can be read at a time.
#'
#' Data are 6-hourly on a global 0.25° grid.
#'
#' @param date date or dates to read (character, Date, or POSIXct).
#' @param time.resolution time resolution, currently only "6hourly" supported.
#' @param xylim extent to crop, anything acceptable to \code{terra::ext()}.
#' @param lon180 logical, use Atlantic-centered longitude? Default FALSE (Pacific-centered).
#' @param magonly logical, return wind speed only?
#' @param dironly logical, return wind direction only?
#' @param uonly logical, return U (eastward) component only?
#' @param vonly logical, return V (northward) component only?
#' @param nobsonly logical, return number of observations only?
#' @param latest if TRUE and date missing, return latest; if FALSE, earliest.
#' @param returnfiles if TRUE, return file catalog instead of data.
#' @param inputfiles optional pre-filtered file catalog (from \code{ccmp_files()}).
#' @param ... currently ignored.
#'
#' @return \code{SpatRaster}. If default (no component flag), 2 layers named "U", "V".
#'   With a component flag, single layer per date. Time dimension is set.
#'
#' @seealso \code{\link{read_ccmp}} for legacy interface, \code{\link{ccmp_files}}
#'   for file catalog
#'
#' @references \url{http://www.remss.com}
#'
#' @export
#' @examples
#' \dontrun{
#' # Latest wind (U and V)
#' uv <- read_ccmp_wind_6hourly()
#'
#' # Wind speed - can read multiple dates
#' speed <- read_ccmp_wind_6hourly(c("2023-01-15", "2023-01-15 06:00"), magonly = TRUE)
#'
#' # Number of observations
#' nobs <- read_ccmp_wind_6hourly("2023-01-15", nobsonly = TRUE)
#' }
read_ccmp_wind_6hourly <- function(date,
                                    time.resolution = "6hourly",
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

  # --- validate component flags ---
  flags <- c(magonly, dironly, uonly, vonly, nobsonly)
  if (sum(flags) > 1) {
    stop("only one of 'magonly', 'dironly', 'uonly', 'vonly', 'nobsonly' may be TRUE")
  }
  single_output <- any(flags)

  # --- file catalog ---
  # ccmp_files() does the band expansion - one row per 6-hourly timestep
  files <- inputfiles %||% ccmp_files(time.resolution = time.resolution)

  if (returnfiles) return(files)

  # --- date handling ---
  if (missing(date)) {
    date <- if (latest) max(files$date) else min(files$date)
  }
  date <- timedateFrom(date)
  files <- .processFiles(date, files, time.resolution)

  if (nrow(files) == 0L) {
    stop("no files found for requested dates")
  }

  # Multiple dates only allowed with single-component output
  if (nrow(files) > 1 && !single_output) {
    warning("only one time step can be read at once unless 'magonly', 'dironly', 'uonly', 'vonly' or 'nobsonly' is TRUE; using first date")
    files <- files[1L, , drop = FALSE]
  }

  # --- read and process ---
  results <- vector("list", nrow(files))

  for (i in seq_len(nrow(files))) {
    f <- files$fullname[i]
    band <- files$band[i]

    if (nobsonly) {
      # Read nobs only
      r <- terra::rast(f, subds = "nobs")[[band]]
    } else {
      # Read U and V from subdatasets, specific band
      u <- terra::rast(f, subds = "uwnd")[[band]]
      v <- terra::rast(f, subds = "vwnd")[[band]]

      # Handle rotation
      if (lon180) {
        needs_rotate <- .needs_rotation(u, lon180 = TRUE)
        if (needs_rotate) {
          u <- terra::rotate(u)
          v <- terra::rotate(v)
        }
      }

      # Combine based on flags
      r <- if (magonly) {
        sqrt(u^2 + v^2)
      } else if (dironly) {
        # Meteorological convention: direction wind is coming FROM
        # N=0, E=90, S=180, W=270
        (90 - terra::atan2(v, u) * 180 / pi) %% 360
      } else if (uonly) {
        u
      } else if (vonly) {
        v
      } else {
        # Both components
        r <- c(u, v)
        names(r) <- c("U", "V")
        r
      }
    }

    # Handle rotation for nobsonly
    if (nobsonly && lon180) {
      needs_rotate <- .needs_rotation(r, lon180 = TRUE)
      if (needs_rotate) {
        r <- terra::rotate(r)
      }
    }

    # Crop if requested
    if (!is.null(xylim)) {
      r <- terra::crop(r, terra::ext(xylim))
    }

    results[[i]] <- r
  }

  # Stack results
  if (length(results) == 1) {
    out <- results[[1]]
  } else {
    out <- terra::rast(results)
  }

  # Set CRS if missing
  if (is.na(terra::crs(out, proj = TRUE))) {
    terra::crs(out) <- "EPSG:4326"
  }

  # Set time
  terra::time(out) <- rep(files$date, each = if (single_output) 1L else 2L)

  # Set names
  if (single_output) {
    nm <- if (magonly) "speed" else if (dironly) "direction" else if (uonly) "U" else if (vonly) "V" else "nobs"
    names(out) <- paste(nm, format(files$date, "%Y-%m-%d_%H%M"), sep = "_")
  }

  out
}
