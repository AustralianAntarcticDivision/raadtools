# R/read-currents.R
# Terra-native ocean currents readers
#
# Tier 2: U/V component handling with mag/dir/u/v decomposition

#' Read Copernicus ocean surface currents
#'
#' Read geostrophic current velocity from Copernicus/AVISO altimetry products.
#'
#' @details
#' Data contain U (eastward) and V (northward) velocity components. By default
#' both components are returned as a 2-layer SpatRaster. Use the component
#' arguments to return derived quantities instead:
#'
#' \itemize{
#'   \item \code{magonly = TRUE}: current speed (magnitude), sqrt(U² + V²)
#'   \item \code{dironly = TRUE}: current direction (degrees, oceanographic convention)
#'   \item \code{uonly = TRUE}: U component only
#'   \item \code{vonly = TRUE}: V component only
#' }
#'
#' Only one of these flags may be TRUE. When reading multiple dates with the
#' default (both U and V), only one date can be read at a time - use a component
#' flag to read time series.
#'
#' Data are on a global 0.25° grid. Source orientation varies by file; use
#' \code{lon180 = TRUE} (default) to ensure Atlantic-centered output (-180 to 180).
#'
#' @param date date or dates to read (character, Date, or POSIXct).
#' @param xylim extent to crop, anything acceptable to \code{terra::ext()}.
#' @param lon180 logical, ensure Atlantic-centered longitude? Default TRUE.
#' @param magonly logical, return current speed only?
#' @param dironly logical, return current direction only?
#' @param uonly logical, return U (eastward) component only?
#' @param vonly logical, return V (northward) component only?
#' @param latest if TRUE and date missing, return latest; if FALSE, earliest.
#' @param returnfiles if TRUE, return file catalog instead of data.
#' @param inputfiles optional pre-filtered file catalog.
#' @param ... currently ignored.
#'
#' @return \code{SpatRaster}. If default (no component flag), 2 layers named "U", "V".
#'   With a component flag, single layer per date. Time dimension is set.
#'
#' @seealso \code{\link{readcurr}} for legacy interface
#'
#' @export
#' @examples
#' \dontrun{
#' # Latest currents (U and V)
#' uv <- read_copernicus_current_daily()
#'
#' # Current speed only - can read multiple dates
#' speed <- read_copernicus_current_daily(c("2023-01-15", "2023-06-15"), magonly = TRUE)
#'
#' # Direction
#' dir <- read_copernicus_current_daily("2023-01-15", dironly = TRUE)
#' }
read_copernicus_current_daily <- function(date,
                                           xylim = NULL,
                                           lon180 = TRUE,
                                           magonly = FALSE,
                                           dironly = FALSE,
                                           uonly = FALSE,
                                           vonly = FALSE,
                                           latest = TRUE,
                                           returnfiles = FALSE,
                                           ...,
                                           inputfiles = NULL) {

  # --- validate component flags ---
  flags <- c(magonly, dironly, uonly, vonly)
  if (sum(flags) > 1) {
    stop("only one of 'magonly', 'dironly', 'uonly', 'vonly' may be TRUE")
  }
  single_output <- any(flags)

  # --- file catalog ---
  files <- inputfiles %||% raadfiles::altimetry_daily_files()

  if (returnfiles) return(files)

  # --- date handling ---
  if (missing(date)) {
    date <- if (latest) max(files$date) else min(files$date)
  }
  date <- timedateFrom(date)
  files <- .processFiles(date, files, "daily")

  if (nrow(files) == 0L) {
    stop("no files found for requested dates")
  }

  # Multiple dates only allowed with single-component output
  if (nrow(files) > 1 && !single_output) {
    warning("only one date can be read at a time unless 'magonly', 'dironly', 'uonly' or 'vonly' is TRUE; using first date")
    files <- files[1L, , drop = FALSE]
  }

  # --- read and process ---
  results <- vector("list", nrow(files))

  for (i in seq_len(nrow(files))) {
    f <- files$fullname[i]

    # Read U and V components
    u <- terra::rast(f, subds = "ugos")
    v <- terra::rast(f, subds = "vgos")

    # Handle rotation based on file orientation and lon180 preference
    needs_rotate <- .needs_rotation(u, lon180)
    if (needs_rotate) {
      u <- terra::rotate(u)
      v <- terra::rotate(v)
    }

    # Combine based on flags
    r <- if (magonly) {
      sqrt(u^2 + v^2)
    } else if (dironly) {
      # Oceanographic convention: direction current is flowing TO
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
  terra::time(out) <- as.Date(rep(files$date, each = if (single_output) 1L else 2L))

  # Set names
  if (single_output) {
    nm <- if (magonly) "speed" else if (dironly) "direction" else if (uonly) "U" else "V"
    names(out) <- paste(nm, format(files$date, "%Y-%m-%d"), sep = "_")
  }

  out
}


#' @rdname read_copernicus_current_daily
#' @export
read_aviso_current_daily <- read_copernicus_current_daily


# -----------------------------------------------------------------------------
# Internal helpers
# -----------------------------------------------------------------------------

#' Check if raster needs rotation to match lon180 preference
#'
#' @param r SpatRaster to check
#' @param lon180 desired output: TRUE for -180/180, FALSE for 0/360
#' @return logical, TRUE if rotation needed
#' @noRd
.needs_rotation <- function(r, lon180) {
  ext <- as.vector(terra::ext(r))
  xmin <- ext[1]
  xmax <- ext[2]

 # Determine current orientation
  is_atlantic <- xmin < 0  # has negative longitudes = Atlantic view

  # Need to rotate if current doesn't match desired
  if (lon180 && !is_atlantic) {
    return(TRUE)  # want Atlantic, have Pacific -> rotate
 }
  if (!lon180 && is_atlantic) {
    return(TRUE)  # want Pacific, have Atlantic -> rotate
  }

  FALSE
}
