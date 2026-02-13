# R/read-wind.R
# Terra-native NCEP2 wind readers
#
# Tier 2: Band-indexed files with U/V in separate files

#' Read NCEP2 wind data
#'
#' Read wind velocity from NCEP2 reanalysis products.
#'
#' @details
#' Data contain U (eastward) and V (northward) wind components stored in
#' separate files. By default both components are returned as a 2-layer
#' SpatRaster. Use the component arguments to return derived quantities:
#'
#' \itemize{
#'   \item \code{magonly = TRUE}: wind speed (magnitude), sqrt(U² + V²)
#'   \item \code{dironly = TRUE}: wind direction (degrees, meteorological: N=0, E=90, S=180, W=270)
#'   \item \code{uonly = TRUE}: U component only
#'   \item \code{vonly = TRUE}: V component only
#' }
#'
#' Only one of these flags may be TRUE. When reading multiple dates with the
#' default (both U and V), only one timestep can be read at a time.
#'
#' Data are 6-hourly on a global 2.5° grid (144x73 cells).
#'
#' @param date date or dates to read (character, Date, or POSIXct).
#' @param time.resolution time resolution, currently only "6hourly" supported.
#' @param xylim extent to crop, anything acceptable to \code{terra::ext()}.
#' @param lon180 logical, ensure Atlantic-centered longitude? Default TRUE.
#' @param magonly logical, return wind speed only?
#' @param dironly logical, return wind direction only?
#' @param uonly logical, return U (eastward) component only?
#' @param vonly logical, return V (northward) component only?
#' @param latest if TRUE and date missing, return latest; if FALSE, earliest.
#' @param returnfiles if TRUE, return file catalog instead of data.
#' @param inputfiles optional pre-filtered file catalog (from \code{windfiles()}).
#' @param ... currently ignored.
#'
#' @return \code{SpatRaster}. If default (no component flag), 2 layers named "U", "V".
#'   With a component flag, single layer per date. Time dimension is set.
#'
#' @seealso \code{\link{readwind}} for legacy interface, \code{\link{windfiles}}
#'   for file catalog
#'
#' @export
#' @examples
#' \dontrun{
#' # Latest wind (U and V)
#' uv <- read_ncep2_wind_6hourly()
#'
#' # Wind speed - can read multiple dates
#' speed <- read_ncep2_wind_6hourly(c("2023-01-15", "2023-01-15 06:00"), magonly = TRUE)
#'
#' # Direction
#' dir <- read_ncep2_wind_6hourly("2023-01-15", dironly = TRUE)
#' }
read_ncep2_wind_6hourly <- function(date,
                                     time.resolution = "6hourly",
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
  # windfiles() does the band expansion - one row per 6-hourly timestep
  files <- inputfiles %||% windfiles(time.resolution = time.resolution)

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
    warning("only one time index can be read at once unless 'magonly', 'dironly', 'uonly' or 'vonly' is TRUE; using first date")
    files <- files[1L, , drop = FALSE]
  }

  # --- read and process ---
  results <- vector("list", nrow(files))

  for (i in seq_len(nrow(files))) {
    # Read U and V from separate files, specific band
    u <- terra::rast(files$ufullname[i], lyrs = files$band[i])
    v <- terra::rast(files$vfullname[i], lyrs = files$band[i])

    e <- terra::ext(u)
    res_x <- terra::res(u)[1]
    terra::ext(u) <- c(e[1] + res_x/2, e[2] + res_x/2, e[3], e[4])
    terra::ext(v) <- c(e[1] + res_x/2, e[2] + res_x/2, e[3], e[4])
    
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
    nm <- if (magonly) "speed" else if (dironly) "direction" else if (uonly) "U" else "V"
    names(out) <- paste(nm, format(files$date, "%Y-%m-%d_%H%M"), sep = "_")
  }

  out
}
