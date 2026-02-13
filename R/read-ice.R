# R/read-ice.R
# Terra-native NSIDC sea ice readers

#' Read NSIDC daily sea ice concentration
#'
#' Read NSIDC passive microwave sea ice concentration (25km) for a single hemisphere.
#'
#' @details
#' Data are returned in the native polar stereographic projection:
#' \itemize{
#'   \item South: EPSG:3976 (WGS 84 / NSIDC Sea Ice Polar Stereographic South)
#'   \item North: EPSG:3413 (WGS 84 / NSIDC Sea Ice Polar Stereographic North)
#' }
#'
#' Values are sea ice concentration as percentage (0-100). When \code{setNA = TRUE},
#' special values (land, coast, missing, polar hole) are masked to NA.
#'
#' The NSIDC archive contains some dates with placeholder files (no ice variable).
#' These are automatically filtered out using an internal index of known bad files.
#' When multiple sensor variables exist in a file, the first is used.
#'
#' For combined hemisphere output or reprojection to a custom grid, use the legacy
#' \code{\link{readice}} function with \code{hemisphere = "both"}.
#'
#' @param date date or dates to read (character, Date, or POSIXct).
#' @param hemisphere "south" (default) or "north".
#' @param xylim extent to crop (in native projection coordinates), or NULL.
#' @param setNA logical, mask special values (land, coast, missing) to NA? Default TRUE.
#' @param latest if TRUE and date missing, return latest; if FALSE, earliest.
#' @param returnfiles if TRUE, return file catalog instead of data.
#' @param inputfiles optional pre-filtered file catalog.
#' @param ... currently ignored.
#'
#' @return \code{SpatRaster} with time dimension, or tibble if \code{returnfiles = TRUE}.
#'
#' @seealso \code{\link{readice}} for legacy interface with \code{hemisphere = "both"} support,
#'   \code{\link{read_nsidc_ice_monthly}} for monthly data
#'
#' @export
#' @examples
#' \dontrun{
#' # Latest southern hemisphere ice
#' ice <- read_nsidc_ice_daily()
#'
#' # Specific date, northern hemisphere
#' ice_n <- read_nsidc_ice_daily("2023-03-15", hemisphere = "north")
#'
#' # Multiple dates
#' ice <- read_nsidc_ice_daily(c("2023-01-15", "2023-06-15"))
#'
#' # Without NA masking (keeps special values as raw integers)
#' ice_raw <- read_nsidc_ice_daily("2023-01-15", setNA = FALSE)
#' }
read_nsidc_ice_daily <- function(date,
                                  hemisphere = c("south", "north"),
                                  xylim = NULL,
                                  setNA = TRUE,
                                  latest = TRUE,
                                  returnfiles = FALSE,
                                  ...,
                                  inputfiles = NULL) {

  hemisphere <- match.arg(hemisphere)

  # --- file catalog ---
  files <- inputfiles %||% switch(hemisphere,
    south = raadfiles::nsidc_south_daily_files(),
    north = raadfiles::nsidc_north_daily_files()
  )

  # Filter known bad dates (placeholder files with no ice variable)
 # bad_nsidc is internal data: integer dates of dud files (< 40KB)
  rawdate <- as.integer(as.Date(files$date))
  files <- files[!(rawdate %in% bad_nsidc), ]

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

  # --- read ---
 # NSIDC v2 NetCDF - may have multiple sensor variables, take first (lyrs=1)
  # GDAL returns values scaled 0-1, multiply by 100 for percentage
  r <- terra::rast(files$fullname, lyrs = 1)
  r <- r * 100
  
  # Set CRS explicitly - should be in file but older files may lack it
  terra::crs(r) <- switch(hemisphere,
    south = "EPSG:3976",
    north = "EPSG:3413"
  )

  # --- setNA: mask special values ---
  if (setNA) {
    # Valid ice concentration is in (0, 100]
    # Mask: values > 100 (special codes: land, coast, missing, polar hole)
    # Mask: values <= 0 (no data / missing in original encoding)
    r <- terra::classify(r, matrix(c(100.001, Inf, NA,
                                      -Inf, 0.001, NA), ncol = 3, byrow = TRUE), right = FALSE)
  }

  terra::time(r) <- as.Date(files$date)
  names(r) <- format(files$date, "%Y-%m-%d")

  # --- crop ---
  if (!is.null(xylim)) {
    r <- terra::crop(r, terra::ext(xylim))
  }

  r
}


#' Read NSIDC monthly sea ice concentration
#'
#' Read NSIDC passive microwave sea ice concentration (25km) monthly data.
#'
#' @inheritParams read_nsidc_ice_daily
#'
#' @return \code{SpatRaster} with time dimension, or tibble if \code{returnfiles = TRUE}.
#'
#' @seealso \code{\link{read_nsidc_ice_daily}} for daily data
#'
#' @export
#' @examples
#' \dontrun{
#' # Latest monthly ice
#' ice <- read_nsidc_ice_monthly()
#' }
read_nsidc_ice_monthly <- function(date,
                                    hemisphere = c("south", "north"),
                                    xylim = NULL,
                                    setNA = TRUE,
                                    latest = TRUE,
                                    returnfiles = FALSE,
                                    ...,
                                    inputfiles = NULL) {

  hemisphere <- match.arg(hemisphere)

  # --- file catalog ---
 files <- inputfiles %||% switch(hemisphere,
    south = raadfiles::nsidc_south_monthly_files(),
    north = raadfiles::nsidc_north_monthly_files()
  )

  if (returnfiles) return(files)

  # --- date handling ---
  if (missing(date)) {
    date <- if (latest) max(files$date) else min(files$date)
  }
  date <- timedateFrom(date)
  files <- .processFiles(date, files, "monthly")

  if (nrow(files) == 0L) {
    stop("no files found for requested dates")
  }

  # --- read ---
  r <- terra::rast(files$fullname, lyrs = 1)
  r <- r * 100

  terra::crs(r) <- switch(hemisphere,
    south = "EPSG:3976",
   north = "EPSG:3413"
  )

  if (setNA) {
    r <- terra::classify(r, matrix(c(100.001, Inf, NA,
                                      -Inf, 0.001, NA), ncol = 3, byrow = TRUE), right = FALSE)
  }

  terra::time(r) <- as.Date(files$date)
  names(r) <- format(files$date, "%Y-%m")

  if (!is.null(xylim)) {
    r <- terra::crop(r, terra::ext(xylim))
  }

  r
}
