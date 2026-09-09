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
#' Values are sea ice concentration as a fraction (0-1), as the files store
#' them. When \code{setNA = TRUE}, any residual flag value (land, coast,
#' missing, polar hole) is masked to NA; open water is a legitimate 0 and is
#' kept.
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
  # GDAL applies the file's scale factor, so values arrive as a fraction 0-1,
  # which is what we return.
  r <- .rast_nc(files$fullname, lyrs = 1)

  # Set CRS explicitly - should be in file but older files may lack it
  terra::crs(r) <- switch(hemisphere,
    south = "EPSG:3976",
    north = "EPSG:3413"
  )

  # --- setNA: mask special values ---
  if (setNA) {
    # Valid ice concentration is [0, 1]. Flag codes (land, coast, missing,
    # polar hole) sit above 1. Nothing is masked at the bottom: 0 is open
    # water, not missing data.
    r <- terra::classify(r, matrix(c(1.0001, Inf, NA), ncol = 3, byrow = TRUE),
                         right = FALSE)
  }

  terra::time(r) <- as.Date(files$date)
  names(r) <- format(files$date, "%Y-%m-%d")

  # --- crop ---
  if (!is.null(xylim)) {
    r <- terra::crop(r, terra::ext(xylim))
  }

  r
}


#' Read NSIDC CDR daily sea ice concentration
#'
#' Read the NOAA/NSIDC Climate Data Record of passive microwave sea ice
#' concentration (G02202 V6, 25km) for a single hemisphere.
#'
#' @details
#' The CDR is what \code{\link{readice}} and \code{\link{icefiles}} reach for
#' by default. \code{\link{read_nsidc_ice_daily}} reads NSIDC-0051 v2 instead,
#' which is the historical default.
#'
#' A CDR file holds eight subdatasets and concentration is not the first of
#' them - band 1 is \code{cdr_seaice_conc_interp_spatial_flag}. This reader
#' addresses \code{cdr_seaice_conc} by name, so a positional read cannot
#' silently return a flag layer.
#'
#' Values are sea ice concentration as a fraction (0-1). When
#' \code{setNA = TRUE}, any residual flag value (above 1) is masked; open
#' water is a legitimate 0 and is kept.
#'
#' The projection is taken from the file. The CDR grids are the same 25km polar
#' stereographic grids as NSIDC-0051, and only if the file declares no CRS at
#' all does this reader fall back to EPSG:3976 (south) or EPSG:3413 (north).
#'
#' @inheritParams read_nsidc_ice_daily
#'
#' @return \code{SpatRaster} with time dimension, or tibble if \code{returnfiles = TRUE}.
#'
#' @seealso \code{\link{read_nsidc_ice_daily}} for NSIDC-0051 v2,
#'   \code{\link{icefiles}} for the file listing
#'
#' @export
#' @examples
#' \dontrun{
#' ice <- read_nsidc_cdr_daily()
#' ice_n <- read_nsidc_cdr_daily("2023-03-15", hemisphere = "north")
#' }
read_nsidc_cdr_daily <- function(date,
                                 hemisphere = c("south", "north"),
                                 xylim = NULL,
                                 setNA = TRUE,
                                 latest = TRUE,
                                 returnfiles = FALSE,
                                 ...,
                                 inputfiles = NULL) {

  hemisphere <- match.arg(hemisphere)

  files <- inputfiles %||% icefiles(product = "cdr", hemisphere = hemisphere)

  if (returnfiles) return(files)

  if (missing(date)) {
    date <- if (latest) max(files$date) else min(files$date)
  }
  date <- timedateFrom(date)
  files <- .processFiles(date, files, "daily")

  if (nrow(files) == 0L) {
    stop("no files found for requested dates")
  }

  ## address the concentration subdataset by name, not by position
  r <- .rast_nc(.cdr_conc_dsn(files$fullname))

  if (!nzchar(terra::crs(r))) {
    terra::crs(r) <- switch(hemisphere,
      south = "EPSG:3976",
      north = "EPSG:3413"
    )
  }

  if (setNA) {
    r <- terra::classify(r, matrix(c(1.0001, Inf, NA), ncol = 3, byrow = TRUE),
                         right = FALSE)
  }

  terra::time(r) <- as.Date(files$date)
  names(r) <- format(files$date, "%Y-%m-%d")

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
  r <- .rast_nc(files$fullname, lyrs = 1)

  terra::crs(r) <- switch(hemisphere,
    south = "EPSG:3976",
   north = "EPSG:3413"
  )

  if (setNA) {
    r <- terra::classify(r, matrix(c(1.0001, Inf, NA), ncol = 3, byrow = TRUE),
                         right = FALSE)
  }

  terra::time(r) <- as.Date(files$date)
  names(r) <- format(files$date, "%Y-%m")

  if (!is.null(xylim)) {
    r <- terra::crop(r, terra::ext(xylim))
  }

  r
}
