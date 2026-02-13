# R/read-oisst.R
# Terra-native OISST readers - Layer 1 of the new architecture

#' @noRd
`%||%` <- function(a, b) if (is.null(a)) b else a

#' Read OISST daily sea surface temperature
#'
#' Read NOAA Optimum Interpolation Sea Surface Temperature (OISST) v2.1 daily data.
#'
#' @details
#' OISST files contain four variables:
#' \itemize{
#'   \item \code{sst} - sea surface temperature (Celsius)
#'   \item \code{anom} - SST anomaly from climatology (Celsius)
#'   \item \code{err} - estimated error standard deviation (Celsius)
#'   \item \code{ice} - sea ice concentration (percent)
#' }
#'
#' Data are on a global 0.25 degree grid (1440x720). Source files use Pacific-centered
#' longitude (0-360); use \code{lon180 = TRUE} (default) to convert to
#' Atlantic-centered (-180 to 180).
#'
#' Date matching uses a 1.5-day tolerance window - if no file exists within
#' 1.5 days of a requested date, an error is raised.
#'
#' @param date date or dates to read (character, Date, or POSIXct). If missing,
#'   returns earliest or latest depending on \code{latest} argument.
#' @param xylim extent to crop, anything acceptable to \code{terra::ext()}.
#'   Applied after rotation if \code{lon180 = TRUE}.
#' @param lon180 logical, convert from Pacific (0,360) to Atlantic (-180,180) view?
#'   Default \code{TRUE}.
#' @param varname variable to read: one of "sst", "anom", "err", "ice".
#' @param latest if \code{TRUE} and \code{date} missing, return latest available;
#'   if \code{FALSE}, return earliest.
#' @param returnfiles if \code{TRUE}, return the file catalog instead of reading data.
#' @param inputfiles optionally provide a pre-filtered file catalog (tibble with
#'   \code{date} and \code{fullname} columns).
#' @param ... currently ignored.
#'
#' @return \code{SpatRaster} with time dimension set to file dates, or tibble if
#'   \code{returnfiles = TRUE}.
#'
#' @seealso \code{\link{readsst}} for legacy interface (returns raster objects)
#'
#' @export
#' @examples
#' \dontrun{
#' # latest SST
#' sst <- read_oisst_daily()
#'
#' # specific dates
#' sst <- read_oisst_daily(c("2023-01-15", "2023-06-15"))
#'
#' # SST anomaly, cropped to Southern Ocean
#' anom <- read_oisst_daily("2023-01-15", varname = "anom",
#'                          xylim = c(-180, 180, -80, -40))
#'
#' # just get the file list
#' files <- read_oisst_daily(returnfiles = TRUE)
#' }
read_oisst_daily <- function(date,
                              xylim = NULL,
                              lon180 = TRUE,
                              varname = c("sst", "anom", "err", "ice"),
                              latest = TRUE,
                              returnfiles = FALSE,
                              ...,
                              inputfiles = NULL) {

  varname <- match.arg(varname)

  # --- file catalog ---
  files <- inputfiles %||% raadfiles::oisst_daily_files()

  if (returnfiles) return(files)

  # --- date handling via existing infrastructure ---
  if (missing(date)) {
    date <- if (latest) max(files$date) else min(files$date)
  }
  date <- timedateFrom(date)

 # .processFiles handles: validation, sorting, fuzzy matching, deduplication
  files <- .processFiles(date, files, "daily")

  if (nrow(files) == 0L) {
    stop("no files found for requested dates")
  }

  # --- read ---
 # OISST files are non-compliant netCDF (no grid_mapping), set CRS explicitly
  r <- terra::rast(files$fullname, subds = varname)
  terra::crs(r) <- "EPSG:4326"
  terra::time(r) <- as.Date(files$date)
  names(r) <- format(files$date, "%Y-%m-%d")

  # --- transforms ---
  if (lon180) {
    r <- terra::rotate(r)
  }

  if (!is.null(xylim)) {
    r <- terra::crop(r, terra::ext(xylim))
  }

  r
}


#' Read OISST monthly sea surface temperature
#'
#' Read NOAA Optimum Interpolation Sea Surface Temperature (OISST) v2.1 monthly data.
#'
#' @inheritParams read_oisst_daily
#'
#' @details
#' Monthly files contain the same variables as daily files. Date matching uses
#' a 28-day tolerance window.
#'
#' @return \code{SpatRaster} with time dimension set to file dates, or tibble if
#'   \code{returnfiles = TRUE}.
#'
#' @seealso \code{\link{read_oisst_daily}} for daily data
#'
#' @export
#' @examples
#' \dontrun
#' # latest monthly SST
#' sst <- read_oisst_monthly()
#'
#' # specific months
#' sst <- read_oisst_monthly(c("2023-01-01", "2023-06-01"))
#' }
read_oisst_monthly <- function(date,
                                xylim = NULL,
                                lon180 = TRUE,
                                varname = c("sst", "anom", "err", "ice"),
                                latest = TRUE,
                                returnfiles = FALSE,
                                ...,
                                inputfiles = NULL) {

  varname <- match.arg(varname)

  # --- file catalog ---
  files <- inputfiles %||% raadfiles::oisst_monthly_files()

  if (returnfiles) return(files)

  # --- date handling ---
  if (missing(date)) {
    date <- if (latest) max(files$date) else min(files$date)
  }
  date <- timedateFrom(date)

  # .processFiles with monthly tolerance (28 days)
  files <- .processFiles(date, files, "monthly")

  if (nrow(files) == 0L) {
    stop("no files found for requested dates")
  }

  # --- read ---
  r <- terra::rast(files$fullname, subds = varname)
  terra::crs(r) <- "EPSG:4326"
  terra::time(r) <- as.Date(files$date)
  names(r) <- format(files$date, "%Y-%m")

  # --- transforms ---
  if (lon180) {
    r <- terra::rotate(r)
  }

  if (!is.null(xylim)) {
    r <- terra::crop(r, terra::ext(xylim))
  }

  r
}
