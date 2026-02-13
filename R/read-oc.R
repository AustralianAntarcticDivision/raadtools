# R/read-oc.R
# Terra-native NASA Ocean Colour readers
#
# Multi-sensor support: SeaWiFS (1997-2010), MODISA (2002+), VIIRS (2012+)
# Products: chlorophyll-a (CHL), PAR
# Temporal: daily, 8-day (weekly), monthly

# =============================================================================
# Multi-era file helpers (kept from chla.R)
# =============================================================================

.multi_era_ocfiles <- function(time.resolution = "daily", varname = "CHL") {
  f1 <- try(ocfiles(time.resolution, product = "MODISA", varname = varname, type = "L3m"), silent = TRUE)
  f2 <- try(ocfiles(time.resolution, product = "VIIRS", varname = varname, type = "L3m"), silent = TRUE)
  f3 <- try(ocfiles(time.resolution, product = "SeaWiFS", varname = varname, type = "L3m"), silent = TRUE)
  
  files <- NULL
  if (!inherits(f1, "try-error")) files <- rbind(files, f1)
  if (!inherits(f2, "try-error")) files <- rbind(files, f2)
  if (!inherits(f3, "try-error")) files <- rbind(files, f3)
  
  if (is.null(files)) stop("no ocean colour files found!")
  
  dplyr::arrange(dplyr::distinct(files, date, .keep_all = TRUE), date)
}


# =============================================================================
# Core internal reader
# =============================================================================

.read_oc_l3m <- function(date,
                          time.resolution,
                          varname,
                          subds,
                          xylim = NULL,
                          lon180 = TRUE,
                          latest = TRUE,
                          returnfiles = FALSE,
                          inputfiles = NULL) {

  files <- inputfiles %||% .multi_era_ocfiles(time.resolution, varname = varname)

  if (returnfiles) return(files)

  if (missing(date)) {
    date <- if (latest) max(files$date) else min(files$date)
  }
  date <- timedateFrom(date)
  files <- .processFiles(date, files, time.resolution)

  if (nrow(files) == 0L) {
    stop("no files found for requested dates")
  }

  # Read each file
  read_one <- function(f) {
    r <- terra::rast(f, subds = subds)
    # Source files have slight extent noise, standardize
    terra::ext(r) <- terra::ext(-180, 180, -90, 90)
    r
  }

  out <- terra::rast(lapply(files$fullname, read_one))

  # Handle lon180 - NASA L3m is typically -180 to 180 already
  # but check and rotate if needed
  if (!lon180) {
    ext <- as.vector(terra::ext(out))
    if (ext[1] < 0) {
      out <- terra::rotate(out, left = FALSE)
    }
  }

  # Crop if requested
  if (!is.null(xylim)) {
    out <- terra::crop(out, terra::ext(xylim))
  }

  # Set CRS and time
  if (is.na(terra::crs(out, proj = TRUE))) {
    terra::crs(out) <- "EPSG:4326"
  }
  terra::time(out) <- as.Date(files$date)
  names(out) <- format(files$date, "%Y-%m-%d")

  out
}


# =============================================================================
# Chlorophyll-a readers
# =============================================================================

#' Read ocean colour chlorophyll-a (daily)
#'
#' Read NASA ocean colour chlorophyll-a from L3m (mapped) products.
#' Combines data from SeaWiFS (1997-2010), MODISA (2002+), and VIIRS (2012+)
#' into a continuous time series.
#'
#' @details
#' Data are 4km global mapped products from NASA Ocean Biology Processing Group.
#' When sensor eras overlap, one observation per date is returned (typically MODISA).
#'
#' To read from a specific sensor only, use \code{ocfiles()} to get the file
#' catalog and pass it via \code{inputfiles}.
#'
#' @param date date or dates to read (character, Date, or POSIXct).
#' @param xylim extent to crop, anything acceptable to \code{terra::ext()}.
#' @param lon180 logical, use Atlantic-centered longitude? Default TRUE.
#' @param varname subdataset to read, default "chlor_a".
#' @param latest if TRUE and date missing, return latest; if FALSE, earliest.
#' @param returnfiles if TRUE, return file catalog instead of data.
#' @param inputfiles optional pre-filtered file catalog (from \code{ocfiles()}).
#' @param ... currently ignored.
#'
#' @return \code{SpatRaster} with time dimension set.
#'
#' @seealso \code{\link{ocfiles}} for file catalog,
#'   \code{\link{read_oc_chl_8day}}, \code{\link{read_oc_chl_monthly}}
#'
#' @export
#' @examples
#' \dontrun{
#' # Latest chlorophyll
#' chl <- read_oc_chl_daily()
#'
#' # Earliest (should be SeaWiFS era)
#' chl_sw <- read_oc_chl_daily(latest = FALSE)
#'
#' # Specific sensor only
#' modis_files <- ocfiles("daily", product = "MODISA", varname = "CHL", type = "L3m")
#' chl_modis <- read_oc_chl_daily("2020-01-15", inputfiles = modis_files)
#' }
read_oc_chl_daily <- function(date,
                               xylim = NULL,
                               lon180 = TRUE,
                               varname = "chlor_a",
                               latest = TRUE,
                               returnfiles = FALSE,
                               ...,
                               inputfiles = NULL) {
  .read_oc_l3m(
    date = date,
    time.resolution = "daily",
    varname = "CHL",
    subds = varname,
    xylim = xylim,
    lon180 = lon180,
    latest = latest,
    returnfiles = returnfiles,
    inputfiles = inputfiles
  )
}


#' @rdname read_oc_chl_daily
#' @export
read_oc_chl_8day <- function(date,
                              xylim = NULL,
                              lon180 = TRUE,
                              varname = "chlor_a",
                              latest = TRUE,
                              returnfiles = FALSE,
                              ...,
                              inputfiles = NULL) {
  .read_oc_l3m(
    date = date,
    time.resolution = "weekly",  # "weekly" maps to 8D in ocfiles()
    varname = "CHL",
    subds = varname,
    xylim = xylim,
    lon180 = lon180,
    latest = latest,
    returnfiles = returnfiles,
    inputfiles = inputfiles
  )
}


#' @rdname read_oc_chl_daily
#' @export
read_oc_chl_monthly <- function(date,
                                 xylim = NULL,
                                 lon180 = TRUE,
                                 varname = "chlor_a",
                                 latest = TRUE,
                                 returnfiles = FALSE,
                                 ...,
                                 inputfiles = NULL) {
  .read_oc_l3m(
    date = date,
    time.resolution = "monthly",
    varname = "CHL",
    subds = varname,
    xylim = xylim,
    lon180 = lon180,
    latest = latest,
    returnfiles = returnfiles,
    inputfiles = inputfiles
  )
}


# =============================================================================
# PAR reader
# =============================================================================

#' Read ocean colour PAR (8-day)
#'
#' Read Photosynthetically Available Radiation from NASA MODISA L3m products.
#'
#' @inheritParams read_oc_chl_daily
#'
#' @return \code{SpatRaster} with time dimension set.
#'
#' @seealso \code{\link{read_oc_chl_daily}}
#'
#' @export
read_oc_par_8day <- function(date,
                              xylim = NULL,
                              lon180 = TRUE,
                              latest = TRUE,
                              returnfiles = FALSE,
                              ...,
                              inputfiles = NULL) {

  files <- inputfiles %||% raadfiles::par_files(time.resolution = "8D")

  if (returnfiles) return(files)

  if (missing(date)) {
    date <- if (latest) max(files$date) else min(files$date)
  }
  date <- timedateFrom(date)
  files <- .processFiles(date, files, "8D")

  if (nrow(files) == 0L) {
    stop("no files found for requested dates")
  }

  read_one <- function(f) {
    r <- terra::rast(f, subds = "par")
    terra::ext(r) <- terra::ext(-180, 180, -90, 90)
    r
  }

  out <- terra::rast(lapply(files$fullname, read_one))

  if (!lon180) {
    ext <- as.vector(terra::ext(out))
    if (ext[1] < 0) {
      out <- terra::rotate(out, left = FALSE)
    }
  }

  if (!is.null(xylim)) {
    out <- terra::crop(out, terra::ext(xylim))
  }

  if (is.na(terra::crs(out, proj = TRUE))) {
    terra::crs(out) <- "EPSG:4326"
  }
  terra::time(out) <- as.Date(files$date)
  names(out) <- format(files$date, "%Y-%m-%d")

  out
}
