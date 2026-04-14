# R/read-mld.R
# Terra-native MLD (Mixed Layer Depth) climatology reader
#
# Sallée et al. 2013 Southern Ocean MLD climatology
# 12 monthly layers

#' Read Mixed Layer Depth climatology
#'
#' Read the Sallée et al. (2013) Southern Ocean Mixed Layer Depth climatology.
#'
#' @details
#' This is a monthly climatology (12 layers, one per month) covering the
#' Southern Ocean south of 30°S. Data represent typical mixed layer depth
#' for each month, not actual dates.
#'
#' When a date is provided, the corresponding month is extracted. Multiple
#' dates will return multiple layers (one per unique month).
#'
#' @param date date(s) to select month from. Default returns all 12 months.
#' @param xylim extent to crop, anything acceptable to \code{terra::ext()}.
#' @param returnfiles if TRUE, return file info instead of data.
#' @param ... currently ignored.
#'
#' @return \code{SpatRaster} with one layer per month requested.
#'
#' @references
#' Sallée, J.-B., et al. (2013). Assessment of Southern Ocean mixed-layer
#' depths in CMIP5 models: Historical bias and forcing response.
#' Journal of Geophysical Research: Oceans, 118.
#'
#' @export
#' @examples
#' \dontrun{
#' # All 12 months
#' mld <- read_mld_climatology()
#'
#' # Just January
#' mld_jan <- read_mld_climatology("2020-01-15")
#'
#' # Summer months
#' mld_summer <- read_mld_climatology(c("2020-12-01", "2020-01-01", "2020-02-01"))
#' }
read_mld_climatology <- function(date,
                                  xylim = NULL,
                                  returnfiles = FALSE,
                                  ...) {

  # Find the file
  f <- raadfiles::get_raad_filenames(all = TRUE) %>%
    dplyr::filter(stringr::str_detect(file, "sallee_mld2013.Rdata"))

  if (nrow(f) == 0) {
    stop("MLD climatology file not found")
  }

  p <- file.path(f$root[1], f$file[1])

  # returnfiles - construct a pseudo file table
  if (returnfiles) {
    files <- tibble::tibble(
      fullname = p,
      date = seq(as.Date("2013-01-01"), by = "1 month", length.out = 12L),
      month = month.abb
    )
    return(files)
  }

  # Load the RasterBrick from .Rdata
  mld <- NULL
  load(p)  # loads object named 'mld'

  if (is.null(mld)) {
    stop("Failed to load MLD data from ", p)
  }

  # Convert to terra
  out <- terra::rast(mld)
  names(out) <- month.abb

  # Handle date selection
  if (missing(date)) {
    # Return all 12 months
    months_requested <- month.abb
  } else {
    date <- timedateFrom(date)
    months_requested <- unique(format(date, "%b"))
  }

  # Subset to requested months
  out <- out[[months_requested]]

  # Crop if requested
  if (!is.null(xylim)) {
    out <- terra::crop(out, terra::ext(xylim))
  }

  # Set CRS if missing
 if (is.na(terra::crs(out, proj = TRUE))) {
    terra::crs(out) <- "EPSG:4326"
  }

  out
}


# =============================================================================
# Legacy shim
# =============================================================================

#' @rdname read_mld_climatology
#' @export
readmld <- function(date,
                    xylim = NULL,
                    returnfiles = FALSE,
                    ...) {

  if (isTRUE(getOption("raadtools.shim.warn", TRUE))) {
    .Deprecated("read_mld_climatology", package = "raadtools",
      msg = paste0(
        "'readmld' is deprecated. ",
        "Use 'read_mld_climatology' for terra-native output.\n",
        "Set options(raadtools.shim.warn = FALSE) to suppress this warning."
      ))
  }

  r <- read_mld_climatology(
    date = date,
    xylim = xylim,
    returnfiles = returnfiles,
    ...
  )

  if (returnfiles) return(r)

  warning("MLD data is only a climatology, returning matching month only")

  # Convert to raster for backward compat
  raster::brick(r)
}
