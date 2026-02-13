# R/read-topo.R
# Terra-native topography/bathymetry readers
#
# Dispatcher pattern for ~20 static datasets

#' Read topography or bathymetry data
#'
#' Read data from various topography/bathymetry sources.
#'
#' @details
#' The following datasets are available via the \code{topo} argument:
#'
#' \describe{
#'   \item{gebco_23}{GEBCO 2023 grid (ice surface), 15 arc-second global}
#'   \item{gebco_21}{GEBCO 2021 grid}
#'   \item{gebco_19}{GEBCO 2019 grid}
#'   \item{gebco_14}{GEBCO 2014 grid}
#'   \item{gebco_08}{GEBCO 2008 grid, 30 arc-second}
#'   \item{etopo1}{ETOPO1 1 arc-minute global relief}
#'   \item{etopo2}{ETOPO2 (historic, deprecated)}
#'   \item{ibcso, ibcso_is}{IBCSO v2 ice surface, 500m Antarctic}
#'   \item{ibcso_bed}{IBCSO v2 bedrock}
#'   \item{smith_sandwell}{Global seafloor from satellite altimetry}
#'   \item{kerguelen}{Kerguelen Plateau bathymetric grid}
#'   \item{cryosat2}{CryoSat-2 Antarctic DEM}
#'   \item{ramp}{RADARSAT Antarctic DEM v2}
#'   \item{rema_8m, rema_100m, rema_200m, rema_1km}{REMA Antarctic DEMs}
#'   \item{lake_superior}{Lake Superior bathymetry}
#'   \item{macrie1100m, macrie2100m}{Macquarie Ridge 100m grids}
#' }
#'
#' @param topo character, dataset name (see Details)
#' @param xylim extent to crop, or SpatRaster template for warping.
#'   Accepts anything \code{terra::ext()} understands: numeric vector
#'   \code{c(xmin, xmax, ymin, ymax)}, SpatExtent, or SpatRaster.
#'   If a SpatRaster with CRS is provided, data is warped to match.
#' @param lon180 logical, return Atlantic-centered [-180,180] view? Default TRUE.
#' @param polar logical, for IBCSO return polar stereographic version?
#' @param resample character, resampling method for warping (default "bilinear")
#' @param returnfiles logical, return file path instead of data?
#' @param ... currently ignored
#'
#' @return \code{SpatRaster}, or character file path if \code{returnfiles = TRUE}
#'
#' @seealso \code{\link{topofile}} for file paths only
#'
#' @export
#' @examples
#' \dontrun
#' # Read GEBCO 2023
#' bathy <- read_topo("gebco_23")
#'
#' # Crop to region
#' bathy <- read_topo("gebco_23", xylim = c(100, 150, -60, -40))
#'
#' # Warp to template raster
#' template <- terra::rast(nrows = 100, ncols = 100,
#'                         xmin = 100, xmax = 150, ymin = -60, ymax = -40,
#'                         crs = "EPSG:4326")
#' bathy <- read_topo("gebco_23", xylim = template)
#' }
read_topo <- function(topo = c("gebco_23", "gebco_21", "gebco_19", "gebco_14", "gebco_08",
                               "etopo1", "etopo2",
                               "ibcso", "ibcso_is", "ibcso_bed",
                               "smith_sandwell",
                               "kerguelen",
                               "cryosat2",
                               "ramp",
                               "rema_8m", "rema_100m", "rema_200m", "rema_1km",
                               "lake_superior",
                               "macrie1100m", "macrie2100m"),
                      xylim = NULL,
                      lon180 = TRUE,
                      polar = FALSE,
                      resample = "bilinear",
                      returnfiles = FALSE,
                      ...) {

 topo <- match.arg(topo)

  # Handle ibcso alias
 if (topo == "ibcso") topo <- "ibcso_is"

  # Get file path (may be VRT string)
  tfile <- topofile(topo = topo, polar = polar, lon180 = lon180, ...)

  if (returnfiles) return(tfile)

  # Special handling for rema_8m with non-SpatRaster xylim
 if (topo == "rema_8m" && !is.null(xylim) && !inherits(xylim, "SpatRaster")) {
    stop("'xylim' must be a SpatRaster template for rema_8m (too large for simple crop)")
  }

  # Read the data
 r <- terra::rast(tfile)

  # Set CRS if missing but looks like lonlat
 if (is.na(terra::crs(r, proj = TRUE))) {
    ext <- as.vector(terra::ext(r))
    if (ext[1] >= -180 && ext[2] <= 360 && ext[3] >= -90 && ext[4] <= 90) {
      terra::crs(r) <- "OGC:CRS84"
    }
  }

  # Handle lon180 rotation for Pacific-centered sources
  if (!lon180 && topo %in% c("gebco_08", "ibcso_is", "ibcso_bed", "etopo2")) {
    r <- terra::rotate(r)
  }

  # Handle xylim: either crop or warp
  if (!is.null(xylim)) {
    if (inherits(xylim, "SpatRaster")) {
      # Warp to template
      if (is.na(terra::crs(xylim))) {
        stop("cannot use xylim SpatRaster template without CRS")
      }
      if (is.na(terra::crs(r))) {
        stop(sprintf("cannot warp topo '%s' - source has no CRS", topo))
      }
      r <- terra::project(r, xylim, method = resample, by_util = TRUE)
    } else {
      # Crop to extent
      r <- terra::crop(r, terra::ext(xylim))
    }
  }

  r
}


#' @rdname read_topo
#' @export
read_bathy <- read_topo


# =============================================================================
# Legacy shims
# =============================================================================

#' @rdname read_topo
#' @export
readtopo <- function(topo = c("gebco_23", "gebco_21", "gebco_19", "gebco_14", "gebco_08",
                              "etopo1", "etopo2",
                              "ibcso", "ibcso_is", "ibcso_bed",
                              "smith_sandwell",
                              "kerguelen",
                              "cryosat2",
                              "ramp",
                              "rema_8m", "rema_100m", "rema_200m", "rema_1km",
                              "lake_superior",
                              "macrie1100m", "macrie2100m"),
                     xylim = NULL,
                     lon180 = TRUE,
                     polar = FALSE,
                     resample = "bilinear",
                     returnfiles = FALSE,
                     ...) {

  topo <- match.arg(topo)

  if (isTRUE(getOption("raadtools.shim.warn", TRUE))) {
    .Deprecated("read_topo", package = "raadtools",
      msg = paste0(
        "'readtopo' is deprecated. ",
        "Use 'read_topo' for terra-native output.\n",
        "Set options(raadtools.shim.warn = FALSE) to suppress this warning."
      ))
  }

 # Handle legacy raster/extent inputs for xylim
  if (!is.null(xylim)) {
    if (inherits(xylim, "BasicRaster")) {
      xylim <- terra::rast(xylim)
    } else if (inherits(xylim, "Extent")) {
      xylim <- as.vector(xylim)
    }
  }

  r <- read_topo(
    topo = topo,
    xylim = xylim,
    lon180 = lon180,
    polar = polar,
    resample = resample,
    returnfiles = returnfiles,
    ...
  )

  if (returnfiles) return(r)

  # Convert to raster for backward compat
  raster::raster(r)
}


#' @rdname read_topo
#' @export
readbathy <- readtopo
