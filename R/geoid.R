#' Read Earth Gravitation Model
#'
#' Global 2.5 Minute Geoid Undulations, a big raster from multiple files. We just create a temporary
#' virtual raster from the source files, and return as a SpatRaster.
#'
#' Each file is an ESRI GRID raster data set of 2.5-minute geoid undulation values covering a 45 x 45 degree area.
#' Each raster file has a 2.5-minute cell size and is a subset of the global 2.5 x 2.5-minute grid of pre-computed
#' geoid undulation point values found on the EGM2008-WGS 84 Version web page. This ESRI GRID format represents a
#' continuous surface of geoid undulation values where each 2.5-minute raster cell derives its value from the original
#' pre-computed geoid undulation point value located at the SW corner of each cell.
#'
#' @param xylim an extent, in longlat, or an object that provides one
#' @param force if TRUE ignore cached virtual saved file, and recompute (try this if it otherwise fails)
#'
#' @return SpatRaster, longitude latitue grid of geoid level
#' @export
#' @examples
#' \dontrun{
#' geoid <- read_geoid()
#' }
read_geoid <- function(xylim = NULL, force = FALSE) {
  .shim_notice("read_geoid")
  tiles <- raadfiles::geoid_files()
  r <- terra::vrt(tiles$fullname, filename = tf <- tempfile(fileext = ".vrt"))
  if (!is.null(xylim)) r <- terra::crop(r, .as_ext(xylim))
  names(r) <- "EGM2008"
  r
}
