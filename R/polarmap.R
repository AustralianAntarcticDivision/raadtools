#' Polar map
#'
#' Spatial lines in the standard (71S) South Polar projection. Data is
#' the 'rnaturalearth ne_coastline' at 'scale = 10'. 
#' 
#' Use 'crs' to modify the projection, or set to 'NA' to leave in longlat. 
#' @param crs PROJ.4 string
#'
#' @return SpatVector of lines
#' @export
#'
#' @examples
#' plot(polar_map())
polar_map <- function(crs = commonprojections$polar) {
  ## pmap is stored wrapped. A SpatVector is an external pointer, so saving one
  ## into sysdata.rda gives back a dead handle on load - every method on it
  ## fails with "NULL value passed as symbol address". terra::wrap() turns it
  ## into a PackedSpatVector, which is plain data and serialises fine.
  out <- terra::unwrap(pmap)
  if (is.na(crs)) return(out)
  terra::project(out, crs)
}
