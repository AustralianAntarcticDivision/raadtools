#' Polar map
#'
#' Spatial lines in the standard (71S) South Polar projection. Data is
#' the 'rnaturalearth ne_coastline' at 'scale = 10'. 
#' 
#' Use 'crs' to modify the projection, or set to 'NA' to leave in longlat. 
#' @param crs PROJ.4 string
#'
#' @return SpatialLinesDataFrame
#' @export
#'
#' @examples
#' plot(polar_map())
polar_map <- function(crs = commonprojections$polar) {
  if (is.na(crs)) return(pmap)
  sp::spTransform(pmap, crs)
}