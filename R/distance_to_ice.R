
#' Distance to a sea ice 'edge'. 
#' 
#' Calculate the shortest distance (metres) to a threshold sea ice contour.  If in
#' doubt use `distance_to_ice_edge`, the definition of the edge is not straightforward, especially so for
#' the higher resolution products and near the coast. 
#' `distance_to_ice_edge` computes a single "main" edge at continental scale
#' `distance_to_ice` computes all distances to any ice at threshold concentration
#' 
#' The distance is always positive, use `readice` in the usual way to determine if a
#' location is inside or out of the ice field itself. (If inside means zero distance to ice
#' for you then set it explicitly based on the concentration a point is in.)
#' Future work may generalize this to other data sources. 
#' @inheritParams readice
#' @param threshold the sea ice concentration threshold to contour at
#' @param ... passed to `readice`, e.g. `hemisphere`
#' @return raster layer with distances to this date's sea ice edge
#' @export
#' @note beware that any queried location outside of this layer's range will be 
#' undetermined, and the external boundary of this layer is not constant with 
#' respect to the pole, and that in general a location may be closer to ice in the 
#' opposite hemisphere. 
#' 
#' The argument `hemisphere` may be north or south (default is south), but this will only work if your locations
#' are on the actual map, so it's not possible to request the distance to ice in both poles for any point. 
#' @examples
#' plot(distance_to_ice(latest = TRUE))
#' plot(distance_to_ice_edge(latest = TRUE))
#' a = extract(distance_to_ice, aurora[17:25, ])
#' extract(distance_to_ice, aurora[17:25, ], hemisphere = "south")
#' # library(trip)
#' # extract(distance_to_ice_edge, walrus818[seq(50, 400, by = 20), ], hemisphere = "north")
distance_to_ice_edge <- function(date, threshold = 15, ..., returnfiles = FALSE, inputfiles = NULL) {
  if (returnfiles) return(icefiles(...))
  if (!missing(date) && length(date) > 1L) {
    warning("'date' should be of length = 1, using first supplied")
    date <- date[1L]
  }
  distance_to_ice_edge0(date, threshold = threshold, returnfiles = FALSE, inputfiles = inputfiles, ...)
}

distance_to_ice_edge0 <- function(date, threshold = 15, ..., returnfiles = FALSE, inputfiles = NULL) {
  ice <- readice(date, ..., inputfiles = inputfiles, setNA = FALSE)
  cl <- keepOnlyMostComplexLine(rasterToContour(ice, levels = threshold))
  pp <- rgdal::project(coordinates(ice), projection(ice), inv = TRUE)
  pcl <- coordinates(as(cl, "SpatialPointsDataFrame"))
  raster::setZ(raster::distanceFromPoints(ice, pcl), timedateFrom(date))
}  
#' @name distance_to_ice_edge
#' @export
distance_to_ice <- function(date, threshold = 15, ..., returnfiles = FALSE, inputfiles = NULL) {
  if (returnfiles) return(icefiles())
  if (!missing(date) && length(date) > 1L) {
    warning("'date' should be of length = 1, using first supplied")
    date <- date[1L]
  }
  ice <- readice(date, ..., inputfiles = inputfiles, setNA = FALSE)
  cl <- rasterToContour(ice, levels = threshold)
  pp <- rgdal::project(coordinates(ice), projection(ice), inv = TRUE)
  pcl <- coordinates(as(cl, "SpatialPointsDataFrame"))
  raster::setZ(raster::distanceFromPoints(ice, pcl), timedateFrom( date))
}