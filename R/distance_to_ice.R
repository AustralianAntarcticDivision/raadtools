 
keepOnlyMostComplexLine <- function(x) {
  for (iObj in seq_len(nrow(x))) {
    if (inherits(x, "SpatialLinesDataFrame")) {
      wmax <- which.max(sapply(x[iObj, ]@lines[[1]]@Lines, function(x) nrow(x@coords)))
      x@lines[[iObj]]@Lines <- x@lines[[iObj]]@Lines[wmax]
    }
    if (inherits(x, "SpatialPolygonsDataFrame")) {
      wmax <- which.max(sapply(x[iLine, ]@lines[[1]]@Lines, function(x) nrow(x@coords)))
      x@lines[[iLine]]@Lines <- x@lines[[iLine]]@Lines[wmax]
    }
  }
  x
}

#' Distance to a sea ice 'edge'. 
#' 
#' Calculate the shortest distance (metres) to a threshold sea ice contour.  If in
#' doubt use `distance_to_ice_edge`, the definition of the edge is not straightforward, especially so for
#' the higher resolution prducts and near the coast. 
#' `distance_to_ice_edge` computes a single "main" edge at continental scale
#' `distance_to_ice` computes all distances to any ice at threshold concentration
#' 
#' The distance is always positive, use `readice` in the usual way to determine if a
#' location is inside or out of the ice field itself. (If inside means zero distance to ice
#' for you then set it explicitly based on the concentration a point is in.)
#' Future work may generalize this to other data sources. 
#' @inheritParams readice
#' @param threshold the sea ice concentration threshold to contour at
#' @param ... passed to `readice`
#' @return raster layer with distances to this date's sea ice edge
#' @export
#' @note beware that any queried location outside of this layer's range will be 
#' undetermined, and the external boundary of this layer is not constant with 
#' respect to the pole, and that in general a location may be closer to ice in the 
#' opposite hemisphere. 
#' @examples
#' plot(distance_to_ice(latest = TRUE))
#' plot(distance_to_ice_edge(latest = TRUE))
#' a = extract(distance_to_ice, aurora[17:25, ])
distance_to_ice_edge <- function(date, threshold = 15, ..., returnfiles = FALSE, inputfiles = NULL) {
  if (returnfiles) return(icefiles())
  ice <- readice(date, ..., inputfiles = inputfiles, setNA = FALSE)
  cl <- keepOnlyMostComplexLine(rasterToContour(ice, levels = threshold))
  pp <- rgdal::project(coordinates(ice), projection(ice), inv = TRUE)
  pcl <- coordinates(as(cl, "SpatialPointsDataFrame"))
  raster::distanceFromPoints(ice, pcl)
}  
#' @name distance_to_ice_edge
#' @export
distance_to_ice <- function(date, threshold = 15, ..., returnfiles = FALSE, inputfiles = NULL) {
  if (returnfiles) return(icefiles())
  ice <- readice(date, ..., inputfiles = inputfiles, setNA = FALSE)
  cl <- rasterToContour(ice, levels = threshold)
  pp <- rgdal::project(coordinates(ice), projection(ice), inv = TRUE)
  pcl <- coordinates(as(cl, "SpatialPointsDataFrame"))
  raster::distanceFromPoints(ice, pcl)
}