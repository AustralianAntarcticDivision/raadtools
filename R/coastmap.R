##' Coastline data set as SpatialPolygons, currently not supported. 
##'
##' This function reads and returns a coastline polygon data set, either from the
##' source or serialized cache (.Rdata). Data source locations are controlled by options.
##'
##' The following named data sets were available.
##'
##' "world", "world2" - Atlantic and Pacific versions of maptools wrld_simpl ("world1", "world360" are aliases)
##' "ant_coast", "ant_coast01", "ant_coast10" - AAD Antartic coast in full, "1 mill", and "10 mill" resolution ("cst00_polygon", "cst01_polygon", and "cst10_polygon" are aliases)
##' "Countries_hires" - the "CIA" world map exported from the Manifold GIS data set
##'
##' @title Coast map
##' @param map A named map source
##' @param \dots arguments passed to worker functions
##' @return  SpatialPolygonsDataFrame or SpatialPolygons (world1/2)
##' @export
coastmap <- function(map = c(
  "world", "world2",
  "ant_coast", "ant_coast01", "ant_coast10",
  "Countries_hires",
  "world1", "world360",
  "cst00_polygon", "cst01_polygon", "cst10_polygon"), ...) {
  ##                     "GA_shelf", "GA_shelf_longlat", "GA_shelf_line", "GA_shelf_tosouth"), ...) {
  
stop("coastmap() is currently unsupported")
}

