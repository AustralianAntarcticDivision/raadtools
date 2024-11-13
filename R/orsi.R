.orsi <- function() {
  data("orsifronts", package  = "orsifronts")
  get("orsifronts")
}
.park <- function() {
  data("orsifronts", package  = "parkfronts")
  get("parkfronts")
}

##' Load spatial map of fronts data.
##'
##' Currently ORSI or PARK is the only supported layers.
##' 
##' "park" = Southern Ocean fronts as determined by Park and Durand, doi: 10.17882/59800
##' 
##' "orsi" - the ORSI fronts derived from the files provided by the
##' WOCE Atlas, see References
##' @title Fronts map data for the Southern Ocean
##' @param map name of map to load
##' @references \url{http://woceatlas.tamu.edu/Sites/html/atlas/SOA_DATABASE_DOWNLOAD.html}
##' @return SpatialLinesDataFrame
##' @export
frontsmap <- function(map = c("park", "orsi")) {
  switch(map, 
         orsi  = .orsi(), 
         park = .park())
  
}

