
##' Load spatial map of fronts data.
##'
##' Currently ORSI is the only supported layer.
##'
##' "orsi" - the ORSI fronts derived from the files provided by the
##' WOCE Atlas, see References
##' @title Fronts map data for the Southern Ocean
##' @param map name of map to load
##' @references \url{http://woceatlas.tamu.edu/Sites/html/atlas/SOA_DATABASE_DOWNLOAD.html}
##' @return SpatialLinesDataFrame
##' @export
frontsmap <- function(map = c("orsi")) {
  .orsi()
}

.orsi <- function(layer = "orsi") {
  datapath <- getOption("default.datadir")
  cachepath <- file.path(datapath, "cache")
  f <- file.path(cachepath, grep(layer, list.files(cachepath), value = TRUE))
  load(f)
  return(get(layer))
}