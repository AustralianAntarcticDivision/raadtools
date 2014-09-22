##' Load definitions of administrative regions.
##'
##' The following maps are available, with modifier "LL" for
##' unprojected versions (modified for the dateline wrap).
##'
##' \describe{
##' \item{sectors}{The basic Southern Ocean sectors, each other layer derives from these}
##' \item{ssrus}{The CCAMLR SSRU boundaries}
##' \item{habitatAssessFull}{The sectors plus physical regions}
##' \item{habitatAsses}{The sectors plus physical regions, with non-ocean removed}}
##' @title administrative polygons
##' @param map name of layer to load, see Details
##' @param ... ignored
##' @return \code{SpatialPolygonsDataFrame}
##' @export
adminmap <- function(map = c("sectors", "ssrus",
                             "habitatAssess", "habitatAssessLL",
                             "habitatAssessFull", "habitatAssessFullLL"
), ...) {
  map <- match.arg(map)
  res <- switch(map,
                sectors = .admin("SectorAreas_PS"),
                ssrus = .admin("ssru"),
                habitatAssess = .admin("BioregionCLIP"),
                habitatAssessLL = .admin("BioregionCLIP_Longlat"),
                habitatAssessFull = .admin("BioregionORIG"),
                habitatAssessFullLL = .admin("BioregionORIG_Longlat"),
  )
  res
}

.admin <- function(layer = c("SectorAreas_PS", "ssru", "BioregionORIG_Longlat", "BioregionCLIP", "BioregionCLIP_Longlat", "BioregionORIG"), fromCache = TRUE) {
  datapath <- "\\\\aad.gov.au/files/ERM/Projects/HabitatAssessment/output_layers/Sectors"
  
  cachepath <- file.path(getOption("default.datadir"), "cache", "vector_cache")
  
  layer = match.arg(layer)
  
  if (fromCache & !is.null(cachepath) & file.exists(cachepath)) {
    f <- file.path(cachepath, grep(layer, list.files(cachepath), value = TRUE)[1])
    load(f)
    return(get(layer))
  }
  
  layerpath <- datapath
  
  
  if(require(rgdal)) {
    readOGR(layerpath, layer)
  } else {
    stop(sprintf("cannot read layer %s from %s", layer, layerpath))
  }
  
}

.loadcache_admin <- function(layers =  c("SectorAreas_PS", "BioregionORIG_Longlat", "BioregionCLIP", "BioregionCLIP_Longlat", "BioregionORIG")) {
  
  cachepath <- file.path(getOption("default.datadir"), "cache", "vector_cache")
  if (!file.exists(cachepath)) stop(sprintf("cachepath %s not available"))
  for (lay in layers) {
    print(lay)
    objectname <- lay
    assign(objectname, .admin(lay, fromCache = FALSE))
    fname <- file.path(cachepath, sprintf("%s.Rdata", lay))
    save(list = objectname, file = fname)
    rm(list = objectname)
  }
  
}

