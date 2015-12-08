##' Coastline data set as SpatialPolygons*
##'
##' This function reads and returns a coastline polygon data set, either from the
##' source or serialized cache (.Rdata). Data source locations are controlled by options.
##'
##' The following named data sets are available.
##'
##' "world", "world2" - Atlantic and Pacific versions of \code{\link[maptools]{wrld_simpl}} ("world1", "world360" are aliases)
##' "ant_coast", "ant_coast01", "ant_coast10" - AAD Antartic coast in full, "1 mill", and "10 mill" resolution ("cst00_polygon", "cst01_polygon", and "cst10_polygon" are aliases)
##' "Countries_hires" - the "CIA" world map exported from the Manifold GIS data set
##'
##' @title Coast map
##' @param map A named map source
##' @param \dots arguments passed to worker functions
##' @return  SpatialPolygonsDataFrame or SpatialPolygons (world1/2)
##' @examples
##' ## load the maptools::wrld_simpl data set in [0,360]
##' \dontrun{
##' w360 <- coastmap("world360")
##' }
##'
##' ## load the AAD coast layer in "1 mill" resolution
##' \dontrun{
##' cst01 <- coastmap("ant_coast10")
##' }
##' @export
coastmap <- function(map = c(
  "world", "world2",
  "ant_coast", "ant_coast01", "ant_coast10",
  "Countries_hires",
  "world1", "world360",
  "cst00_polygon", "cst01_polygon", "cst10_polygon"), ...) {
  ##                     "GA_shelf", "GA_shelf_longlat", "GA_shelf_line", "GA_shelf_tosouth"), ...) {
  
  map <- match.arg(map)
  
  res <- switch(map,
                world = .world(),
                world1 = .world(),   ## synonym of world
                world2 = .world(FALSE),
                world360 = .world(FALSE),  ## synonym of world2
                ant_coast = .aadcoast(layer = "cst00_polygon", ...),
                ant_coast01 = .aadcoast(layer = "cst01_polygon", ...),
                ant_coast10 = .aadcoast(layer = "cst10_polygon", ...),
                cst00_polygon = .aadcoast(layer = "cst00_polygon", ...),  ## synonym of ant_coast
                cst01_polygon = .aadcoast(layer = "cst01_polygon", ...),  ## synonym of ant_coast01
                cst10_polygon = .aadcoast(layer = "cst10_polygon", ...),   ## synonym of ant_coast10
                Countries_hires = .manifoldcoast(layer = "Countries_hires")
                ##GA_shelf = .geoscience(layer = "Geomorph_shelf_laea"),
                ##GA_shelf_longlat = .geoscience(layer = "Geomorph_shelf_longlat"),
                ##GA_shelf_line = .geoscience(layer = "Geomorph_shelf_line"),
                ##GA_shelf_tosouth = .geoscience(layer = "Geomorph_shelf_tosouth")
  )
  res
}

.manifoldcoast <-  function(layer = c("Countries_hires"), fromCache = TRUE, debug = FALSE) {
  datapath <- getOption("default.datadir")
  cachepath <- file.path(datapath, "data_local", "vector_cache")
  
  layer = match.arg(layer)
  
  if (fromCache & !is.null(cachepath) & file.exists(cachepath)) {
    f <- file.path(cachepath, grep(layer, list.files(cachepath), value = TRUE))
    if (debug) print("loading from cache")
    if (debug) print(layer)
    if (debug) print(f)
    
    load(f)
    return(get(layer))
  }
  ##layerpath <- switch(layer,
  ##                    Countries_hires= file.path(datapath, "coastline")
  ## )
  ## if(debug) print(layerpath)
  ## if(debug) print(layer)
  ## if(require(rgdal)) {
  ## readOGR(layerpath, layer)
  ## } else {
  ##   stop(sprintf("cannot read layer %s from %s", layer, layerpath))
  ## }
  
}


##.geoscience <- function(layer) {
##    require(rgdal)
##     readOGR("D:/Toolbox/data_candidates/GeoScience", layer)
##}

##' @importFrom rgeos gIntersection gUnion
##' @importFrom maptools elide
##' @importFrom raster extent
##' @importMethodsFrom raster extent
.world <-
  function(world1 = TRUE) {
    
    on.exit(detach(pos = pos))
    attach(system.file("data", "wrld_simpl.rda", package = "maptools"))
    pos <- rev(grep("wrld_simpl.rda", search()))[1L]
    wrld <- get("wrld_simpl", pos = pos)
    ##detach(pos = pos)
    if (world1) return(as(wrld, "SpatialPolygons"))
    bb <- bbox(wrld)
    opt <- options(warn = -1)
    on.exit(options(opt))
    w1 <- gIntersection(wrld, as(extent(-180, 0, bb[2,1], bb[2,2]), "SpatialPolygons"), byid = TRUE)
    w2 <- gIntersection(wrld, as(extent(0,180, bb[2,1], bb[2,2]), "SpatialPolygons"), byid = TRUE)
    wrld <- gUnion(elide(w1, shift = c(360, 0)), w2, byid = TRUE)
    proj4string(wrld) <- CRS(" +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0 +over")
    return(wrld)
  }

.aadcoast <-
  function(layer = c("cst10_polygon", "cst01_polygon", "cst00_polygon"), fromCache = TRUE, debug = FALSE) {
    ##require(rgdal)
    datapath <- getOption("default.datadir")
    cachepath <- file.path(datapath, "data_local", "vector_cache")
    
    ##     gispath <- getOption("gispath")
    ##     cachepath <- getOption("cachepath")
    
    layer = match.arg(layer)
    
    if (fromCache & !is.null(cachepath) & file.exists(cachepath)) {
      f <- file.path(cachepath, grep(layer, list.files(cachepath), value = TRUE))
      if (debug) print("loading from cache")
      if (debug) print(layer)
      if (debug) print(f)
      
      load(f)
      return(get(layer))
    }
    ##   layerpath <- switch(layer,
    ##                    cst10_polygon = file.path(gispath, "Antarctic coast", "add_ver6", "10mill"),
    ##                    cst01_polygon = file.path(gispath, "Antarctic coast", "add_ver6", "1mill"),
    ##                    cst00_polygon = file.path(gispath, "Antarctic coast", "add_ver6", "best_resolution")
    ##                    )
    ##   readOGR(layerpath, layer)
    
    
  }