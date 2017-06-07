.onLoad <- function(libname, pkgname) {
  base::loadNamespace("ncdf4")
  base::loadNamespace("raster")
  mm <- getOption("rasterMaxMemory")
  if (is.null(mm)){
    raster::rasterOptions(maxmemory = 4e9)
  }
}


.onAttach <- function(libname, pkgname) {

}