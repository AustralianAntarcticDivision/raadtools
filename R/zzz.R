.onLoad <- function(libname, pkgname) {
  ## cannot remember why for ncdf4, probably should check and warn (or fail) here?
  base::loadNamespace("ncdf4")
  ## need raster so we can override the tiny maxmemory (which will be fixed: https://github.com/rspatial/raster/issues/4)
  base::loadNamespace("raster")
  mm <- getOption("rasterMaxMemory")
  if (is.null(mm)){
    raster::rasterOptions(maxmemory = 4e9)
  }
  options(raadtools.geoid_tile_vrt = NULL)
}


.onAttach <- function(libname, pkgname) {

}
