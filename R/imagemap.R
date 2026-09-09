
##' Read map images.
##'
##' The ancient lost art of cartography.
##'
##' ibcso_background: The IBCSO RGB-map rasterlayer in high resolution
##' @title Maps of places
##' @param map name of the map to load
##' @param fact resize factor, see \code{\link[terra]{aggregate}}
##' @return SpatRaster, with R G B bands
##' @references
##' \url{http://www.ibcso.org/data.html}
##' @export
imagemap <- function(map = c("ibcso_background_hq"),
                     fact = 1L) {
  ftx <- .allfilelist()
  cfiles <- grep("www.ibcso.org", ftx, value = TRUE)
  cfiles1 <- grep("ibcso_background_hq.tif$", cfiles, value = TRUE)
  map <- match.arg(map)
  fpath <- switch(map,
                  ibcso_background_hq = cfiles1
                  ## ant_and_sthn_ocean_13989 = file.path(datadir,  "maps", "ant_and_sthn_ocean_13989.tif"),
                  ## ant_sthn_ocean_ed9_13939 = file.path(datadir, "maps", "ant_sthn_ocean_ed9_13939.tif"),
                  ## kerguelen_to_antarctica_bathy_14033 = file.path(datadir, "maps",  "kerguelen_to_antarctica_bathy_14033.tif")
  )
  
  if (file.exists(fpath) & interactive()) message("\n\nremember to plot with plotRGB(x)")
  x <- .rast_nc(fpath)
  if (fact > 1) {
    ##aggregate(x, fact, fun = function(x, na.rm = TRUE) sample(x, 1L))
    ## terra spells raster's "ngb" as "near"
    template <- terra::rast(terra::ext(x), nrows = ceiling(nrow(x)/fact),
                            ncols = ceiling(ncol(x)/fact), crs = terra::crs(x))
    x <- terra::resample(x, template, method = "near")
    
  }
  names(x) <- c("R", "G", "B")
  x
}
