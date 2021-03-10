## because
# Projection: Data projected to NSIDC Sea Ice Polar Stereographic South. EPSG:3412 with 1km spatial resolution
# EPSG: 3412
# proj4string: +proj=stere +lat_0=-90 +lat_ts=-70 +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs

## manual workings for extent derived in read functions below
#lon <- flip(raster::raster(file, varname = "Longitude"), "y")
#lat <- flip(raster::raster(file, varname = "Latitude"), "y")
#idx <- c(1, ncol(lon), ncell(lon) - ncol(lon), ncell(lon))
#corner <- cbind(lon[idx], lat[idx])
#setExtent(r, spex::buffer_extent(extent(rgdal::project(corner, prj)), 1000))
# class      : RasterLayer 
# dimensions : 8300, 7900, 65570000  (nrow, ncol, ncell)
# resolution : 1000, 1000  (x, y)
# extent     : -3950000, 3950000, -3950000, 4350000  (xmin, xmax, ymin, ymax)




#' @export
#' @name read_leads_clim
read_leads_clim_south <- function(xylim = NULL) {
  file <- raadfiles::iceclim_south_leadsfiles()$fullname[1L]
  ## this is all you need, derived with interrogation of lon/lat arrays below
  sink(tempfile()); on.exit(sink(NULL), add = TRUE)
  r <- flip(raster::raster(file, varname = "LeadFrequency"), "y")
    prj <- "+proj=stere +lat_0=-90 +lat_ts=-70 +lon_0=0 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs"
  ext <- raster::extent(-3950000, 3950000, -3950000, 4350000 )
  projection(r) <- prj
  r <- setExtent(r, ext)
  if (!is.null(xylim)) {
    r <- crop(r, xylim)
  }
  r[r > 254] <- NA
  r
  
}

#' @export
#' @name read_leads_clim
read_leads_clim_north <- function(xylim = NULL) {
  file <- raadfiles::iceclim_north_leadsfiles()$fullname[1L]
  sink(tempfile()); on.exit(sink(NULL), add = TRUE)

  r <- flip(raster::raster(file, varname = "LeadFrequency"), "y")

  prj <- "+proj=stere +lat_0=-90 +lat_ts=-70 +lon_0=0 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs"
  ext <- raster::extent(-3950000, 3950000, -3950000, 4350000 )
  projection(r) <- prj
  r <- setExtent(r, ext)
  if (!is.null(xylim)) {
    r <- crop(r, xylim)
  }
  r[r > 254] <- NA
  r
  
}

#' Relative lead frequencies for the polar oceans
#' 
#' Average Lead-Frequency for the Arctic for winter months November-April 2002/03-2018/19 based on daily lead composites as 
#' derived from MOD/MYD-29 IST 5 min granules
#' 
#' @references 
#' F. Reiser, S. Willmes, G. Heinemann (2020), A new algorithm for daily sea ice lead identification in the Arctic and Antarctic 
#' winter from thermal-infrared satellite imagery, Data subm.
#' @export
#' @examples 
#' read_leads_clim()
#' read_leads_clim_north(xylim = extent(c(-1, 1, -1, 1) * 50000))
#' south <- read_leads_clim_south()
#' 
#' ## hone in on Mawson
#' pt <- rgdal::project(cbind(62 + 52/60, -(67 +  36/60)), projection(south))
#' lead <- read_leads_clim_south(xylim = extent(pt[1] + c(-1, 1) * 250000, pt[2] + c(-1, 1) * 250000))
#' plot(lead, col = grey.colors(100))
#' abline(v = pt[1], h = pt[2])
read_leads_clim <- function(hemisphere = c("south", "north"), 
                        xylim = NULL, ...) {
  hemisphere <- match.arg(hemisphere)
switch(hemisphere, 
        south = read_leads_clim_south(xylim = xylim), 
        north = read_leads_clim_north(xylim = xylim))
}