copernicus_is_atlantic <- function(x) {
  !copernicus_get_maxlon(x) > 180
}

copernicus_get_maxlon <- function(x) {
  nc <- RNetCDF::open.nc(x)
  val <- RNetCDF::var.get.nc(nc, "longitude", start = 1440L, count = 1L)
  RNetCDF::close.nc(nc)
  val
}

read_i_u <- function(file, xylim = NULL, lon180 = FALSE) {
  x <- raster(file, varname = "ugos")
  if (copernicus_is_atlantic(file) && !lon180) x <- .rotate(x)
  if (!copernicus_is_atlantic(file) && lon180) x <- .rotate(x)
  if (!is.null(xylim)) x <- crop(x, xylim)
  
  x
}
read_i_v <- function(file, xylim = NULL, lon180 = FALSE) {
  x <- raster(file, varname = "vgos")
  if (copernicus_is_atlantic(file) && !lon180) x <- .rotate(x)
  if (!copernicus_is_atlantic(file) && lon180) x <- .rotate(x)
  if (!is.null(xylim)) x <- crop(x, xylim)
  
  x
}
read_i_uv <- function(file, xylim = NULL, lon180 = FALSE) {
  stack(read_i_u(file, xylim = xylim, lon180 = lon180), 
        read_i_v(file, xylim = xylim, lon180 = lon180))
}
read_i_dir <- function(file, xylim = NULL, lon180 = FALSE) {
  x <- read_i_uv(file, xylim = xylim, lon180 = lon180)
  overlay(x[[1]], x[[2]], fun = function(x, y) (90 - atan2(y, x) * 180/pi) %% 360)
}
read_i_mag <- function(file, xylim = NULL, lon180 = FALSE) {
  x <- read_i_uv(file, xylim = xylim, lon180 = lon180)
  vlen(x[[1]], x[[2]])
}

vlen <- function(x, y) sqrt(x * x + y * y)



readcurr_polar <- function(date, 
                            xylim = NULL, 
                            latest = TRUE,
                            returnfiles = FALSE, ..., inputfiles = NULL) {
  
  if (is.null(inputfiles)) {
    files <- raadfiles::altimetry_currents_polar_files()
  } else {
    files <- inputfiles
  }
  if (returnfiles)
    return(files)
  if (missing(date)) date <- if (latest) max(files$date) else min(files$date)
  date <- timedateFrom(date)
  files <- .processFiles(date, files, "daily")
  
  nfiles <- nrow(files)
  
  if (nfiles > 1) warning("only read one time slice atm with readcurr_polar")
  
  out <- raster::setZ(raster::brick(raster::raster(files$ufullname[1L]), raster::raster(files$vfullname[1L])), rep(files$date[1L], 2L))
  if (!is.null(xylim)) out <- raster::crop(out, xylim)
  out
}


##' Load file names and dates of AVISO current data
##'
##' A data.frame of file names and dates
##' @title AVISO ocean currents files
##' @param time.resolution time resolution to load
##' @param ... reserved for future use, currently ignored
##' @seealso \code{\link{readcurr}}
##' @return data.frame of file names and dates
##' @export
##' @importFrom raster filename 
currentsfiles <- function(time.resolution = c("daily", "weekly"), ...) {
  time.resolution <- match.arg(time.resolution)
  if (time.resolution != "daily") warning("only daily available, no weekly - ignoring 'time.resolution'")
  raadfiles::altimetry_daily_files()
}



altimetry_daily_ugos_files <- function() {
  files <- raadfiles::altimetry_daily_files()
  files$vrt_dsn <- .vrt_ds0(files$fullname, sds = "ugos")
  files
}
altimetry_daily_vgos_files <- function() {
  files <- raadfiles::altimetry_daily_files()
  files$vrt_dsn <- .vrt_ds0(files$fullname, sds = "vgos")
  files
}

# read_altimetry_u <- function(x, extent, dimension) {
#  vapour::vapour_warp_raster_dbl(x, 
#                                      extent = vc$extent, dimension = vc$dimension)
# }
# read_altimetry_v <- function(x, extent, dimension) {
#  files <- altimetry_daily_vgos_files() 
#  i <- 1
#  vrt <- files$vrt_dsn[i]
#  vapour::vapour_warp_raster_dbl(vrt, 
#                                 extent = vc$extent, dimension = vc$dimension)
#  
# }


.vrt_ds0 <- function(x, sds) {
  sprintf("NetCDF:%s:%s", x, sds)
}

##' Read AVISO ocean current data 
##'
##' Current data is read from files managed by
##' \code{\link{currentsfiles}}. Dates are matched to file names by
##' finding the nearest match in time within a short duration. By
##' default only one time step is returned with both U and V
##' components. Multiple dates can be returned for magnitude or
##' direction, U or V only.
##'
##' This is the "DT merged all satellites Global Ocean Gridded SSALTO/DUACS Sea Surface Height L4 product and derived variables"
##'  See References.
##'
##' @param date date or dates of data to read, see Details
##' @param time.resolution time resolution to read
##' @param xylim spatial extents to crop from source data, can be anything accepted by \code{\link[raster]{extent}}, see Details
##' @param lon180 defaults to TRUE, to "rotate" Pacific view [0, 360] data to Atlantic view [-180, 180]
##' @param magonly return just the magnitude from the U and V
##' components
##' @param dironly return just the direction from the U and V, in degrees N=0, E=90, S=180, W=270
##' @param uonly return just the U component of velocity
##' @param vonly return just the V component of velocity
##' components, in degrees (0 north, 90 east, 180 south, 270 west)
##' @param latest if TRUE (and date not supplied) return the latest time available, otherwise the earliest
##' @param returnfiles ignore options and just return the file names and dates
##' @param ... passed to brick, primarily for \code{filename}
##' @export
##' @note These data for daily files are stored in longitude/latitude projection on the sphere between longitudes in the Pacific
##' view \[0, 360\], the default behaviour is to reset this to Atlantic
##' view \[-180, 180\] with \code{lon180}. 
##'
##' @return \code{\link[raster]{raster}} object with the "U"
##' (meridional/horizontal/X) and "V" (zonal/vertical/Y) components of velocity in
##' m/s. Setting either of the (mutually exclusive) \code{magonly}
##' and \code{dironly} arguments returns the magnitude (in m/s) or
##' direction (in degrees relative to North) of the velocity vectors.
##' @seealso \code{\link{icefiles}} for details on the repository of
##' data files, \code{\link[raster]{raster}} for the return value
# imports should not be necessary here
##' @importFrom raster t flip atan2
##' @export
##' @references \url{http://marine.copernicus.eu}
##' @examples
#' ## read a single time slice, and plot the directions [0,360) as an image with arrows
#' x <- readcurr(dironly = TRUE)
#' ## get a local extent for a zoom plot
#' e <- extent(projectExtent(raster(extent(130, 150, -50, -30), crs = "+proj=longlat"), projection(x)))
#' x <- crop(readcurr(), e)
#' crds <- coordinates(x)
#' scale <- 1.5
#' vlen <- function(x) sqrt(x[[1]]^2 + x[[2]]^2)
#' plot(vlen(crop(x, e)))
#' x1 <- crds[,1]
#' y1 <- crds[,2]
#' x2 <- crds[,1] + values(x[[1]]) * scale
#' y2 <- crds[,2] + values(x[[1]]) * scale
#' arrows(x1, y1, x2, y2, length = 0.03)
readcurr <- function (date, time.resolution = c("daily"),
                      xylim = NULL, lon180 = TRUE, 
                      magonly = FALSE,
                      dironly = FALSE,
                      uonly = FALSE,
                      vonly = FALSE,
                      latest = TRUE,
                      returnfiles = FALSE, ..., inputfiles = NULL) {
  
  time.resolution <- match.arg(time.resolution)
  
  if (is.null(inputfiles)) {
    files <- currentsfiles(time.resolution = time.resolution)
  } else {
    files <- inputfiles
  }
  thefun <- read_i_uv
 
  if (magonly) thefun <- read_i_mag
  if (dironly) thefun <- read_i_dir
  if (uonly ) thefun <- read_i_u
  if (vonly) thefun <- read_i_v
  
  

  if (returnfiles)
    return(files)
  if (missing(date)) date <- if (latest) max(files$date) else min(files$date)
  date <- timedateFrom(date)
  files <- .processFiles(date, files, time.resolution)
  
  nfiles <- nrow(files)
  
  ## prevent reading more than one unless mag/dironly
  if (nfiles > 1L & !magonly & !dironly & !uonly & !vonly) {
    files <- files[1L,]
    nfiles <- 1L
    warning("only one time step can be read at once unless one of 'magonly', 'dironly', 'uonly' or 'vonly' is TRUE")
  }
  if ((magonly + dironly + uonly + vonly) > 1) stop("only one of 'magonly', 'dironly', 'uonly' or 'vonly' may be TRUE")

  

  #ximage::ximage(matrix(m, grid$dimension[2], byrow = TRUE))
 # return(NULL)
  dots <- list(...)

  
  
  op <- options(warn = -1)
  on.exit(options(op))
  
  r0 <- stack(lapply(files$fullname, thefun, xylim = xylim, lon180 = lon180), filename = filename)
  if (nlayers(r0) == nrow(files)) {
    r0 <- setZ(r0, files$date)
  } else {
    if (nlayers(r0) == 2 & nrow(files) == 1) {
      r0 <- setZ(r0, rep(files$date, 2))
    }
  }
  
  if ("filename" %in% names(dots)) {
    
     r0 <- writeRaster(r0, filename = dots[["filename"]])
    
  }
  
  projection(r0) <- "+proj=longlat +a=6371000 +b=6371000 +no_defs"
  
r0
  
  
}

# dimensions:
#   time = 1 ;
# lat = 720 ;
# lon = 1440 ;
# nv = 2 ;
# variables:
#   float time(time) ;
# time:long_name = "Time" ;
# time:standard_name = "time" ;
# time:units = "days since 1950-01-01 00:00:00 UTC" ;
# time:calendar = "julian" ;
# time:axis = "T" ;
# float lat(lat) ;
# lat:long_name = "Latitude" ;
# lat:standard_name = "latitude" ;
# lat:units = "degrees_north" ;
# lat:bounds = "lat_bnds" ;
# lat:axis = "Y" ;
# lat:valid_min = -90 ;
# lat:valid_max = 90 ;
# float lat_bnds(nv, lat) ;
# float lon(lon) ;
# lon:long_name = "Longitude" ;
# lon:standard_name = "longitude" ;
# lon:units = "degrees_east" ;
# lon:bounds = "lon_bnds" ;
# lon:axis = "X" ;
# lon:valid_min = 0 ;
# lon:valid_max = 360 ;
# float lon_bnds(nv, lon) ;
# int crs ;
# crs:grid_mapping_name = "latitude_longitude" ;
# crs:semi_major_axis = 6371000 ;
# crs:inverse_flattening = 0 ;
# int nv(nv) ;
# int u(lon, lat, time) ;
# u:_FillValue = -2147483647 ;
# u:long_name = "Absolute geostrophic velocity: zonal component" ;
# u:standard_name = "surface_eastward_geostrophic_sea_water_velocity" ;
# u:units = "m/s" ;
# u:scale_factor = 1e-04 ;
# int v(lon, lat, time) ;
# v:_FillValue = -2147483647 ;
# v:long_name = "Absolute geostrophic velocity: meridian component" ;
# v:standard_name = "surface_northward_geostrophic_sea_water_velocity" ;
# v:units = "m/s" ;
# v:scale_factor = 1e-04 ;
# 
# // global attributes:
#   :cdm_data_type = "Grid" ;
# :title = "DT merged Global Ocean Gridded Absolute Geostrophic Velocities SSALTO/Duacs L4 product" ;
# :summary = "This dataset contains Delayed Time Level-4 absolute geostrophic velocities products from multi-satellite observations over Global Ocean." ;
# :comment = "Surface product; Absolute Geostrophic Velocities" ;
# :time_coverage_resolution = "P1D" ;
# :product_version = "5.0" ;
# :institution = "CNES, CLS" ;
# :project = "SSALTO/DUACS" ;
# :references = "www.aviso.altimetry.fr" ;
# :contact = "aviso@altimetry.fr" ;
# :license = "http://www.aviso.altimetry.fr/fileadmin/documents/data/License_Aviso.pdf" ;
# :platform = "ERS-1, Topex/Poseidon" ;
# :date_created = "2014-02-28 13:07:00" ;
# :history = "2014-02-28 13:07:00:creation" ;
# :Conventions = "CF-1.6" ;
# :standard_name_vocabulary = "http://cf-pcmdi.llnl.gov/documents/cf-standard-names/standard-name-table/12/cf-standard-name-table.html" ;
# :geospatial_lat_min = -90 ;
# :geospatial_lat_max = 90 ;
# :geospatial_lon_min = 0 ;
# :geospatial_lon_max = 360 ;
# :geospatial_vertical_min = "0.0" ;
# :geospatial_vertical_max = "0.0" ;
# :geospatial_lat_units = "degrees_north" ;
# :geospatial_lon_units = "degrees_east" ;
# :geospatial_lat_resolution = 0.25 ;
# :geospatial_lon_resolution = 0.25 ;



.currentsfiles1 <- function(fromCache = TRUE, ...) {
  # datadir = getOption("default.datadir")
  # cachefile <- file.path(datadir, "cache", sprintf("currentsfiles_weekly.Rdata"))
  # if (fromCache) {
  #   load(cachefile)
  #   cfs$fullname <- file.path(datadir, cfs$file)
  #   return(cfs)
  # }
  # 
  ftx <- .allfilelist()
  cfiles <- grep("aviso_old", ftx, value = TRUE)
  cfiles1 <- grep("current", cfiles, value = TRUE)
  cfiles2 <- grep("merged_madt", cfiles1, value = TRUE)
  cfiles3 <- grep("nc$", cfiles2, value = TRUE)
  #data.source = file.path(datadir, "current", "aviso", "upd", "7d")
  #cfiles <- list.files(data.source, pattern = ".nc$", full.names = TRUE)
  datepart <- sapply(strsplit(basename(cfiles3), "_"), function(x) x[length(x)-1])
  currentdates <- timedateFrom(as.Date(strptime(datepart, "%Y%m%d")))
  
  cfs <- data.frame(fullname = cfiles3,  date = currentdates, stringsAsFactors = FALSE)
  cfs <- cfs[diff(cfs$date) > 0, ]
  
  ## drop duplicates, this should prefer upd to nrt
  cfs <- cfs[!duplicated(cfs$date), ]
  #save(cfs, file = cachefile)
  #cfs$fullname <- file.path(datadir, cfs$file)
  cfs
  
}
