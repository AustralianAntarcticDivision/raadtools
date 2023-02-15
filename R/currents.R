copernicus_is_atlantic <- function(x) {
  !copernicus_get_maxlon(x) > 180
}

copernicus_get_maxlon <- function(x) {
  nc <- RNetCDF::open.nc(x)
  val <- RNetCDF::var.get.nc(nc, "longitude", start = 1440L, count = 1L)
  RNetCDF::close.nc(nc)
  val
}


warp_wrapper <- function(dsn, gridinfo, resample = "near") {
  vapour::vapour_warp_raster_dbl(dsn, extent = gridinfo$extent, 
                             dimension = gridinfo$dimension, projection = gridinfo$projection, resample = resample)
}
read_i <- function(file, gridinfo) {
  warp_wrapper(file, gridinfo, resample = "near") 
}
# read_i_v0 <- function(file, xylim = NULL, lon180 = FALSE) {
#   warp_wrapper(file, gridinfo, resample = "near") 
# }
# read_uv0 <- function(file, xylim = NULL, lon180 = FALSE) {
#   stack(read_i_u0(file, xylim = xylim, lon180 = lon180), 
#         read_i_v0(file, xylim = xylim, lon180 = lon180))
# }
# read_i_dir0 <- function(file, xylim = NULL, lon180 = FALSE) {
#   x <- read_uv0(file, xylim = xylim, lon180 = lon180)
#   overlay(x[[1]], x[[2]], fun = function(x, y) (90 - atan2(y, x) * 180/pi) %% 360)
# }
vlen <- function(x, y) sqrt(x * x + y * y)
# read_i_mag0 <- function(file, xylim = NULL, lon180 = FALSE) {
#   x <- read_uv0(file, xylim = xylim, lon180 = lon180)
#   vlen(x[[1]], x[[2]])
# }




readcurr_polar <- function(date, 
                            xylim = NULL, 
                            latest = TRUE,
                            returnfiles = FALSE, ..., inputfiles = NULL) {
  
  if (is.null(inputfiles)) {
    files <- raadfiles:::altimetry_currents_polar_files()
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
  #raadfiles::altimetry_daily_files()
  files <- dplyr::inner_join(dplyr::rename(altimetry_daily_varname_files("ugos"), ugos_vrt = vrt_dsn), 
                             altimetry_daily_varname_files("vgos") |> dplyr::transmute(date, vgos_vrt = vrt_dsn), "date")
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
  
  info <- list(extent = c(-180, 180, -90, 90), dimension = c(1440, 720), 
               projection = "OGC:CRS84")
  if (is.null(xylim)) {
    grid <- info
  } else {
    grid <- .griddish(info, xylim)
    gg <- vaster::vcrop(grid$extent, info$dimension, info$extent)
    grid$dimension <- gg$dimension
    grid$extent <- gg$extent
  
  }
  time.resolution <- match.arg(time.resolution)
  
  ## put this block into currentsfiles() when happy
  if (is.null(inputfiles)) {
    files <- currentsfiles()
  } else {
    files <- inputfiles
  }
  
 
  if (magonly) thefun <- read_mag_daily
  if (dironly) thefun <- read_dir_daily
  if (uonly ) thefun <- read_ugos_daily
  if (vonly) thefun <- read_vgos_daily
  
  

  if (returnfiles)
    return(files)
  if (missing(date)) date <- if (latest) max(files$date) else min(files$date)
  date <- timedateFrom(date)
  files <- .processFiles(date, files, time.resolution)
  
  nfiles <- nrow(files)
  
  ## prevent reading more than one unless mag/dironly
  if ( !magonly & !dironly & !uonly & !vonly) {
    files <- files[1L,]
    message("reading u and v as layers is soft deprecated, please use read_ugos_daily() or read_ugos_daily()")
    message("for reading mag-nitude or dir-ection only, please use read_mag_daily() or read_dir_daily()")
    
    nfiles <- 1L
  
    thefun <- function(date, xylim = NULL, latest = TRUE, returnfiles = FALSE, ..., inputfiles = NULL) {
      u <- read_ugos_daily(date = date, xylim = xylim, latest  = latest, returnfiles = returnfiles,  ..., inputfiles = inputfiles)
      if (returnfiles) return(u)
      v <- read_vgos_daily(date = date, xylim = xylim, latest  = latest, returnfiles = returnfiles,  ..., inputfiles = inputfiles)
      stack(list(u, v))
    }
  }
  if ((magonly + dironly + uonly + vonly) > 1) stop("only one of 'magonly', 'dironly', 'uonly' or 'vonly' may be TRUE")


  
  op <- options(warn = -1)
  on.exit(options(op))
 
  r0 <- lapply(files$date, thefun, xylim = xylim, lon180 = lon180)
  if (length(r0) == 1) r0 <- r0[[1]] else r0 <- stack(r0)
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