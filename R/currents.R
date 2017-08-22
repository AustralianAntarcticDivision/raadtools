
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
  datadir <- getOption("default.datadir")
  ## ftp.aviso.altimetry.fr/global/delayed-time/grids/madt/all-sat-merged/uv/1993/dt_global_allsat_madt_uv_19930101_20140106.nc
  ## ftp.aviso.altimetry.fr/global/delayed-time/grids/madt/all-sat-merged/uv/1993/dt_global_allsat_madt_uv_19930102_20140106.nc" 
  
  time.resolution <- match.arg(time.resolution)
  # if (time.resolution == "weekly") stop("weekly currents no longer supported")
  # ftx <- .allfilelist(rda = TRUE, fullname = FALSE)
  # cfiles0 <- grep("ftp.aviso.altimetry.fr", ftx, value = TRUE)
  # cfiles1 <- grep("uv", cfiles0, value = TRUE)
  # cfiles <- grep(".nc$", cfiles1, value = TRUE)
  # if (length(cfiles) < 1) stop("no files found")
  # 
  # doffs <- 1
  # datepart <- sapply(strsplit(basename(cfiles), "_"), function(x) x[length(x) - doffs])
  # 
  # dates <- timedateFrom(as.Date(strptime(datepart, "%Y%m%d")))
  # ## just the last one
  # nas <- is.na(dates[length(dates)])
  # if (nas) dates[length(dates)] <- max(dates, na.rm = TRUE) + 24 * 3600
  # #cfs <- data.frame(file = gsub(paste(datadir, "/", sep = ""), "", cfiles), date = dates,
  # #                  fullname = cfiles, stringsAsFactors = FALSE)[order(dates), ]
  # cfs <- data.frame(file = cfiles, date = dates,
  #                   fullname = file.path(datadir, cfiles), stringsAsFactors = FALSE)[order(dates), ]
  # ## drop any duplicated, this occurs with the delayed/near-real time update
  # cfs[!duplicated(cfs$date), ]  
  raadfiles::altimetry_daily_files()
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
##' This is the "SSALTO/DUACS - DT Geostrophic Velocities - Up-to-date Global Processing". See References.
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
##' @param latest if TRUE return the latest time available, ignoring the 'date' argument
##' @param returnfiles ignore options and just return the file names and dates
##' @param ... passed to brick, primarily for \code{filename}
##' @export
##' @note These data for daily files are stored in longitude/latitude projection on the sphere between longitudes in the Pacific
##' view \[0, 360\], the default behaviour is to reset this to Atlantic
##' view \[-180, 180\] with \code{lon180}. 
##'
##' For weekly files, this source is no longer provided by Aviso but is cached by the raadtools system at AAD/ACE, for the period 1992 to 2014. 
##' Note that the projection was Mercator for this earlier product. 
##' @return \code{\link[raster]{raster}} object with the "U"
##' (horizontal/X) and "V" (vertical/Y) components of velocity in
##' cm/s. Setting either of the (mutually exclusive) \code{magonly}
##' and \code{dironly} arguments returns the magnitude (in cm) or
##' direction (in degrees relative to North) of the velocity vectors.
##' @seealso \code{\link{icefiles}} for details on the repository of
##' data files, \code{\link[raster]{raster}} for the return value
# imports should not be necessary here
##' @importFrom raster t flip atan2
##' @export
##' @references \url{http://www.aviso.oceanobs.com/en/data/products/sea-surface-height-products/global/index.html}
##' @examples
##' \dontrun{
##' ## read a single time slice, and plot the directions [0,360) as an image with arrows
##' x <- readcurr(dironly = TRUE)
##' ## get a local extent for a zoom plot
##' e <- extent(projectExtent(raster(extent(130, 150, -50, -30), crs = "+proj=longlat"), projection(x)))
##' x1 <- crop(readcurr(), e)
##' crds <- coordinates(x1)
##' scale <- 2000
##' plot(crop(x, e))
##' x1 <- crds[,1]
##' y1 <- crds[,2]
##' x2 <- crds[,1] + values(x1[["U"]]) * scale
##' y2 <- crds[,2] + values(x1[["V"]]) * scale
##' arrows(x1, y1, x2, y2, length = 0.03)
##' }
readcurr <- function (date, time.resolution = c("daily", "weekly"),
                      xylim = NULL, lon180 = TRUE, 
                      magonly = FALSE,
                      dironly = FALSE,
                      uonly = FALSE,
                      vonly = FALSE,
                      latest = FALSE,
                      returnfiles = FALSE, ..., inputfiles = NULL) {
  time.resolution <- match.arg(time.resolution)
  
  if (is.null(inputfiles)) {
    files <- currentsfiles(time.resolution = time.resolution)
  } else {
    files <- inputfiles
  }
  
  read_i_u <- function(file, xylim = NULL, lon180 = FALSE) {
    x <- raster(file, varname = "ugos")
    if (lon180) x <- raadtools:::.rotate(x)
    if (!is.null(xylim)) x <- crop(x, xylim)
    
    x
  }
  read_i_v <- function(file, xylim = NULL, lon180 = FALSE) {
    x <- raster(file, varname = "vgos")
    if (lon180) x <- raadtools:::.rotate(x)
    if (!is.null(xylim)) x <- crop(x, xylim)
    
    x
  }
  read_uv <- function(file, xylim = NULL, lon180 = FALSE) {
    stack(read_i_u(file, xylim = xylim, lon180 = lon180), 
          read_i_v(file, xylim = xylim, lon180 = lon180))
  }
  read_i_dir <- function(file, xylim = NULL, lon180 = FALSE) {
    x <- read_uv(file, xylim = xylim, lon180 = lon180)
    overlay(x[[1]], x[[2]], fun = function(x, y) (90 - atan2(y, x) * 180/pi) %% 360)
  }
  vlen <- function(x, y) sqrt(x * x + y * y)
  read_i_mag <- function(file, xylim = NULL, lon180 = FALSE) {
    x <- read_uv(file, xylim = xylim, lon180 = lon180)
    vlen(x[[1]], x[[2]])
  }
  
  thefun <- read_uv  
  if (magonly) thefun <- read_i_mag
  if (dironly) thefun <- read_i_dir
  if (uonly ) thefun <- read_i_u
  if (vonly) thefun <- read_i_v
  
  

  if (returnfiles)
    return(files)
  if (missing(date)) date <- min(files$date)
  if (latest) date <- max(files$date)
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

.readcurr1 <- function(date,
                       time.resolution = "weekly",
                       xylim = NULL,
                       ##setNA = TRUE,
                       ##rescale = TRUE,
                       magonly = FALSE,
                       dironly = FALSE,
                       
                       uonly = FALSE,
                       vonly = FALSE,
                       
                       lon180 = TRUE,
                       latest = FALSE,
                       returnfiles = FALSE,
                       verbose = TRUE,
                       ...) {
  
  read0 <- function(x, varname) {
    xtreme <- 20037508
    ytreme <- 16925422
    x <- flip(flip(t(raster(x, varname = varname, stopIfNotEqualSpaced=FALSE)), direction = "y"),
              direction = "x")
    extent(x) <- extent(0, xtreme * 2, -ytreme, ytreme)
    projection(x) <- "+proj=merc +ellps=WGS84 +over"
    x
  }
  
  
  time.resolution <- match.arg(time.resolution)
  if ((magonly + dironly + uonly + vonly) > 1) stop("only one of magonly, dironly, uonly or vonly may be used, exiting")
  
  files <- .currentsfiles1()
  if (returnfiles) return(files)
  
  
  if (missing(date)) date <- min(files$date)
  if (latest) date <- max(files$date)
  date <- timedateFrom(date)
  ##findex <- .processDates(date, files$date, time.resolution)
  files <- .processFiles(date, files, time.resolution)
  
  
  nfiles <- nrow(files)
  ## prevent reading more than one unless mag/dironly
  if (nfiles > 1L & !magonly & !dironly & !uonly & !vonly) {
    files <- files[1L,]
    nfiles <- 1L
    warning("only one time step can be read at once unless 'magonly', 'dironly', 'uonly' or 'vonly' is TRUE")
  }
  
  
  
  if (!(magonly | dironly)) rasterfun <- function(x1, x2) {x <- brick(x1, x2); names(x) <- c("U", "V");x}
  if (magonly) rasterfun <- function(x1, x2) sqrt(x1 * x1 + x2 *x2)
  if (dironly) rasterfun <- function(x1, x2) (90 - atan2(x2, x1) * 180/pi) %% 360
  
  if (!(magonly | dironly)) {
    if (uonly) rasterfun <- function(x1, x2) x1
    if (vonly) rasterfun <- function(x1, x2) x2
  }
  
  ## process xylim
  cropit <- FALSE
  if (!is.null(xylim)) {
    cropit <- TRUE
    cropext <- extent(xylim)
    
  }
  
  
  
  r <- vector("list", nfiles) ##brick(rasterfun(r1, r2), nl = length(findex))
  for (ifile in seq_len(nfiles)) {
    r1 <- read0(files$fullname[ifile], varname = "Grid_0001")
    r2 <- read0(files$fullname[ifile], varname = "Grid_0002")
    ##r1 <- .readAVISO(files$fullname[findex[ifile]], justone = FALSE)
    ##r <- setValues(r, values(rasterfun(r1, r2)), layer = i)
    r0 <- rasterfun(r1, r2)
    if (lon180) r0 <- suppressWarnings(.rotate(r0))
    if(cropit) r0 <- crop(r0, cropext)
    r[[ifile]] <- r0
    ##if (verbose & ifile %% 10L == 0L) .progressreport(ifile, nfiles)
    
  }
  
  r <- brick(stack(r), ...)
  if (magonly | dironly | uonly | vonly) r <- setZ(r, files$date) else r <- setZ(r, rep(files$date, 2L))
  
  if (magonly | dironly | uonly | vonly)  {
    r <- setZ(r, files$date)
    names(r) <- sprintf("wind_%s", format(files$date, "%Y%m%d"))
  } else {
    
    r <- setZ(r, rep(files$date, 2L))
    names(r) <- sprintf("%scurrents_%s", c("U", "V"), format(files$date, "%Y%m%d"))
  }
  
  
  
  ##    if (lon180) r <- suppressWarnings(.rotate(r))
  return(r)
  
}
