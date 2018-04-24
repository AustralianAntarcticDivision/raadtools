## driver functions for our AMPS collection
readwrfU <- function(date, ..., returnfiles = FALSE, inputfiles = NULL) {
  if (returnfiles) return(files)
  ff <- inputfiles$fullname[findInterval(date, inputfiles$date)  ]
  readwrf0(ff, band = 5)
}
readwrfV <- function(date, ..., returnfiles = FALSE, inputfiles = NULL) {
  if (returnfiles) return(files)
  ff <- inputfiles$fullname[findInterval(date, inputfiles$date)  ]
  readwrf0(ff, band = 27)
}

detect_amps_grid_from_filename <- function(x) {
  d1 <- grepl("_d1_", basename(x))
  d2 <- grepl("_d2_", basename(x))
  if (all(d1)) return("d1")
  if (all(d2)) return("d2")
  stop("grid not recognized, or mixed inputs")
}


### 
### prj <-  "+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=180 +x_0=0 +y_0=0 +a=6367470 +b=6367470 +units=m +no_defs"
### d1 <- amps_d1files()
### d2 <- amps_d2files()
### 
### lat <- readwrf0(d1$fullname[1])
### lon <- readwrf0(d1$fullname[1], band = 2)
### x <- readwrf0(d1$fullname[1], band = 3)
### pts <- rgdal::project(cbind(values(lon), values(lat)), prj)
### spex::buffer_extent(extent(pts), 30000)
### 
### ######                       MAGIC NUMBERS
### plot(setExtent(x, extent(c(-4920000 - 30000, 4920000 , -6240000, 6810000))))
### plot(spTransform(wrld_simpl, projection(x)), add = TRUE)
### 
### lat <- readwrf0(d2$fullname[1])
### lon <- readwrf0(d2$fullname[1], band = 2)
### x <- readwrf0(d2$fullname[1], band = 3)
### pts <- rgdal::project(cbind(values(lon), values(lat)), prj)
### 
### setExtent(x, spex::buffer_extent(extent(pts), 10000))
### 
### ######                       MAGIC NUMBERS
### plot(setExtent(x, extent(c(-3370000 - 10000, 3280000, -2850000 - 10000, 3410000))))
### plot(spTransform(wrld_simpl, projection(x)), add = TRUE)
amps_grid_spec <- function(grid) {
  prj <-  "+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=180 +x_0=0 +y_0=0 +a=6367470 +b=6367470 +units=m +no_defs"
  ## grid 2 Continental 10km grid
  ## http://www2.mmm.ucar.edu/rt/amps/information/configuration/maps.html
  # WRF mass grid:
  #   dimensions: 666 x 627
  # lower left lat/lon (i.e., center of lower left mass grid cell): -50.80146 N, 49.79108 E
  # upper right lat/lon (i.e., center of upper right mass grid cell): -48.20068 N, -136.12355 E
  # cbind(c(49.79108, -136.12355), c(-50.80146, -48.20068))
  switch(grid, 
         d1 =   list(proj = prj, ex = c(xmin = -4920000 - 30000, xmax = 4920000 , ymin = -6240000, ymax = 6810000)), 
         d2 =   list(proj = prj, ex = c(xmin = -3370000 - 10000, xmax = 3280000, ymin = -2850000 - 10000, ymax = 3410000))
)
}
# readwrf0 <- function(x, band = 1) {
#   ## band 5 is first u
#   ## band 27 is first v
#   prj <- "+proj=stere +lat_0=-90 +lat_ts=-60 +lon_0=180 +x_0=0 +y_0=0 +a=6367470 +b=6367470 +units=m +no_defs"
#   rex <- c(xmin = -4724338, xmax = 4724338, 
#            ymin = -5979038, ymax = 6518408)
#   dat <- setExtent(stack(x, quick = TRUE)[[band]], rex)
#   projection(dat) <- prj
#   dat
# }
readwrf0 <- function(x, band = 1) {
  ## band 5 is first u
  ## band 27 is first v
  grid <- detect_amps_grid_from_filename(x)
  gridspec <- amps_grid_spec(grid)
  dat <- suppressWarnings(rgdal::readGDAL(x, band = band, silent = TRUE))
  dat <- setExtent(raster(dat), gridspec$ex)
  projection(dat) <- gridspec$proj
  data("amps_metadata", package = "raadtools")
  setNames(dat, sprintf("%s_%s", amps_metadata$GRIB_ELEMENT[band], amps_metadata$GRIB_SHORT_NAME[band]))
}

#' AMPS files
#' 
#' @inheritParams windfiles
#' @importFrom tibble tibble
#' @importFrom dplyr %>% arrange filter mutate
#' @export
#' @section gdalinfo
#' @importFrom raadfiles amps_d1files amps_d2files
#' @export amps_d1files amps_d2files
amps_model_files <- function(data.source = "", time.resolution = "4hourly", ...) {
  files <- raadfiles::amps_files()
  #datadir <- getOption("default.datadir")
  files$fullname <- file.path(files$root, files$file)
    dplyr::transmute(files, hour = substr(basename(fullname), 20, 22),
           model = substr(basename(fullname), 9, 10),
           date = as.POSIXct(strptime(basename(files$fullname), "%Y%m%d%H"), tz = "GMT") + 
             as.integer(hour) * 3600, fullname, file) 
  
  
}
amps_d1_icefiles <- function(data.source = "", time.resolution = "12hourly", ...) {
  files <- amps_model_files(data.source = data.source, time.resolution = time.resolution,  ...)
  ## TODO normalize file set
  ## we want the most files with the highest preference
  filter(files, grepl("f000", basename(fullname)), as.integer(hour) == 0)
}
#' read AMPS data
#' 
#' Read from 	The Antarctic Mesoscale Prediction System (AMPS) files. 
#' 
#' \code{readamps_d1wind} reads the "d1" \code{level} wind (defaults to 1). 
#' 
#' \code{readamps} reads the  \code{band} (defaults to 1). 
#' 
#' See \code{\link{amps_metadata}} for a description of the bands. 
#' 
#'
#' Data  http://www2.mmm.ucar.edu/rt/amps/wrf_grib/, and contain gridded forecast data from the AMPS-WRF model. 
#' The ‘d1’ (domain 1) files are on a 30km grid, while the ‘d2’ (domain 2) files are on a 10km grid. 
#' The grid domains are shown at  http://polarmet.osu.edu/AMPS/ - d2 extends out to southern Australia and the tip of South America, and 
#' d1 covers just Antarctica (including Davis) and parts of the Southern Ocean.
#' There are two forecast cycles, starting at 00UT and 12UT daily. Forecasts are for +00, +03, +06, +09, +12 and +15 hours. 
#' These steps can be used to see how the wind field is forecast as changing, and to obtain two different data sets each 
#' 3 hour timestep (e.g. the +15h forecast for the 00UT run is at the same time as the +03h forecast from the 12UT run). 
#' The 00UT and 12UT runs contain fewer parameters that the +03h and later forecasts
#' 
#' \preformatted{
#' An example file date is "2015-10-25 UTC"
#' gdalinfo 2015102512_WRF_d1_f000.grb
#' 
#' uonly is 
#' 
#' Band 5 Block=329x1 Type=Float64, ColorInterp=Undefined
#' Description = 10[m] HTGL (Specified height level above ground)
#' Metadata:
#'   GRIB_COMMENT=u-component of wind [m/s]
#'   GRIB_ELEMENT=UGRD
#'   GRIB_FORECAST_SECONDS=0 sec
#'   GRIB_REF_TIME=  1445774400 sec UTC
#'   GRIB_SHORT_NAME=10-HTGL
#'   GRIB_UNIT=[m/s]
#'   GRIB_VALID_TIME=  1445774400 sec UTC
#'   
#' vonly is
#' Band 27 Block=329x1 Type=Float64, ColorInterp=Undefined
#' Description = 10[m] HTGL (Specified height level above ground)
#' Metadata:
#'   GRIB_COMMENT=v-component of wind [m/s]
#' GRIB_ELEMENT=VGRD
#' GRIB_FORECAST_SECONDS=0 sec
#' GRIB_REF_TIME=  1445774400 sec UTC
#' GRIB_SHORT_NAME=10-HTGL
#' GRIB_UNIT=[m/s]
#' GRIB_VALID_TIME=  1445774400 sec UTC
#' }
#' @inheritParams readwind
#' @return Raster
#' @export
#'
#' @examples
#' af <- amps_d1files()
#' w <- readamps_d1wind(latest = TRUE, inputfiles = af)
#' vlen <- function(a, b) sqrt(a * a + b * b)
#' plot(vlen(w[[1]], w[[2]]))
#' 
#' ## arrow jigger
#' arr <- function(x, sub = seq(ncell(x[[1]])), scale = 1) {
#'  cr <- coordinates(x[[1]]); cr1 <- cr; 
#'  cr1[,1] <- cr[,1] + values(x[[1]])*scale; 
#'  cr1[,2] <- cr1[,2] + values(x[[2]]*scale); 
#'  segments(cr[sub,1], cr[sub,2], cr1[sub,1], cr1[sub,2])
#' }
#' arr(w, sample(ncell(w), 10000), scale = 30000)
readamps_d1wind <- function(date, time.resolution = "4hourly", xylim = NULL, 
                magonly = FALSE, dironly = FALSE, uonly = FALSE, vonly = FALSE,
                latest = FALSE, returnfiles = FALSE, level = 1, ..., inputfiles = NULL) {
  
  time.resolution <- match.arg(time.resolution)
  if ((magonly + dironly + uonly + vonly) > 1) stop("only one of magonly, dironly, uonly or vonly may be used, exiting")
  
  if (is.null(inputfiles)) {
    files <- amps_d1files(time.resolution = time.resolution)
  } else {
    files <- inputfiles
  }
  if (returnfiles) return(files)
  
  if (missing(date)) date <- min(files$date)
  if (latest) date <- max(files$date)
  date <- timedateFrom(date)
  ## findex <- .processDates(date, files$date, time.resolution)
  files <- .processFiles(date, files, time.resolution)
  
  
  nfiles <- nrow(files)
  if (nfiles > 1L & !magonly & !dironly & !uonly & !vonly) {
    warning("only one time index can be read at once unless 'magonly', 'dironly', 'uonly' or 'vonly' is TRUE")
    files <- files[1L,]
    nfiles <- 1L
  }
  
  
  if (!(magonly | dironly))
    rasterfun <- function(x1, x2) {
      x <- brick(x1, x2)
      names(x) <- c("U", "V")
      x
    }
  if (magonly)
    rasterfun <- function(x1, x2) sqrt(x1 * x1 + x2 * x2)
  if (dironly)
    rasterfun <- function(x1, x2) (90 - atan2(x2, x1) * 180/pi)%%360
  
  if (!(magonly | dironly)) {
    if (uonly) rasterfun <- function(x1, x2) x1
    if (vonly) rasterfun <- function(x1, x2) x2
  }
  
  
  cropit <- FALSE
  if (!is.null(xylim)) {
    cropit <- TRUE
    cropext <- extent(xylim)
  }
  
  r <- vector("list", nfiles)
  
  is_first_hour <- grepl("f000", basename(files$fullname))
  bands <- c(1, 23) + level - 1
  for (ifile in seq_len(nfiles)) {
    r1 <- readwrf0(files$fullname[ifile], band = bands[1L] + is_first_hour[ifile] * 4) #raster(files$ufullname[ifile], band = files$band[ifile])
    r2 <- readwrf0(files$fullname[ifile], band = bands[2L] + is_first_hour[ifile] * 4) #raster(files$vfullname[ifile], band = files$band[ifile])
    r0 <- rasterfun(r1, r2)
    #if (lon180)     r0 <- suppressWarnings(.rotate(r0))
    if (cropit) r0 <- crop(r0, cropext)
    r[[ifile]] <- r0
    
  }
  r <- stack(r)
  if (magonly | dironly | uonly | vonly)  {
    r <- setZ(r, files$date)
    names(r) <- sprintf("wind_%s", format(files$date, "%Y%m%d"))
  } else {
    
    r <- setZ(r, rep(files$date, 2L))
    names(r) <- sprintf("%swind_%s", c("U", "V"), format(files$date, "%Y%m%d"))
  }
  
  
  ## get alignment right (put this in raster?)
  extent(r) <- extent(c(xmin(r) + res(r)[1]/2, xmax(r) + res(r)[1]/2,
                        ymin(r), ymax(r)))
  
  ##if (is.na(projection(r))) projection(r) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0" 
  
  ## need to determine if "filename" was passed in
  dots <- list(...)
  if ("filename" %in% names(dots)) {
    r <- writeRaster(r, ...)
  }
  
  
  r
}


readamps_d1ice <- function(date, time.resolution = "daily", xylim = NULL, 
                            
                            latest = FALSE, returnfiles = FALSE, level = 1, ..., inputfiles = NULL) {
  
  time.resolution <- match.arg(time.resolution)
 
  if (is.null(inputfiles)) {
    files <- amps_d1_icefiles(time.resolution = time.resolution)
  } else {
    files <- inputfiles
  }
  if (returnfiles) return(files)
  
  if (missing(date)) date <- min(files$date)
  if (latest) date <- max(files$date)
  date <- timedateFrom(date)
  ## findex <- .processDates(date, files$date, time.resolution)
  files <- .processFiles(date, files, time.resolution)
  
  
  nfiles <- nrow(files)

  
  

  
  cropit <- FALSE
  if (!is.null(xylim)) {
    cropit <- TRUE
    cropext <- extent(xylim)
  }
  
  r <- vector("list", nfiles)
  
  for (ifile in seq_len(nfiles)) {
    r1 <- readwrf0(files$fullname[ifile], band = 3) #raster(files$ufullname[ifile], band = files$band[ifile])
    if (cropit) r1 <- crop(r1, cropext)
    r[[ifile]] <- r1
    
  }
  r <- stack(r)

    r <- setZ(r, files$date)
    names(r) <- sprintf("ice_%s",  format(files$date, "%Y%m%d"))

  
  
  ## get alignment right (put this in raster?)
  extent(r) <- extent(c(xmin(r) + res(r)[1]/2, xmax(r) + res(r)[1]/2,
                        ymin(r), ymax(r)))
  
  ##if (is.na(projection(r))) projection(r) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0" 
  
  ## need to determine if "filename" was passed in
  dots <- list(...)
  if ("filename" %in% names(dots)) {
    r <- writeRaster(r, ...)
  }
  
  
  r
}


#' @export
#' @name readamps_d1wind
readamps <- function(date, time.resolution = "4hourly", xylim = NULL, 
                            band = 1, 
                            latest = FALSE, returnfiles = FALSE, ..., inputfiles = NULL) {
  
  time.resolution <- match.arg(time.resolution)

  if (is.null(inputfiles)) {
    files <- amps_d1files(time.resolution = time.resolution)
  } else {
    files <- inputfiles
  }
  if (returnfiles) return(files)
  
  if (missing(date)) date <- min(files$date)
  if (latest) date <- max(files$date)
  date <- timedateFrom(date)
  ## findex <- .processDates(date, files$date, time.resolution)
  files <- .processFiles(date, files, time.resolution)
  
  
  nfiles <- nrow(files)

  
  cropit <- FALSE
  if (!is.null(xylim)) {
    cropit <- TRUE
    cropext <- extent(xylim)
  }
  
  r <- vector("list", nfiles)
  

  for (ifile in seq_len(nfiles)) {
    r0 <- readwrf0(files$fullname[ifile], band = band) #raster(files$ufullname[ifile], band = files$band[ifile])
    if (cropit) r0 <- crop(r0, cropext)
    r[[ifile]] <- r0
    
  }
  r <- stack(r)
    r <- setZ(r, files$date)
  
  
  
  ## get alignment right (put this in raster?)
  extent(r) <- extent(c(xmin(r) + res(r)[1]/2, xmax(r) + res(r)[1]/2,
                        ymin(r), ymax(r)))
  

  ## need to determine if "filename" was passed in
  dots <- list(...)
  if ("filename" %in% names(dots)) {
    r <- writeRaster(r, ...)
  }
  
 # print(sprintf("band: %i\n", band))
 # print(sprintf("min: %f\n", cellStats(r, min)))
  r
}
parse_amps_meta <- function(){
  tx <- readLines(system.file("extdata/amps/ampsfile_gdalinfo.txt", package= "raadtools"))
  idx <- grep("Description", tx)
  description <- gsub("Description = ", "", tx[idx])
  l <- lapply(idx, function(x) gsub("\\s+", "", tx[x + 1 + 1:7]))
  nms <- unlist(lapply(strsplit(l[[1]], "="), "[", 1))
  d <- bind_rows(lapply(l, function(x) tibble::as_tibble(setNames(lapply(strsplit(x, "="), "[", 2), nms))), .id = "Band")
  d$Band <- as.integer(d$Band)
  d
}
