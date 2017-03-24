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
readwrf0 <- function(x, band = 1) {
  ## band 5 is first u
  ## band 27 is first v
  prj <- "+proj=stere +lat_0=-90 +lat_ts=-60 +lon_0=180 +x_0=0 +y_0=0 +a=6367470 +b=6367470 +units=m +no_defs"
  rex <- c(xmin = -4724338, xmax = 4724338, 
           ymin = -5979038, ymax = 6518408)
  dat <- setExtent(stack(x, quick = TRUE)[[band]], rex)
  projection(dat) <- prj
  dat
}
readwrf0 <- function(x, band = 1) {
  ## band 5 is first u
  ## band 27 is first v
  prj <- "+proj=stere +lat_0=-90 +lat_ts=-60 +lon_0=180 +x_0=0 +y_0=0 +a=6367470 +b=6367470 +units=m +no_defs"
  rex <- c(xmin = -4724338, xmax = 4724338, 
           ymin = -5979038, ymax = 6518408)
  #dat <- setExtent(stack(x, quick = TRUE)[[band]], rex)
  dat <- suppressWarnings(rgdal::readGDAL(x, band = band))
  dat <- setExtent(raster(dat), rex)
  projection(dat) <- prj
  setNames(dat, sprintf("%s_%s", amps_metadata$GRIB_ELEMENT[band], amps_metadata$GRIB_SHORT_NAME[band]))
}

#' AMPS files
#' 
#' @inheritParams windfiles
#' @importFrom tibble tibble
#' @importFrom dplyr %>% arrange filter
#' @export
#' @section gdalinfo
#' 
amps_d1files <- function(data.source = "", time.resolution = "4hourly", ...) {
  datadir <- getOption("default.datadir")
  allfiles <- raadtools:::.allfilelist(rda = TRUE, fullname = FALSE)
  files1 <- grep("www2.mmm.ucar.edu", allfiles, value = TRUE)
  files2 <- grep("wrf_grib", files1, value = TRUE)
  files3 <- grep("grb$", files2, value = TRUE)
  #files <- tibble::tibble(fullname = grep("grb$", files2, value = TRUE)) %>% filter(grepl("d1", fullname))
  files <- tibble::tibble(file = files3) %>% filter(grepl("d1", file))
  files$fullname <- file.path(datadir, files$file)
  files$date <- as.POSIXct(strptime(basename(files$fullname), "%Y%m%d"), tz = "UTC") + 
     (as.integer(factor(as.integer(substr(basename(files$fullname), 20, 22)))) - 1) * 4 * 3600
  files <- files %>% arrange(date) 
  files <- files[!duplicated(files[, "date", drop = FALSE]), ]
  files
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
  
  bands <- c(5, 27) + level - 1
  for (ifile in seq_len(nfiles)) {
    r1 <- readwrf0(files$fullname[ifile], band = bands[1L]) #raster(files$ufullname[ifile], band = files$band[ifile])
    r2 <- readwrf0(files$fullname[ifile], band = bands[2L]) #raster(files$vfullname[ifile], band = files$band[ifile])
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
