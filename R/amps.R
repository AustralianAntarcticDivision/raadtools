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



## driver functions for our AMPS collection
readwrfU <- function(date, level = 1L, ..., returnfiles = FALSE, inputfiles = NULL) {
  if (returnfiles) return(inputfiles)
  ff <- inputfiles$fullname[findInterval(date, inputfiles$date)  ]
  readwrf0(ff, band = 5 + level - 1L)
}
readwrfV <- function(date, level = 1L, ..., returnfiles = FALSE, inputfiles = NULL) {
  if (returnfiles) return(inputfiles)
  ff <- inputfiles$fullname[findInterval(date, inputfiles$date)  ]
  readwrf0(ff, band = 27 + level - 1L)
}

detect_amps_grid_from_filename <- function(x) {
  d1 <- grepl("_d1_", basename(x))
  d2 <- grepl("_d2_", basename(x))
  if (all(d1)) return("d1")
  if (all(d2)) return("d2")
  stop("grid not recognized, or mixed inputs")
}


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
#' @importFrom raadfiles amps_d1files amps_d2files amps_model_files
#' @export
amps_d1_icefiles <- function(data.source = "", time.resolution = "12hourly", ...) {
  files <- amps_model_files(data.source = data.source, time.resolution = time.resolution,  ...)
  ## TODO normalize file set
  ## we want the most files with the highest preference
  filter(files, grepl("f000", basename(fullname)), as.integer(hour) == 0)
}

readamps_d1ice <- function(date, time.resolution = "daily", xylim = NULL,

                           latest = TRUE, returnfiles = FALSE, level = 1, ..., inputfiles = NULL) {

  time.resolution <- match.arg(time.resolution)

  if (is.null(inputfiles)) {
    files <- amps_d1_icefiles(time.resolution = time.resolution)
  } else {
    files <- inputfiles
  }
  if (returnfiles) return(files)

  if (missing(date)) date <- if (latest) max(files$date) else min(files$date)
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

