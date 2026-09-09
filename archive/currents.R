## Retired from R/currents.R and R/utils.R on 2026-09-09. See archive/README.md.
##
## Nothing in the package called any of this. The live readers are
## read_copernicus_current_daily() / read_aviso_current_daily() in
## R/read-currents.R, with readcurr() and readcurrents() as the front doors in
## R/compat-currents.R; .needs_rotation() there replaces copernicus_is_atlantic().


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
  if (!is.null(xylim)) x <- raster::crop(x, xylim)
  
  x
}
read_i_v <- function(file, xylim = NULL, lon180 = FALSE) {
  x <- raster(file, varname = "vgos")
  if (copernicus_is_atlantic(file) && !lon180) x <- .rotate(x)
  if (!copernicus_is_atlantic(file) && lon180) x <- .rotate(x)
  if (!is.null(xylim)) x <- raster::crop(x, xylim)
  
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


.vrt_ds0 <- function(x, sds) {
  sprintf("NetCDF:%s:%s", x, sds)
}


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


## internal rotate to match old behaviour
## https://r-forge.r-project.org/scm/viewvc.php/pkg/raster/R/rotate.R?root=raster&r1=2782&r2=2981
#' @importFrom raster merge
.rotate <-  function(x, ...) {
  e <- extent(x)
  xrange <- e@xmax - e@xmin
  inverse <- FALSE
  if (xrange < 350 | xrange > 370 | e@xmin < -10 | e@xmax > 370) {
    if (xrange < 350 | xrange > 370 | e@xmin < -190 | e@xmax > 190) {
      warning('this does not look like an appropriate object for this function')
    } else {
      inverse <- TRUE
    }
  }
  hx <- e@xmin + xrange / 2
  r1 <- raster::crop(x, extent(e@xmin, hx, e@ymin, e@ymax))
  r2 <- raster::crop(x, extent(hx, e@xmax, e@ymin, e@ymax))
  if (inverse) {
    r1@extent@xmin <- r2@extent@xmax
    r1@extent@xmax <- r1@extent@xmin + 0.5 * xrange
  } else {
    r2@extent@xmin <- r2@extent@xmin - xrange
    r2@extent@xmax <- r2@extent@xmax - xrange
  }
  ln <- names(x)
  out <- merge(r1, r2, overlap=FALSE, ...)
  names(out) <- names(x)
  out@z <- x@z
  
  # suggested by Mike Sumner:
  p <- projection(out)	
  if (length(grep("\\+over", p)) > 0) {
    projection(out) <- gsub("[[:space:]]\\+over", "", p)
  }
  
  return(out)
}
