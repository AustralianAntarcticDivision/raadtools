

##' Load file names and dates of Arrigo production data.
##'
##' These are 8 day estimates from MODIS and SeaWiFS satellite data,
##' original NASA algorithm.
##' @title Arrigo production files
##' @param fromCache load file catalog from cache, or rebuild it
##' @param ... reserved for future use, currently ignored
##' @return  data.frame of file names and dates
##' @export
prodfiles <- function(fromCache = TRUE, ...) {
  datadir <- getOption("default.datadir")
  dirpath <- file.path(datadir, "prod", "Arrigo", "8d")
  if (fromCache) {
    load(file.path(datadir, "cache", "prodfiles.Rdata"))
    pfiles$fullname <- file.path(datadir, pfiles$file)
    return(pfiles)
  }
  fs <- list.files(pattern = "prod\\.bin$", dirpath, full.names = TRUE)
  dates <- timedateFrom(as.Date(basename(fs), "%Y%j"))
  
  
  pfiles <- data.frame(file = gsub("^/", "", gsub(datadir, "", fs)), date = dates, stringsAsFactors = FALSE)
  ##save(pfiles, file = file.path(datadir, "cache", "prodfiles.Rdata"))
  pfiles
  
}

##' Read Arrigo production data.
##'
##' Arrigo production on Stereographic grid.
##' @title Arrigo production data
##' @param date date or dates of data to read
##' @param returnfiles if TRUE return just the files from \code{prodfiles}
##' @param time.resolution choice of temporal resolution, weekly only
##' @param xylim crop or not
##' @param ... reserved for future use, currently ignored
##' @return RasterLayer or RasterBrick
##' @export
readprod <- function(date,  time.resolution = "weekly", xylim = NULL, returnfiles = FALSE, ...) {
  
  read0 <- function(x) {
    proj <- "+proj=stere +lat_0=-90 +lon_0=180 +ellps=sphere"
    offset <- c(5946335, 5946335)
    dims <- c(1280L, 1280L)
    proddata <- readBin(x, numeric(), prod(dims), size = 4, endian = "little")
    proddata[proddata < 0] <- NA
    x <- list(x = seq(-offset[1L], offset[1L], length = dims[1L]),
              y =  seq(-offset[2L], offset[2L], length = dims[2L]),
              z = matrix(proddata, dims[1L])[rev(seq_len(dims[2L])),])
    
    raster(x, crs = proj)
    
  }
  
  
  time.resolution <- match.arg(time.resolution)
  
  files <- prodfiles()
  if (returnfiles) return(files)
  
  if (missing(date)) date <- min(files$date)
  date <- timedateFrom(date)
  ##    findex <- .processDates(date, files$date, time.resolution)
  
  files <- .processFiles(date, files, time.resolution)
  cropit <- FALSE
  if (!is.null(xylim)) {
    cropit <- TRUE
    cropext <- extent(xylim)
  }
  
  
  nfiles <- nrow(files)
  
  r <- vector("list", nfiles)
  
  for (ifile in seq_len(nfiles)) {
    r0 <- read0(files$fullname[ifile])
    if (cropit)
      r0 <- crop(r0, cropext)
    r[[ifile]] <- r0
  }
  
  r <- brick(stack(r), ...)
  names(r) <- sprintf("prod_%s", format(files$date, "%Y%m%d"))
  
  setZ(r, files$date)
  
}


