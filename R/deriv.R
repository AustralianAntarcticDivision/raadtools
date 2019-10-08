
##' Read data from derived sea ice data products.
##'
##'
##' Derived sea ice data is read from files managed by \code{\link{derivicefiles}}.
##'
##' Currently available products are
##'
##' \describe{
##' }
##'
##' Dates are matched to file names by finding the nearest match in
##' time within a short duration. If \code{date} is greater than
##' length 1 then the sorted set of unique matches is returned.
##' @param date date or dates of data to read, see Details
##' @param time.resolution time resoution data to read, daily or monthly
##' @param product choice of sea ice product, see Details
##' @param xylim spatial extents to crop from source data, can be anything accepted by \code{\link[raster]{extent}}
##' @param latest if TRUE and date input missing, return the latest time available, otherwise the earliest
##' @param returnfiles ignore options and just return the file names and dates
##' @param ... passed to brick, primarily for \code{filename}
##' @param inputfiles input the file set to avoid rescanning that (for extract point-in-time)
##' @details 
##' time_since_melt
##' 
##' 32767 (treated as missing data)
##' 
##' 32766 = land
##' 
##' 32765 = open-ocean zone
##' 
##' -32768 = ice that hasn't melted during the data period
##' 
##' In terms of missing data 32767,  in the nc file, so should be NA once read into R): these are 
##' either open water in the sea ice zone that hasn't re-frozen during the data period, or missing sea ice data that 
##' couldn't be interpolated. 
##' @export
##' @return \code{\link[raster]{raster}} object
##' @seealso \code{\link{derivicefiles}} for details on the repository of
##' data files, \code{\link[raster]{raster}} for the return value
readderivice <- function(date,
                    time.resolution = c("daily"),
                    product = c("time_since_melt"),
                    xylim = NULL,
                
                    latest = FALSE,
                    returnfiles = FALSE, ..., inputfiles = NULL) {
  
  time.resolution <- match.arg(time.resolution)
  product <- match.arg(product)
  
  ## get file names and dates and full path
  if (is.null(inputfiles)) {
    files <- derivicefiles(product = product, ...)
  } else {
    files <- inputfiles
  }
  ##files$fullname <- file.path(datadir, files$file)
  if (returnfiles) return(files)
  if (missing(date)) {
    if (latest) date <- max(files$date)  else date <- min(files$date)
  }
  date <- timedateFrom(date)
  files <- .processFiles(date, files, time.resolution)
  
  ## check that files are available
  
  
  ## NSIDC projection and grid size for the Southern Hemisphere
  prj  <- "+proj=stere +lat_0=-90 +lat_ts=-70 +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs " 
  cropit <- FALSE
  if (!is.null(xylim)) {
    cropit <- TRUE
    cropext <- extent(xylim)
  }
  
  nfiles <- nrow(files)
  ## progress
  if (nrow(files) > 1L) {
  pb <- progress::progress_bar$new(
    format = "  extracting [:bar] :percent in :elapsed",
    total = nfiles, clear = FALSE, width= 60)
  pb$tick(0)
  }
  r <- vector("list", nfiles)
  ## loop over file indices
  for (ifile in seq_len(nfiles)) {
    r0 <- raster(files$fullname[ifile])
    if (cropit) r0 <- crop(r0, cropext)
    r[[ifile]] <- r0
    if (nrow(files) > 1L) { pb$tick()}
  }
  if (nfiles > 1) r <- brick(stack(r), ...) else r <- r[[1L]]
  
  projection(r) <- prj
  names(r) <- basename(files$file)
  setZ(r, files$date)
}




##' Load metadata and location of files of derived sea ice data products.
##'
##' This function loads the latest cache of stored files for
##' ice products, currently only daily NSIDC southern hemisphere is available. 
##' @param product which derived product
##' @param ... reserved for future use, currently ignored
##' @export
##' @return data.frame of \code{file} and \code{date}
derivicefiles <- function(product = "time_since_melt", ...) {
  
  datadir <- getOption("default.datadir")
  product <- match.arg(product)
  ftx <- .allfilelist()
 
 cfiles0 <- grep("webdav.data.aad.gov.au", ftx, value = TRUE)
  cfiles <- grep("time_since_melt", cfiles0, value = TRUE)
 
  if (length(cfiles) < 1) stop("no files found")
  
  doffs <- 0
  sep <- "_"
  datepart <- sapply(strsplit(basename(cfiles), sep), function(x) x[length(x) - doffs])
  
  datepattern <-  "%Y%m%d"
  dates <- timedateFrom(as.POSIXct(strptime(datepart, datepattern, tz = "GMT")))
  
  nas <- is.na(dates)
  dates <- dates[!nas]
  cfiles <- cfiles[!nas]
  
  cfs <- data.frame(file = gsub(paste(datadir, "/", sep = ""), "", cfiles), date = dates,
                    fullname = cfiles, stringsAsFactors = FALSE)[order(dates), ]
  
  cfs <- cfs[!duplicated(cfs$date), ]
  
  cfs
}
