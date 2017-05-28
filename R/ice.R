
##' Read data from sea ice data products.
##'
##'
##' Sea ice data is read from files managed by \code{\link{icefiles}}.
##'
##' Currently available products are
##'
##' \describe{
##' \item{'nsidc'}{daily or monthly NSIDC concentration data, processed by the SMMR/SSMI NASA Team}
##' \item{'ssmi'}{daily SSMI concentration data for the Southern Hemisphere}
##' }
##'
##' Dates are matched to file names by finding the nearest match in
##' time within a short duration. If \code{date} is greater than
##' length 1 then the sorted set of unique matches is returned.
##' @param date date or dates of data to read, see Details
##' @param time.resolution time resoution data to read, daily or monthly
##' @param product choice of sea ice product, see Details
##' @param hemisphere north or south
##' @param xylim spatial extents to crop from source data, can be anything accepted by \code{\link[raster]{extent}}
##' @param setNA mask zero and values greater than 100 as NA
##' @param rescale rescale values from integer range?
##' @param latest if TRUE return the latest time available, ignoring the 'date' argument
##' @param returnfiles ignore options and just return the file names and dates
##' @param ... passed to brick, primarily for \code{filename}
##' @param extension default for product "amsr" is "hdf" but can be "tif" , extension = "hdf"
##' @param inputfiles input the files data base to speed up initialization
##' @details For NSIDC data a \code{\link[raster]{ratify}}ied raster is returned if \code{setNA} and 
##' \code{rescale} are both set to \code{FALSE}.  Use \code{levels(x)} to return the data.frame of values 
##' and levels (there's no straight-through rule, all numeric values are explicit along with special
##' values like "Unused"). 
##' The values used are documented here \url{http://nsidc.org/data/docs/daac/nsidc0051_gsfc_seaice.gd.html}
##' @export
##' @examples 
##' \dontrun{
##' # library(raadtools)
##' # 
##' # ice <- readice(product = "amsr", latest = TRUE)
##' # 
##' # sensor <- "MODISA"
##' # ocf <- ocfiles(product = sensor, varname = "RRS")
##' # 
##' # library(roc)
##' # rrs <- readL3(ocf$fullname[nrow(ocf)])
##' # chl <- chla(rrs, algo = "johnson", sensor = sensor)/rrs$weights
##' # 
##' # library(rgdal)
##' # pxy <- project(do.call(cbind, bin2lonlat(rrs$bin_num, rrs$NUMROWS)), projection(ice))
##' # 
##' # par(bg = grey(0))
##' # ##plot(ice, col = grey(seq(0, 0.9, length = 100)), axes = FALSE, legend = FALSE)
##' # plot(ice, col = "transparent", axes = FALSE, legend = FALSE)
##' # points(pxy, cex = 0.1, pch = 16, col = chl.pal(chl))
##' # 
##' # n <- 14
##' # ss <-  rev( 1/((1:n)^.3))
##' # 
##' # for (i in 1:n) {
##' #   rrs <- readL3(ocf$fullname[nrow(ocf) - i])
##' #   chl <- chla(rrs, algo = "johnson", sensor = sensor)/rrs$weights
##' #   ice <- readice(ocf$date[nrow(ocf) - i], product = "amsr")
##' #   xy <- do.call(cbind, bin2lonlat(rrs$bin_num, rrs$NUMROWS))
##' #   asub <- xy[,2] < -38
##' #   pxy <- project(xy[asub, ], projection(ice))
##' #   points(pxy, cex = 0.2, pch = 16, col = chl.pal(chl[asub], alpha = 1/(i + 0.1)))
##' #   contour(ice, lev = 15, col =  rgb(1, 1, 1, alpha = 0.2, add = TRUE, lwd = 1.3)
##' # }  
##' }
##' @return \code{\link[raster]{raster}} object
##' @seealso \code{\link{icefiles}} for details on the repository of
##' data files, \code{\link[raster]{raster}} for the return value
readice <- function(date,
                    time.resolution = c("daily", "monthly"),
                    product = c("nsidc", "amsr", "ssmi"),
                    hemisphere = c("south", "north"), 
                    xylim = NULL,
                    setNA = TRUE, rescale = TRUE, 
                    latest = FALSE,
                    returnfiles = FALSE,  ..., inputfiles = NULL) {
  
  time.resolution <- match.arg(time.resolution)
  product <- match.arg(product)
  
  hemisphere <- match.arg(hemisphere)
  
  if (!is.null(inputfiles)) {
     files <- inputfiles
  } else {
   ## get file names and dates and full path
   files <- icefiles(time.resolution = time.resolution, product = product, hemisphere = hemisphere)
  }
  ##files$fullname <- file.path(datadir, files$file)
  if (returnfiles) return(files)
  if (product == "amsr" & .Platform$OS.type == "windows") warning("sorry, AMSR2 files are HDF4 so this is unlikely to work on your machine")
  if (missing(date)) date <- min(files$date)
  if (latest) date <- max(files$date)
  date <- timedateFrom(date)
  files <- .processFiles(date, files, time.resolution)
  
  ## check that files are available
  
  
  ## NSIDC projection and grid size for the Southern Hemisphere
  prj  <- "+proj=stere +lat_0=-90 +lat_ts=-70 +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs " 
  if(hemisphere == "north") prj <-    "+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs"
  nsidcdims <- if (hemisphere == "south") c(332L, 316L) else c(448L, 304L)
  ext <- if (hemisphere == "south") c(-3950000, 3950000, -3950000, 4350000) else c(-3837500, 3762500, -5362500, 5837500)
  
  ## modify based on dataproduct
  dims <- switch(product,
                 nsidc = nsidcdims,
                 amsr = c(1328, 1264),
                 ssmi = c(632L, 664L))
  
  res <-  switch(product,
                 nsidc = c(25000, 25000),
                 amsr = c(6250, 6250),
                 ssmi = c(12500, 12500))
  
  if (product == "ssmi") stop("sorry SSMI data is temporarily unavailable")
  
  rtemplate <- raster(extent(ext), nrows =  dims[1L], ncols = dims[2L], crs = prj)
  ## process xylim
  cropit <- FALSE
  if (!is.null(xylim)) {
    cropit <- TRUE
    cropext <- extent(xylim)
  }
  
  nfiles <- nrow(files)
  r <- vector("list", nfiles)
  ## note that this can be replaced by a direct raster(file) once the package
  ## gets updated (post raster_2.1-49, October 2013)
  .readNSIDC <- function(fname) {
    con <- file(fname, open = "rb")
    trash <- readBin(con, "integer", size = 1, n = 300)
    dat <- readBin(con, "integer", size = 1, n = prod(dims), endian = "little", signed = FALSE)
    close(con)
    r100 <- dat > 250
    r0 <- dat < 1
    if (rescale) {
      dat <- dat/2.5  ## rescale back to 100
    }
    if (setNA) {
      dat[r100] <- NA
      ##dat[r0] <- NA
    }
    
    # 251  Circular mask used in the Arctic to cover the irregularly-shaped data gap around the pole (caused by the orbit inclination and instrument swath)
    # 252	Unused
    # 253	Coastlines
    # 254	Superimposed land mask
    # 255	Missing data
    # 
    ## ratify if neither rescale nor setNA set
    r <- raster(t(matrix(dat, dims[1])), template = rtemplate)
    if (!setNA && !rescale) {
      ##r <- ratify(r)
      rat <- data.frame(ID = 0:255, icecover = c(0:250, "ArcticMask", "Unused", "Coastlines", "LandMask", "Missing"), 
                  code = 0:255, stringsAsFactors = FALSE)
      levels(r) <- rat
      r
    } else {
      r
    }
  }
  .readAMSR <- function(fname) {
    x <- flip(raster(fname), direction = "y")
    extent(x) <- extent(rtemplate)
    ## earlier files were 0,1
    if (grepl("asi.nl.s6250", basename(fname))) x <- x * 100
    x
  }
  .readSSMI <- function(fname) {
    x <- raster(fname, varname = "concentration")
    x <- flip(x, "y")
    if (!setNA) {
      x[is.na(x)] <- -127
    } else {
      x[x > 100 | x < 1] <- NA
    }
    extent(x) <- extent(rtemplate)
    x
  }
  
 ## if (!product == "nsidc") {
  ## loop over file indices
  for (ifile in seq_len(nfiles)) {
    r0 <- switch(product,
                 nsidc = .readNSIDC(files$fullname[ifile]),
                 amsr = .readAMSR(files$fullname[ifile]), 
                 ssmi = .readSSMI(files$fullname[ifile]))
    
    if (cropit) r0 <- crop(r0, cropext)
    r[[ifile]] <- r0
  }
    r <- stack(r)
  ##} else {
  ##  r <- stack(files$fullname)
  ##}
  
  projection(r) <- prj
  names(r) <- basename(files$file)
  r <- setZ(r, files$date)
  
  ## TODO need filename for the singleton case
 ## r <- brick(r, ...)
  if ("filename" %in% names(list(...))) r <- writeRaster(r, ...)
  
  r
  

}

#r <- readice("1995-01-01", dataproduct = "ssmi")
#r1 <- readice("1995-01-01")




##' Load metadata and location of files of sea ice data products.
##'
##' This function loads the latest cache of stored files for
##' ice products.
##' @param time.resolution daily or monthly files?
##' @param product choice of sea ice product, see \code{\link{readice}}
##' @param hemisphere north or south
##' @param ... reserved for future use, currently ignored
##' @param extension default for product "amsr" is "hdf" but can be "tif" , extension = "hdf"
##' @export
##' @examples
##' \dontrun{
##' icf <- icefiles(time.resolution = "monthly")
##' icf[which.min((as.Date("1995-01-01") + runif(1, -4000, 4000)) - as.Date(icf$date), ]
##' }
##' @return data.frame of \code{file} and \code{date}
icefiles <- function(time.resolution = c("daily", "monthly"), 
                     product = c("nsidc", "amsr"), hemisphere =c("south", "north"), ..., extension = "hdf") {
  
  datadir <- getOption("default.datadir")
  
  time.resolution <- match.arg(time.resolution)
  product <- match.arg(product)
  hemisphere <- match.arg(hemisphere)
  if (product == "ssmi") stop("sorry SSMI is current not available")
  ## data/sidads.colorado.edu/pub/DATASETS/nsidc0081_nrt_nasateam_seaice/north
  ## data/sidads.colorado.edu/pub/DATASETS/nsidc0081_nrt_nasateam_seaice/south
  ## data/sidads.colorado.edu/pub/DATASETS/nsidc0051_gsfc_nasateam_seaice/final-gsfc/north/daily/
  ## data/sidads.colorado.edu/pub/DATASETS/nsidc0051_gsfc_nasateam_seaice/final-gsfc/north/monthly/
  ## data/sidads.colorado.edu/pub/DATASETS/nsidc0051_gsfc_nasateam_seaice/final-gsfc/south/daily/
  ## data/sidads.colorado.edu/pub/DATASETS/nsidc0051_gsfc_nasateam_seaice/final-gsfc/south/monthly/  
  
  ## data/www.iup.uni-bremen.de+8084/amsr2data/asi_daygrid_swath/s6250
  
  if (time.resolution != "daily" & product == "amsr") stop("product=\"AMSR\" is only compatible with daily time resolution")
  
  ftx <- .allfilelist(rda = TRUE, fullname = FALSE)
  ## just shortcut here for AMSR (need to review code below)
  if (product == "amsr") return(.amsr625_dbfiles())
  ppat <- switch(product, 
                 nsidc = "sidads.colorado.edu",
                 ## need to use the + for some reason
                 amsr = "www.iup.uni-bremen.de\\+8084")
  strpat <- switch(product, 
                   nsidc = "nt_", 
                   amsr = "AMSR2")
  
  epat <- switch(product, 
                 nsidc = ".bin$", 
                 
                 amsr = ".hdf$")  ## this is ignored
  if (product == "amsr" & hemisphere != "south") stop("no north hemisphere for amsr")
  cfiles0 <- grep(ppat, ftx, value = TRUE)
  cfiles1 <- if(product == "nsidc") {
    c(grep(time.resolution, cfiles0, value = TRUE), grep("_nrt_", cfiles0, value = TRUE))
    }  else {
      cfiles0
    }
  cfiles2 <- if(product == "nsidc") grep(hemisphere, cfiles1, value = TRUE) else cfiles1
  
  cfiles3 <- grep(strpat, cfiles2, value = TRUE)
  cfiles <- grep(epat, cfiles3, value = TRUE)
  
  if (length(cfiles) < 1) stop("no files found")
  
  doffs <- if(product == "nsidc") 3 else 1
  sep <- if(product == "nsidc") "_" else "-"
  datepart <- sapply(strsplit(basename(cfiles), sep), function(x) x[length(x) - doffs])
  
  
  datepat <-  "%Y%m%d"
  if (time.resolution == "monthly") datepart <- sprintf("%s01", datepart)
  dates <- timedateFrom(as.POSIXct(strptime(datepart, datepat, tz = "GMT")))
  
  nas <- is.na(dates)
  dates <- dates[!nas]
  cfiles <- cfiles[!nas]

  cfs <- data.frame(file = cfiles,
    #file = gsub(paste(datadir, "/", sep = ""), "", cfiles), 
                    date = dates,
                    fullname = file.path(datadir, cfiles), 
    stringsAsFactors = FALSE)[order(dates), ]
  
  cfs <- cfs[!duplicated(cfs$date), ]
  
  cfs
}

.amsr625files <- function(allfiles, ext) {
  ## 2002:2011
  f1 <- "ftp-projects.zmaw.de/seaice/AMSR-E_ASI_IceConc/no_landmask/hdf/s6250/"
  ## 2012:2015+
  f2 <- "www.iup.uni-bremen.de\\+8084/amsr2data/asi_daygrid_swath/s6250"
  datadir <- getOption("default.datadir")
  f3 <- allfiles[c(grep(f1, allfiles), grep(f2, allfiles))]
  f4 <- grep(sprintf("%s", ext),  f3, value = TRUE)
  ## que?
  f5 <- f4[!grepl("s12500", f4)]
  files <- data.frame(fullname = file.path(datadir, f5), 
             #file = gsub(paste0(datadir, "/data/"), "", f5), 
             file = f5,
             date = as.POSIXct(strptime(sapply(strsplit(basename(f5), "-"), "[", 4), "%Y%m%d"), tz = "UTC"), 
             stringsAsFactors = FALSE)
  
           
  files <- files[order(files$date), ]  
  ## keep the last of the duplicates
  #files[!rev(duplicated(rev(files$date))), ]
  files[!duplicated(files$date), ]
  
}
# system.time(af <- .amsr625files(.allfilelist(), "hdf"))
# user  system elapsed 
# 7.296   0.080   7.379 
# system.time(af2 <- .amsr625_dbfiles())
# user  system elapsed 
# 0.476   0.240   1.713

.amsr625_dbfiles <- function() {
  datadir <- getOption("default.datadir")
  db <- dplyr::src_sqlite(file.path(datadir, "admin", "filelist", "allfiles.sqlite"))
  tab <- dplyr::tbl(db, "file_list") %>% ## split the string search into two simpler parts makes it faster
    filter(fullname %like% "%hdf") %>% 
    filter(fullname %like% "%s6250%") %>% collect() %>% filter(!grepl("LongitudeLatitude", fullname)) %>% mutate(file = fullname, fullname = file.path(datadir, file)) 
  # f1 <- "ftp-projects.zmaw.de/seaice/AMSR-E_ASI_IceConc/no_landmask/hdf/s6250/"
  # f2 <- "www.iup.uni-bremen.de\\+8084/amsr2data/asi_daygrid_swath/s6250"
  # tab <- tab[c(grep(f1, tab$fullname), grep(f2, tab$fullname)), ]
  # 
  tab$date <- as.POSIXct(strptime(unlist(lapply(strsplit(basename(tab$fullname), "-"), "[", 4)), "%Y%m%d"), tz = "UTC")
  #tab <- tab[order(tab$date), ]
  #tab$file <- gsub(paste0(getOption("default.datadir"), "/"), "", tab$fullname)
  #tab[!duplicated(tab$date), ]
  tab %>% arrange(date) %>% distinct(date, .keep_all = TRUE)
}


