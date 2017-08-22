

## note that this can be replaced by a direct raster(file) once the package
## gets updated (post raster_2.1-49, October 2013)
# .readNSIDC <- function(fname, rtemplate) {
#   dims <- dim(rtemplate)[1:2]
#   con <- file(fname, open = "rb")
#   trash <- readBin(con, "integer", size = 1, n = 300)
#   dat <- readBin(con, "integer", size = 1, n = prod(dims), endian = "little", signed = FALSE)
#   close(con)
#   r100 <- dat > 250
#   r0 <- dat < 1
#   if (rescale) {
#     dat <- dat/2.5  ## rescale back to 100
#   }
#   if (setNA) {
#     dat[r100] <- NA
#     ##dat[r0] <- NA
#   }
#   
#   # 251  Circular mask used in the Arctic to cover the irregularly-shaped data gap around the pole (caused by the orbit inclination and instrument swath)
#   # 252	Unused
#   # 253	Coastlines
#   # 254	Superimposed land mask
#   # 255	Missing data
#   # 
#   ## ratify if neither rescale nor setNA set
#   r <- raster(t(matrix(dat, dims[1])), template = rtemplate)
#   if (!setNA && !rescale) {
#     ##r <- ratify(r)
#     rat <- data.frame(ID = 0:255, icecover = c(0:250, "ArcticMask", "Unused", "Coastlines", "LandMask", "Missing"), 
#                       code = 0:255, stringsAsFactors = FALSE)
#     levels(r) <- rat
#     r
#   } else {
#     r
#   }
# }


##' Read data from sea ice data products.
##'
##'
##' Sea ice data is read from files managed by \code{\link{icefiles}}.
##'
##' Currently available products are
##'
##' \describe{
##' \item{'nsidc'}{daily or monthly NSIDC concentration data, processed by the SMMR/SSMI NASA Team}
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
                    time.resolution = "daily",
                    product = "nsidc",
                    hemisphere = c("south", "north"), 
                    xylim = NULL,
                    setNA = TRUE, rescale = TRUE, 
                    latest = FALSE,
                    returnfiles = FALSE,  ..., inputfiles = NULL) {
  
#  time.resolution <- match.arg(time.resolution)
 product <- match.arg(product)
 hemisphere <- match.arg(hemisphere)
  
  if (!is.null(inputfiles)) {
    files <- inputfiles
  } else {
    ## get file names and dates and full path
    files <- switch(hemisphere, 
                    north = raadfiles::nsidc_north_daily_files(), 
                    south = raadfiles::nsidc_south_daily_files())
  }
  if (returnfiles) return(files)
  # if (product == "amsr" & .Platform$OS.type == "windows") warning("sorry, AMSR2 files are HDF4 so this is unlikely to work on your machine")
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
  
  res <-  c(25000, 25000)
  
  #if (product == "ssmi") stop("sorry SSMI data is temporarily unavailable")
  
  rtemplate <- raster(extent(ext), nrows =  nsidcdims[1L], ncols = nsidcdims[2L], crs = prj)
  ## process xylim
  cropit <- FALSE
  if (!is.null(xylim)) {
    cropit <- TRUE
    cropext <- extent(xylim)
  }
  
  nfiles <- nrow(files)
  r <- vector("list", nfiles)
  
  fname <- files$fullname
  r <- vector("list", length(fname))
  for (ifile in seq_len(nfiles)) {
    
    con <- file(fname[ifile], open = "rb")
    trash <- readBin(con, "integer", size = 1, n = 300)
    dat <- readBin(con, "integer", size = 1, n = prod(nsidcdims), endian = "little", signed = FALSE)
    close(con)
    r100 <- dat > 250
    #r0 <- dat < 1
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
    r0 <- raster(t(matrix(dat, nsidcdims[1])), template = rtemplate)
    if (!setNA && !rescale) {
      ##r <- ratify(r)
      rat <- data.frame(ID = 0:255, icecover = c(0:250, "ArcticMask", "Unused", "Coastlines", "LandMask", "Missing"), 
                        code = 0:255, stringsAsFactors = FALSE)
      levels(r0) <- rat
    } 
  
    
    if (cropit) r0 <- crop(r0, cropext)
    r[[ifile]] <- r0
  }
  r <- stack(r)

  projection(r) <- prj
  names(r) <- basename(files$file)
  r <- setZ(r, files$date)

  if ("filename" %in% names(list(...))) r <- writeRaster(r, ...)
  r
}



##' Load metadata and location of files of sea ice data products.
##'
##' This function loads the latest cache of stored files for
##' ice products.
##' @param time.resolution daily or monthly files?
##' @param product choice of sea ice product, see \code{\link{readice}}
##' @param hemisphere north or south
##' @param ... reserved for future use, currently ignored
##' @export
##' @importFrom raadfiles nsidc_south_monthly_files nsidc_north_monthly_files 
##' @examples
##' \dontrun{
##' icf <- icefiles(time.resolution = "monthly")
##' icf[which.min((as.Date("1995-01-01") + runif(1, -4000, 4000)) - as.Date(icf$date), ]
##' }
##' @return data.frame of \code{file} and \code{date}
icefiles <- function(time.resolution = "daily", 
                     product = "nsidc", hemisphere =c("south", "north"), ...) {
  
  datadir <- getOption("default.datadir")
  if (product != "nsidc") stop("readice no longer supports AMSR or SSM/I, see specfic read functions for those")
  if (time.resolution != "daily") stop("readice no longer supports monthly time resolution, see specific read function for monthly data")
  
  hemisphere <- match.arg(hemisphere)
  
  files <- switch(hemisphere, 
                  north = raadfiles::nsidc_north_daily_files(), 
                  south = raadfiles::nsidc_south_daily_files())
  files

}
# 
# ## system.time(icf <- icefiles(hemisphere = "south"))
# #user  system elapsed 
# #3.060   0.028   3.138
# # unique(dirname(dirname(icf$fullname)))
# # [1] "/rdsi/PRIVATE/raad/data/sidads.colorado.edu/pub/DATASETS/nsidc0051_gsfc_nasateam_seaice/final-gsfc/south/daily"
# # [2] "/rdsi/PRIVATE/raad/data/sidads.colorado.edu/pub/DATASETS/nsidc0081_nrt_nasateam_seaice"                        
# # [3] "/rdsi/PRIVATE/raad/data/sidads.colorado.edu/pub/DATASETS/nsidc0081_nrt_nasateam_seaice/F18_uncalibrated"    
# 
# ## system.time(icf2 <- .nsidc_south_daily_dbfiles())
# .nsidc_south_daily_dbfiles <- function() {
#   datadir <- getOption("default.datadir")
#   db <- dplyr::src_sqlite(file.path(datadir, "admin", "filelist", "allfiles.sqlite"))
#   tab <- dplyr::tbl(db, "file_list") %>% ## split the string search into two simpler parts makes it faster
#     filter(fullname %like% "%s.bin") %>% 
#     filter(fullname %like% "%nasateam_seaice%") %>% 
#     collect() %>% filter(!grepl("monthly", fullname)) %>% 
#     #filter(grepl("nrt_s.bin", fullname) | grepl("v1.1_s.bin", fullname)) %>% 
#     mutate(file = fullname, fullname = file.path(datadir, file)) %>% 
#     mutate(date = as.POSIXct(strptime(basename(fullname), "nt_%Y%m%d"), tz = "GMT"))
#   tab %>% arrange(date) %>% distinct(date, .keep_all = TRUE)
# }
