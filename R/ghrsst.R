
# 
# changes in 2015?
# 
# 286
# 20151013-JPL-L4UHfnd-GLOB-v01-fv04-MUR.nc
# 317
# 20151113090000-JPL-L4_GHRSST-SSTfnd-MUR-GLOB-v02.0-fv04.1.nc


#' GHRSST L4 files
#'
#' Data frame of file names and dates. 
#' @return data.frame
#' @export
ghrsstfiles <- function() {
  ftx <- .allfilelist()
  ##ftp://podaac-ftp.jpl.nasa.gov/allData/ghrsst/data/GDS2/L4/GLOB/JPL/MUR/v4.1/
  cfiles <- grep("podaac-ftp.jpl.nasa.gov", ftx, value = TRUE)
  cfiles1 <- grep("ghrsst", cfiles, value = TRUE)
  cfiles2 <- grep("L4", cfiles1, value = TRUE)
  cfiles3 <- grep("GLOB/JPL/MUR", cfiles2, value = TRUE)
  cfiles4 <- grep("v4.1", cfiles3, value = TRUE)
  cfiles5 <- grep("nc$", cfiles4, value = TRUE)
  files <- data.frame(fullname = cfiles5, stringsAsFactors = FALSE)
  files$date <- as.POSIXct(strptime(basename(cfiles5), "%Y%m%d"), tz = "GMT")
  files <- files[order(files$date), ]
  
  files <- files[!rev(duplicated(files[rev(seq(nrow(files))), ]$date)), ]

  files
}

nodc_ghrsstfiles <- function() {
  ftx <- .allfilelist()
  ##ftp.nodc.noaa.gov/pub/data.nodc/ghrsst/L4/GLOB/JPL/MUR/
  cfiles <- grep("ftp.nodc.noaa.gov", ftx, value = TRUE)
  cfiles1 <- grep("ghrsst", cfiles, value = TRUE)
  cfiles2 <- grep("L4", cfiles1, value = TRUE)
  cfiles3 <- grep("GLOB/JPL/MUR", cfiles2, value = TRUE)
#cfiles4 <- grep("MUR.nc$", cfiles3, value = TRUE)
  cfiles4 <- grep("GLOB-v0", cfiles3, value = TRUE)
  cfiles5 <- grep("nc$", cfiles4, value = TRUE)
  files <- data.frame(fullname = cfiles5, stringsAsFactors = FALSE)
  files$date <- as.POSIXct(strptime(basename(cfiles5), "%Y%m%d"), tz = "GMT")
  
 # cfiles4 <- grep("MUR.nc$", cfiles3, value = TRUE)
 #  ##http://podaac-w10n.jpl.nasa.gov/w10n/allData/ghrsst/data/L4/GLOB/UKMO/OSTIA/2016
 #  cfiles <- grep("podaac-w10n.jpl.nasa.gov", ftx, value = TRUE)
 #  cfiles1 <- grep("ghrsst", cfiles, value = TRUE)
 #  cfiles2 <- grep("L4", cfiles1, value = TRUE)
 #  cfiles3 <- grep("GLOB/UKMO/OSTIA", cfiles2, value = TRUE)
 #  cfiles5 <- grep("OSTIA.nc$", cfiles3, value = TRUE)
 #  files <- data.frame(fullname = c(cfiles4, cfiles5), stringsAsFactors = FALSE)
 #  files$date <- as.POSIXct(strptime(basename(files$fullname), "%Y%m%d"), tz = "GMT")
 #  
  files <- files[order(files$date), ]

  ## remove dupes, prefer later versions
  
  files <- files[!rev(duplicated(files[rev(seq(nrow(files))), ]$date)), ]

  
  files
  
  
}
  

#' Read GHRSST
#'
#' @param date datetime
#' @param time.resolution daily
#' @param varname one of "analysed_sst", "analysis_error", "mask", "sea_ice_fraction"
#' @param setNA ignored
#' @param latest return latest available data
#' @param returnfiles return the files
#' @param xylim spatial extents to crop from source data, can be anything accepted by \code{\link[raster]{extent}}
#' @param lon180 currently ignored
#' @param ... pass in filename to writeRaster
#'
#' @return RasterStack or RasterLayer
#' @export
readghrsst  <- function (date, time.resolution = c("daily"),
                         xylim = NULL, lon180 = TRUE,
                         varname = c("analysed_sst", "analysis_error", "mask", "sea_ice_fraction"),
                         setNA = TRUE,
                         latest = FALSE,
                         returnfiles = FALSE, ...) {
  if (!lon180) stop("only lon180 is supported")
  time.resolution <- match.arg(time.resolution)
  varname <- match.arg(varname)

  files <- ghrsstfiles()
  if (returnfiles)  return(files)
  
  
  if (missing(date)) date <- min(files$date)
  if (latest) date <- max(files$date)
  date <- timedateFrom(date)
  files <- .processFiles(date, files, time.resolution)

  

  nfiles <- nrow(files)
  
  pb <- progress::progress_bar$new(
    format = "  extracting [:bar] :percent in :elapsed",
    total = nfiles, clear = FALSE, width= 60)
  pb$tick(0)
  read_fun <- function(xfile, ext, varname = "", band = 1) {
    pb$tick()
    crop_if_needed(setExtent(raster(xfile, varname = varname), extent(-180, 180, -90, 90)), ext)
  }
   r0 <- stack(lapply(seq_len(nfiles), function(xi) read_fun(files$fullname[xi], ext = xylim, varname = varname)))
  if (is.na(projection(r0))) projection(r0) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
  r0 <- setZ(r0, files$date)
  
  ## need to determine if "filename" was passed in
  dots <- list(...)
  if ("filename" %in% names(dots)) {
    r0 <- writeRaster(r0, ...)
  }
  
  if (nfiles == 1) {
    r0 <- r0[[1L]]
    r0 <- setZ(r0, files$date)
  }
 
  #wtf
 # r0 <- setZ(r0, getZ(r0) + ISOdatetime(1981, 1, 1, 0, 0, 0, tz = "GMT")) ##1981-01-01 00:00:00)
   r0
  
}