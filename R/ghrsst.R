


#' GHRSST L4 files
#'
#' Data frame of file names and dates. 
#' @return data.frame
#' @export
ghrsstfiles <- function() {
  ftx <- .allfilelist()
  ##ftp.nodc.noaa.gov/pub/data.nodc/ghrsst/L4/GLOB/JPL/MUR/
  cfiles <- grep("ftp.nodc.noaa.gov", ftx, value = TRUE)
  cfiles1 <- grep("ghrsst", cfiles, value = TRUE)
  cfiles2 <- grep("L4", cfiles1, value = TRUE)
  cfiles3 <- grep("GLOB/JPL/MUR", cfiles2, value = TRUE)
  cfiles4 <- grep("MUR.nc$", cfiles3, value = TRUE)
  ##http://podaac-w10n.jpl.nasa.gov/w10n/allData/ghrsst/data/L4/GLOB/UKMO/OSTIA/2016
  cfiles <- grep("podaac-w10n.jpl.nasa.gov", ftx, value = TRUE)
  cfiles1 <- grep("ghrsst", cfiles, value = TRUE)
  cfiles2 <- grep("L4", cfiles1, value = TRUE)
  cfiles3 <- grep("GLOB/UKMO/OSTIA", cfiles2, value = TRUE)
  cfiles5 <- grep("OSTIA.nc$", cfiles3, value = TRUE)
  files <- data.frame(fullname = c(cfiles4, cfiles5), stringsAsFactors = FALSE)
  files$date <- as.POSIXct(strptime(basename(files$fullname), "%Y%m%d"), tz = "GMT")
  
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

  
  cropit <- FALSE
  if (!is.null(xylim)) {
    cropit <- TRUE
    cropext <- extent(xylim)
  
    }
  nfiles <- nrow(files)
  
  r0 <- stack(lapply(files$fullname, raster, varname = varname, stopIfNotEqualSpaced = FALSE), quick = TRUE)
 ## r0 <- suppressWarnings(stack(files$fullname, quick = TRUE, varname = varname, stopIfNotEqualSpaced = FALSE))

#  library(RNetCDF)
# nc <- open.nc("20150101-JPL-L4UHfnd-GLOB-v01-fv04-MUR.nc")
#   print.nc(nc)
#   lon <- var.get.nc(nc, "lon")
#   lat <- var.get.nc(nc, "lat")
#   abs(mean(diff(lon)) - range(diff(lon)))
#   [1] 9.918511e-04 4.383922e-05
#   
#   
  if (cropit) {
    r0 <- crop(r0, cropext, snap = "out")
   
  
    }
  if (is.na(projection(r0))) projection(r0) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
  r0 <- setZ(r0, files$date)
  
  ## need to determine if "filename" was passed in
  dots <- list(...)
  if ("filename" %in% names(dots)) {
    r0 <- writeRaster(r0, ...)
  }
  
  if (nfiles == 1) r0 <- r0[[1L]]
 
  #wtf
  r0 <- setZ(r0, getZ(x) + ISOdatetime(1981, 1, 1, 0, 0, 0, tz = "GMT")) ##1981-01-01 00:00:00)
   r0
  
}