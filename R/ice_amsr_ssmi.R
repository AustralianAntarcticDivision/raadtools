#' Read AMSR sea ice data. 
#' 
#' Both eras are available in the same series, scaling is applied to the first
#' series to ensure the data is always in percentage (0, 100). 
#' 
#' @seealso raadfiles::amsr_daily_files 
#' @inheritParams readice
#' @export
read_amsr_ice <- function(date, xylim = NULL, latest = TRUE, ...) {
  files <- raadfiles::amsr_daily_files()
  if (missing(date)) {
     date <- if (latest) max(files$date) else min(files$date)
  }
  files <- .processFiles(date, files, "daily")
  prj  <- "+proj=stere +lat_0=-90 +lat_ts=-70 +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs " 
  ext <- raster::extent(-3950000, 3950000, -3950000, 4350000)
  
  out <- raster::brick(purrr::map(files$fullname, .readAMSR))
  raster::projection(out) <- prj

  raster::setExtent(raster::setZ(out, files$date), ext)
}
# 
 .readAMSR <- function(fname) {
 
   x <- flip(raster(fname), direction = "y")
  ## earlier files were 0,1
   if (grepl("asi.nl.s6250", basename(fname))) x <- x * 100
   x
 }
# .readSSMI <- function(fname) {
#   x <- raster(fname, varname = "concentration")
#   x <- flip(x, "y")
#   if (!setNA) {
#     x[is.na(x)] <- -127
#   } else {
#     x[x > 100 | x < 1] <- NA
#   }
#   extent(x) <- extent(rtemplate)
#   x
# }

# old_ssmi_amsr_logic <- function() {
#   ftx <- .allfilelist(rda = TRUE, fullname = FALSE)
#   ## just shortcut here for AMSR (need to review code below)
#   if (product == "amsr") return(.amsr625files(ftx, "hdf"))
#   ppat <- switch(product, 
#                  nsidc = "sidads.colorado.edu",
#                  ## need to use the + for some reason
#                  amsr = "www.iup.uni-bremen.de\\+8084")
#   strpat <- switch(product, 
#                    nsidc = "nt_", 
#                    amsr = "AMSR2")
#   
#   epat <- switch(product, 
#                  nsidc = ".bin$", 
#                  
#                  amsr = ".hdf$")  ## this is ignored
#   if (product == "amsr" & hemisphere != "south") stop("no north hemisphere for amsr")
#   cfiles0 <- grep(ppat, ftx, value = TRUE)
#   cfiles1 <- if(product == "nsidc") {
#     c(grep(time.resolution, cfiles0, value = TRUE), grep("_nrt_", cfiles0, value = TRUE))
#   }  else {
#     cfiles0
#   }
#   cfiles2 <- if(product == "nsidc") grep(hemisphere, cfiles1, value = TRUE) else cfiles1
#   
#   cfiles3 <- grep(strpat, cfiles2, value = TRUE)
#   cfiles <- grep(epat, cfiles3, value = TRUE)
#   
#   if (length(cfiles) < 1) stop("no files found")
#   
#   doffs <- if(product == "nsidc") 3 else 1
#   sep <- if(product == "nsidc") "_" else "-"
#   datepart <- sapply(strsplit(basename(cfiles), sep), function(x) x[length(x) - doffs])
#   
#   
#   datepat <-  "%Y%m%d"
#   if (time.resolution == "monthly") datepart <- sprintf("%s01", datepart)
#   dates <- timedateFrom(as.POSIXct(strptime(datepart, datepat, tz = "GMT")))
#   
#   nas <- is.na(dates)
#   dates <- dates[!nas]
#   cfiles <- cfiles[!nas]
#   
#   cfs <- data.frame(file = cfiles,
#                     #file = gsub(paste(datadir, "/", sep = ""), "", cfiles), 
#                     date = dates,
#                     fullname = file.path(datadir, cfiles), 
#                     stringsAsFactors = FALSE)[order(dates), ]
#   
#   cfs <- cfs[!duplicated(cfs$date), ]
#   
#   cfs
# }