
##' Load file names and dates of OISST sea surface temperature data
##'
##' A data frame of file names and datres
##' @title OISST sea surface temperature files
##' @param time.resolution time resolution read
##' @param ... reserved for future use, currently ignored
##' @return data.frame of file names and dates
##' @export
sstfiles <- function(time.resolution = c("daily","monthly"), ...) {
 # datadir <- getOption("default.datadir")
  time.resolution <- match.arg(time.resolution)
  if (time.resolution == "daily") {
    ## maintain the traditional raad order
    files <- raadfiles::oisst_daily_files()[, c("file", "date", "fullname")]
  } else {
    files <- raadfiles::oisst_monthly_files()[, c("file", "date", "fullname", "band")]
  }
  files
}


##' Read OISST sea surface temperature data
##'
##' SST data read from files managed by
##' \code{\link{sstfiles}}. Dates are matched to file names by finding
##' the nearest match in time within a short duration. If \code{date}
##' is greater than length 1 then the sorted set of unique matches is
##' returned.
##' see Details in raadtools 
##' @inheritParams raadtools
#' @inheritDotParams raadtools
##' @param varname variable to return from the data files, default is
##' "sst" or "anom", "err", "ice"

##' @param readall FALSE by default
##' @export
##' @return \code{\link[raster]{raster}} object
##' @seealso \code{\link{icefiles}} for details on the repository of
##' data files, \code{\link[raster]{raster}} for the return value
##' @examples
##' \dontrun{
##' ## read one time slice and plot it up in preparation for reading a time series
##' d <- readsst()
##' plot(d)
##' ## this step is interactive, draw a boundary on the plot
##' ext <- drawExtent()
##' ## these can be created manually with xmin,xmax,ymin,ymax
##' ## ext <- extent(-100, 150, -75, -30)
##' ## now read a big chunk of data for this small region
##' dts <- seq(as.Date("2001-01-03"), by = "1 week", length = 100)
##' sst <- readsst(dts, xylim = ext)
##' }
##'
readsst <-  function (date, time.resolution = c("daily", "monthly"),
                      xylim = NULL, lon180 = TRUE,
                      varname = c("sst", "anom", "err", "ice"),
                      setNA = TRUE,
                      latest = FALSE,
                      returnfiles = FALSE,  ..., inputfiles = NULL) {
  time.resolution <- match.arg(time.resolution)
  varname <- match.arg(varname)
  ## if (time.resolution == "monthly") stop("sorry, no monthly SST at the moment")
  if (is.null(inputfiles)) {
    files <- sstfiles(time.resolution = time.resolution)
  } else {
    files <- inputfiles
  }
  if (returnfiles)
    return(files)
  if (missing(date)) date <- min(files$date)
  if (latest) date <- max(files$date)
  date <- timedateFrom(date)
  files <- .processFiles(date, files, time.resolution)
  nfiles <- nrow(files)
  ## progress
  pb <- progress::progress_bar$new(
    format = "  extracting [:bar] :percent in :elapsed",
    total = nfiles, clear = FALSE, width= 60)
  pb$tick(0)
  read_fun <- function(xfile, ext, msk, rot, varname = "", band = 1) {
    pb$tick()
    mask_if_needed(crop_if_needed(rotate_if_needed(raster(xfile, varname = varname, band = band), rot), ext), msk)
  }
  
  ## TODO determine if we need to rotate, or just shift, or not anything
  rot <- lon180
  msk <- NULL
  if (time.resolution == "monthly") {
    if (setNA) {
      msk <- crop_if_needed(rotate_if_needed(raster(file.path(dirname(files$fullname[1]), "lsmask.nc")), rot), xylim)
      msk[msk < 1] <- NA_real_
    }
    #r0 <- stack(files$fullname[1], bands = files$band)
  } 
  
  
  if (!"band" %in% names(files)) files$band <- 1
  
  
  r0 <- brick(stack(lapply(seq_len(nrow(files)), function(xi) read_fun(files$fullname[xi], ext = xylim, msk = msk, rot = rot, varname = varname, band = files$band[xi]))),
              ...)
  if (is.na(projection(r0))) projection(r0) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
  
  if (nfiles == 1) r0 <- r0[[1L]]
  r0 <- setZ(r0, files$date)
  
  r0
  
}






