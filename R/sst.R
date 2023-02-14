numeric_ext <- function(x) {
  if (inherits(x, "Extent")) {
    x <- c(raster::xmin(x), raster::xmax(x), raster::ymin(x), raster::ymax(x))
  }
  x
}


#' Load file names and dates of OISST sea surface temperature data
#' 
#' A data frame of file names and dates
#' @title OISST sea surface temperature files
#' @param time.resolution time resolution read
#' @param ... reserved for future use, currently ignored
#' @return data.frame of file names and dates
#' @export
sstfiles <- function(time.resolution = c("daily","monthly"), varname = c("sst", "ice", "err", "anom"), ...) {
 # datadir <- getOption("default.datadir")
  time.resolution <- match.arg(time.resolution)
 varname <- match.arg(varname)  
  ## daily are spread across files
  if (time.resolution == "daily") {
    
    files <- raadfiles::oisst_daily_files()
    f1 <- files$fullname[1]
    info <- vapour::vapour_raster_info(f1)
    
    vrt_dsn <- vapour::vapour_vrt(f1, sds = varname, projection = "OGC:CRS84", bands = 1)
    ## hack this for now
    
    vrt_dsn <- .dump_md(vrt_dsn)
    ## do it again for in the Band because  <MDI key="units">%</MDI>
    vrt_dsn <- .dump_md(vrt_dsn, in_band = TRUE)
   vrt_dsn <- gsub(">%<", "><", vrt_dsn) ## because <UnitType>%</UnitType>
    template <- stringr::str_replace(vrt_dsn, f1, "%s")
    files$vrt_dsn <- sprintf(rep(template, dim(files)[1L]), files$fullname)
    
  } else {
    
    ## monthly all in one file
    files <- raadfiles::oisst_monthly_files()
    f1 <- files$fullname[1]
    info <- vapour::vapour_raster_info(f1)
    if (!varname == "sst") message("only 'sst' available varname for monthly")
    vrt_dsn <- vapour::vapour_vrt(f1, sds = "sst", projection = "OGC:CRS84", bands = 1)
    vrt_dsn <- .dump_md(vrt_dsn)
    date <- nc_rawdata(f1, "time") * 24 * 3600 + as.POSIXct("1800-1-1 00:00:00", tz = "UTC") 
    # ncmeta::nc_atts(f1, "time")$value
    # $units
    # [1] "days since 1800-1-1 00:00:00"
    # 
    files <- tibble(fullname = files$fullname[1], 
                    date = date, 
                    band = seq_along(date), 
                    vrt_dsn = stringr::str_replace(vrt_dsn, "<SourceBand>1</SourceBand>", sprintf("<SourceBand>%i</SourceBand>", 
                                                                                                   seq_along(date))))
}
  files
}


#' Read OISST sea surface temperature data
#' 
#' SST data read from files managed by
#' \code{\link{sstfiles}}. Dates are matched to file names by finding
#' the nearest match in time within a short duration. If \code{date}
#' is greater than length 1 then the sorted set of unique matches is
#' returned.
#' see Details in raadtools 
#' @inheritParams raadtools
#' @inheritDotParams raadtools
#' @param varname variable to return from the data files, default is
#' "sst" or "anom", "err", "ice"

#' @param readall FALSE by default
#' @export
#' @return \code{\link[raster]{raster}} object
#' @seealso \code{\link{icefiles}} for details on the repository of
#' data files, \code{\link[raster]{raster}} for the return value
#' @examples
#' ## read one time slice and plot it up in preparation for reading a time series
#' d <- readsst()
#' plot(d)
#' ## this step is interactive, draw a boundary on the plot
#' ext <- drawExtent()
#' ## these can be created manually with xmin,xmax,ymin,ymax
#' ## ext <- extent(-100, 150, -75, -30)
#' ## now read a big chunk of data for this small region
#' dts <- seq(as.Date("2001-01-03"), by = "1 week", length = 100)
#' sst <- readsst(dts, xylim = ext)
#' 
readsst <-  function (date, time.resolution = c("daily", "monthly"),
                      xylim = NULL, lon180 = TRUE,
                      varname = c("sst", "anom", "err", "ice"),
                      setNA = TRUE,
                      latest = TRUE,
                      returnfiles = FALSE,  ..., inputfiles = NULL) {
  if (is.null(xylim) && lon180) xylim <- c(-180, 180, -90, 90)
  time.resolution <- match.arg(time.resolution)
  
  varname <- match.arg(varname)
  ## if (time.resolution == "monthly") stop("sorry, no monthly SST at the moment")
  if (is.null(inputfiles)) {
    files <- sstfiles(time.resolution = time.resolution, varname = varname)
  } else {
    files <- inputfiles
  }

  if (missing(date)) {
    allfiles <- TRUE
    date <- if (latest) max(files$date) else min(files$date)
  }
  
  
  subfiles <- .processFiles(date, files, time.resolution)
  if (returnfiles) {
    if (allfiles) {
      return(files) 
    } else {
        return(subfiles)
    }
  }
  files <- subfiles
  
  nfiles <- nrow(files)
  ## progress
  pb <- progress::progress_bar$new(
    format = "  extracting [:bar] :percent in :elapsed",
    total = nfiles, clear = FALSE, width= 60)
  pb$tick(0)


 
  ## TODO determine if we need to rotate, or just shift, or not anything
  if (!"band" %in% names(files)) files$band <- 1
  
  if (is.null(xylim)) xylim <- vapour::vapour_raster_info(files$vrt_dsn[1])$extent
  r0 <- brick(stack(lapply(seq_len(nrow(files)), function(xi) read_fun(files$vrt_dsn[xi], ext = xylim, progress  = pb))))
  
  msk <- NULL
  if (time.resolution == "monthly") {
    if (setNA) {
      #msk <- crop_if_needed(rotate_if_needed(raster(file.path(dirname(files$fullname[1]), "lsmask.nc")), rot), xylim)
      msk <- read_fun(vapour::vapour_vrt(file.path(dirname(files$fullname[1]), "lsmask.nc"), projection = "OGC:CRS84"), ext = xylim, progress = NULL)
      msk[msk < 1] <- NA_real_
      r0 <- raster::mask(r0, msk)
    }
  } 
  
  
 
  if (nfiles == 1) r0 <- r0[[1L]]
  r0 <- setZ(r0, files$date)
  
  r0
  
}






