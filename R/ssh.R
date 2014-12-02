
##' Load file names and dates of AVISO SSH/SSHA data
##'
##' A data.frame of file names and dates
##' @title AVISO sea surface height / anomaly files
##' @param time.resolution time resolution to find
##' @param ssha logical value, return absolute (SSH) or relative (SSHA anomaly) values
##' @param ... reserved for future use, currently ignored
##' @seealso \code{\link{readssh}}
##' @return data.frame of file names and dates
##' @export
sshfiles <- function(time.resolution = c("daily", "monthly", "monthly_clim", "seasonal_clim"), ssha = FALSE, ...) {
  datadir <- getOption("default.datadir")
  product <- if(ssha) "msla" else "madt"
  ##ftp.aviso.altimetry.fr/global/near-real-time/grids/madt/all-sat-merged/h
  ##ftp.aviso.altimetry.fr/global/delayed-time/grids/madt/all-sat-merged/h
  time.resolution <- match.arg(time.resolution)
  
  tpat <- switch(time.resolution, 
                 daily = "/h/", 
                 monthly = "monthly_mean", 
                 monthly_clim = "monthly_clim",
                 seasonal_clim = "seasonal_clim"
  )
  if (time.resolution != "daily" & product == "madt") stop("ssha=FALSE is only compatible with daily time resolution")
  ftx <- .allfilelist()
  cfiles0 <- grep("ftp.aviso.altimetry.fr", ftx, value = TRUE)
  cfiles1 <- grep(product, cfiles0, value = TRUE)
  cfiles2 <- grep(tpat, cfiles1, value = TRUE)
  cfiles <- grep(".nc$", cfiles2, value = TRUE)
  
  
  ## daily: dt_global_allsat_msla_h_19930101_20140106.nc
  ## monthly_mean: dt_global_allsat_msla_h_y1993_m01.nc
  ## monthly_clim: dt_global_allsat_msla_h_y1993_2013_m01.nc
  ## seasonal_clim: dt_global_allsat_msla_h_y1993_2013_m10_12.nc
  if (length(cfiles) < 1) stop("no files found")
  
  doffs <- if(time.resolution == "monthly_clim") 0 else 1
  datepart <- sapply(strsplit(basename(cfiles), "_"), function(x) x[length(x) - doffs])
  
  
  if (time.resolution == "daily") {
    dates <- timedateFrom(as.Date(strptime(datepart, "%Y%m%d")))
    ## just the last one
    nas <- is.na(dates[length(dates)])
    if (nas) dates[length(dates)] <- max(dates, na.rm = TRUE) + 24 * 3600
  } else if (time.resolution == "monthly") {
    dateparts <- sapply(strsplit(basename(cfiles), "_"), function(x) paste(c(tail(x, 2), "01"), collapse = " "))
    dates <- as.POSIXct(strptime(dateparts, "y%Y m%m.nc %d"))
    
  } else {
    m <- as.numeric(substr(datepart, 2, 3))
    dates <- ISOdatetime(1993, m, 1, 0, 0, 0, tz = "GMT")
  }
  
  cfs <- data.frame(file = gsub(paste(datadir, "/", sep = ""), "", cfiles), date = dates,
                    fullname = cfiles, stringsAsFactors = FALSE)[order(dates), ]
  ## drop any duplicated, this occurs with the delayed/near-real time update
  cfs <- cfs[!duplicated(cfs$date), ]
  
  cfs
}




##' Sea surface height/anomaly
##'
##' Details
##' @title read SSH/A
##' @param date date or dates of data to read, see Details
##' @param time.resolution time resolution to read
##' @param xylim spatial extents to crop from source data, can be anything accepted by \code{\link[raster]{extent}}, see Details
##' @param lon180 defaults to TRUE, to "rotate" Pacific view [0, 360] data to Atlantic view [-180, 180]
##' components, in degrees (0 north, 90 east, 180 south, 270 west)
##' @param ssha logical, to optionally return anomaly or height
##' @param latest if TRUE return the latest time available, ignoring the 'date' argument
##' @param returnfiles ignore options and just return the file names and dates
##' @param verbose print messages on progress etc.
##' @param ... passed to brick, primarily for \code{filename}
##' @export
##' @return data.frame
readssh <- function (date, time.resolution = c("daily", "monthly", "monthly_clim", "seasonal_clim"),
                     xylim = NULL, lon180 = TRUE, ssha = FALSE,
                     latest = FALSE,
                     returnfiles = FALSE, verbose = TRUE, ...)
{
  time.resolution <- match.arg(time.resolution)
  
  files <- sshfiles(time.resolution = time.resolution, ssha = ssha)
  if (returnfiles)
    return(files)
  if (missing(date)) date <- min(files$date)
  if (latest) date <- min(files$date)
  if (time.resolution %in% c("monthly_clim", "seasonal_clim")) {
    stop(sprintf("only daily or monthly read currently available for specific date input. \n Use x <- sshfiles(time.resolution = %s) and raster(x$fullname[1]) etc.", time.resolution)) 
  }
  date <- timedateFrom(date)
  files <- .processFiles(date, files, time.resolution)
  
  cropit <- FALSE
  if (!is.null(xylim)) {
    cropit <- TRUE
    cropext <- extent(xylim)
  }
  nfiles <- nrow(files)
  if (nfiles > 1) {
    r0 <- suppressWarnings(stack(files$fullname, quick = TRUE))
  } else {
    r0 <- suppressWarnings(raster(files$fullname, quick = TRUE))
  }
  ## note that here the object gets turned into a brick, 
  ## presumably with a tempfile backing - that's something to think about more
  ## in terms of passing in "filename"
  if (lon180) r0 <- rotate(r0)
  if (cropit) r0 <- crop(r0, cropext)
  
  ## need to determine if "filename" was passed in
  dots <- list(...)
  if ("filename" %in% names(dots)) {
    r0 <- writeRaster(r0, ...)
  }
  
  
  setZ(r0, files$date)
  
}


.sshfiles1 <- function(ssha = FALSE, ...) {
  datadir = getOption("default.datadir")
  product <- if(ssha) "ssha" else "ssh"
  data.source = file.path(datadir, product, "aviso", "upd", "7d")
  cfiles <- list.files(data.source, pattern = ".nc$", full.names = TRUE)
  datepart <- sapply(strsplit(basename(cfiles), "_"), function(x) x[length(x)-1])
  currentdates <- timedateFrom(as.Date(strptime(datepart, "%Y%m%d")))
  cfs <- data.frame(file = gsub(paste(datadir, "/", sep = ""), "", cfiles), date = currentdates,
                    fullname = cfiles, stringsAsFactors = FALSE)
  ## look at these bad boys
  
  ##696 //aad.gov.au/files/AADC/Scientific_Data/Data/gridded/data/ssh/aviso/upd/7d/nrt_global_merged_madt_h_20130320_20130320_20130326.nc 2013-03-20 11:00:00
  ##697 //aad.gov.au/files/AADC/Scientific_Data/Data/gridded/data/ssh/aviso/upd/7d/nrt_global_merged_madt_h_20130327_20130327_20130402.nc 2013-03-27 11:00:00
  ##698 //aad.gov.au/files/AADC/Scientific_Data/Data/gridded/data/ssh/aviso/upd/7d/nrt_global_merged_madt_h_20130403_20130403_20130406.nc 2013-04-03 11:00:00
  ##699 //aad.gov.au/files/AADC/Scientific_Data/Data/gridded/data/ssh/aviso/upd/7d/nrt_global_merged_madt_h_20130403_20130403_20130409.nc 2013-04-03 11:00:00
  ##700 //aad.gov.au/files/AADC/Scientific_Data/Data/gridded/data/ssh/aviso/upd/7d/nrt_global_merged_madt_h_20130410_20130410_20130416.nc 2013-04-10 10:00:00
  
  cfs[diff(cfs$date) > 0, ]
  
}


.readssh1 <- function (date, time.resolution = "weekly",
                       xylim = NULL, lon180 = TRUE, ssha = FALSE,
                       returnfiles = FALSE, verbose = TRUE, ...)
{
  read0 <- function(x, varname) {
    xtreme <- 20037508
    ytreme <- 16925422
    x <- flip(flip(t(raster(x, varname = varname, stopIfNotEqualSpaced=FALSE)), direction = "y"),
              direction = "x")
    x[x > 9999] <- NA
    extent(x) <- extent(0, xtreme * 2, -ytreme, ytreme)
    projection(x) <- "+proj=merc +ellps=WGS84 +over"
    x
  }
  datadir = getOption("default.datadir")
  time.resolution <- match.arg(time.resolution)
  
  files <- .sshfiles1(ssha = ssha)
  if (returnfiles)
    return(files)
  if (missing(date)) date <- min(files$date)
  date <- timedateFrom(date)
  ##findex <- .processDates(date, files$date, time.resolution)
  files <- .processFiles(date, files, time.resolution)
  
  cropit <- FALSE
  if (!is.null(xylim)) {
    cropit <- TRUE
    cropext <- extent(xylim)
  }
  nfiles <- nrow(files)
  r <- vector("list", nfiles)
  for (ifile in seq_len(nfiles)) {
    r0 <- read0(files$fullname[ifile], varname = "Grid_0001")
    if (lon180)
      r0 <- suppressWarnings(.rotate(r0))
    if (cropit)
      r0 <- crop(r0, cropext)
    r[[ifile]] <- r0
  }
  r <- if (nfiles > 1) brick(stack(r), ...) else r[[1L]]
  setZ(r, files$date)
  
}


