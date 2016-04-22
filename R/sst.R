
##' Load file names and dates of OISST sea surface temperature data
##'
##' A data frame of file names and datres
##' @title OISST sea surface temperature files
##' @param time.resolution time resolution read
##' @param ... reserved for future use, currently ignored
##' @return data.frame of file names and dates
##' @export
sstfiles <- function(time.resolution = c("daily","monthly"), ...) {
  datadir <- getOption("default.datadir")

  ## data/eclipse.ncdc.noaa.gov/pub/OI-daily-v2/NetCDF/1981/AVHRR
  ## "avhrr-only-v2.19810901.nc"

  time.resolution <- match.arg(time.resolution)

  ftx <- .allfilelist()
  if (time.resolution == "daily") {
      cfiles0 <- grep("eclipse.ncdc.noaa.gov", ftx, value = TRUE)
      cfiles1 <- grep("OI-daily-v2", cfiles0, value = TRUE)
      cfiles <- grep(".nc$", cfiles1, value = TRUE)
      if (length(cfiles) < 1) stop("no files found")

      doffs <-  1
      datepart <- sapply(strsplit(basename(cfiles), "\\."), function(x) x[length(x) - doffs])

      dates <- timedateFrom(as.Date(strptime(datepart, "%Y%m%d", tz = "GMT")))

      cfs <- data.frame(file = gsub(paste(datadir, "/", sep = ""), "", cfiles), date = dates,
                    fullname = cfiles, stringsAsFactors = FALSE)[order(dates), ]
      ## shouldn't be any, but no harm
      fs <- cfs[!duplicated(cfs$date), ]
      #if (nrow(fs) < nrow(cfs)) warning("Some duplicated files in OI-daily-V2 collection? Please report to maintainer. ")
  } else {
      cfiles0 <- grep("ftp.cdc.noaa.gov/Datasets/noaa.oisst.v2/", ftx, value = TRUE)
      cfiles <- grep("sst.mnmean.nc$", cfiles0, value = TRUE)

      if (length(cfiles) < 1) stop("no files found")
      if (length(cfiles) > 1) stop("only expecting one file for monthly OIv2 SST, but found ",length(cfiles))

      r <- stack(cfiles, quick = TRUE)
      fs <- rep(cfiles, nlayers(r))

      dates <- timedateFrom(strptime(names(r), "X%Y.%m.%d"))
      fs <- data.frame(file = gsub("^/", "", gsub(datadir, "", fs)), date = dates, fullname = cfiles, stringsAsFactors = FALSE)[order(dates),]
      fs$band <- seq_len(nlayers(r))
  }
  fs
}




.sstfiles1 <- function (time.resolution = c("daily", "monthly"), fromCache = TRUE,
                        ...)
{
  datadir <- getOption("default.datadir")
  time.resolution <- match.arg(time.resolution)
  if (fromCache) {
    load(file.path(datadir, "cache", sprintf("sstfiles_%s.Rdata",
                                             time.resolution)))
    ## kludge fix
    imatch <- which("files" == names(sstf))
    if (length(imatch) > 0) names(sstf)[imatch] <- "file"
    ###############
    sstf$fullname <- file.path(datadir, sstf$file)
    return(sstf)
  }
  if (time.resolution == "daily") {
    dirpath <- file.path(datadir, "sst", "OI-daily-v2", "daily")
    fs <- list.files(dirpath, pattern = "\\.nc$", recursive = TRUE,
                     full.names = TRUE)
    fsstrings <- as.Date(substr(basename(fs), 15, 22), "%Y%m%d")
    dates <- timedateFrom(as.Date(fsstrings, "%Y%m%d", tz = "UTC"))
    sstf <- data.frame(file = gsub("^/", "", gsub(datadir, "",
                                                  fs)), date = dates, stringsAsFactors = FALSE)[order(dates),
                                                                                                ]
  }
  if (time.resolution == "monthly") {
    dirpath <- file.path(datadir, "sst", "oiv2")
    r <- stack(file.path(dirpath, "sst.mnmean.nc"), quick = TRUE)

    fs <- rep(file.path(dirpath, "sst.mnmean.nc"), nlayers(r))

    dates <- timedateFrom(strptime(names(r), "X%Y.%m.%d"))
    sstf <- data.frame(file = gsub("^/", "", gsub(datadir, "",
                                                  fs)), date = dates, stringsAsFactors = FALSE)[order(dates),
                                                                                                ]
    sstf$band <- seq_len(nlayers(r))
  }

  save(sstf, file = file.path(datadir, "cache", sprintf("sstfiles_%s.Rdata",
                                                        time.resolution)))
  sstf
}

.progressreport <- function(current, finish) {
  cat(paste(rep("\b", 16), collapse = ""))
  cat(sprintf("%6d of %6d", current, finish));
  flush.console()
  invisible(NULL)
}


.readmonsst <- function(date) {
  
}
##' Read OISST sea surface temperature data from daily files
##'
##' SST data read from files managed by
##' \code{\link{sstfiles}}. Dates are matched to file names by finding
##' the nearest match in time within a short duration. If \code{date}
##' is greater than length 1 then the sorted set of unique matches is
##' returned.
##' @param date date or dates of data to read, see Details
##' @param time.resolution time resoution data to read, daily or monthly
##' @param xylim spatial extents to crop from source data, can be anything accepted by \code{\link[raster]{extent}}, see Details
##' @param lon180 defaults to TRUE, to "rotate" Pacific view [0, 360] data to Atlantic view [-180, 180]
##' @param varname variable to return from the data files, default is
##' "sst" or "anom", "err", "ice"
##' @param setNA mask out land values (only applies to monthly time.resolution)
##' @param latest if TRUE return the latest time available, ignoring the 'date' argument
##' @param returnfiles ignore options and just return the file names and dates
##' @param readall FALSE by default
##' @param ... passed in to brick, primarily for \code{filename}
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
                      returnfiles = FALSE, readall = FALSE, ...) {
  time.resolution <- match.arg(time.resolution)
  varname <- match.arg(varname)
  ## if (time.resolution == "monthly") stop("sorry, no monthly SST at the moment")

  files <- sstfiles(time.resolution = time.resolution)
  if (returnfiles)
    return(files)


  if (missing(date)) date <- min(files$date)
  if (latest) date <- max(files$date)
  date <- timedateFrom(date)
  files <- .processFiles(date, files, time.resolution)
  if (readall) {
    files <- sstfiles(time.resolution = time.resolution)
    lon180 <- FALSE
    xylim <- NULL
  }

  cropit <- FALSE
  if (!is.null(xylim)) {
    cropit <- TRUE
    cropext <- extent(xylim)
  }
  nfiles <- nrow(files)
  
  if (time.resolution == "monthly") {
    r0 <- suppressWarnings(stack(files$fullname[1], bands = files$band))
    if (setNA) {
      mm <- raster(file.path(dirname(files$fullname[1]), "lsmask.nc"))
      mm[mm < 1] <- NA_real_
    }
  } else {
    r0 <- suppressWarnings(stack(files$fullname, quick = TRUE, varname = varname))
  }
#   if (nfiles > 1) {
#     r0 <- suppressWarnings(stack(files$fullname, quick = TRUE, varname = varname))
#   } else {
#     bands <- files$band
#     if (is.null(bands)) {
#       r0 <- suppressWarnings(raster(files$fullname, varname = varname))
#     } else {
#       r0 <- suppressWarnings(stack(files$fullname[1], bands = bands))
#       if (setNA) {
#         mm <- raster(file.path(files$fullname[1], "lsmask.nc"))
#         mm[mm < 1] <- NA_real_
#         ##r0 <- mask(r0, mm)
#       }
#     }
#   }
  
  
  ## note that here the object gets turned into a brick,
  ## presumably with a tempfile backing - that's something to think about more
  ## in terms of passing in "filename"
  if (lon180) {
    r0 <- rotate(r0)
    if (setNA & time.resolution == "monthly") mm <- rotate(mm)
  }
  if (cropit) {
    r0 <- crop(r0, cropext, snap = "out")
    if (setNA & time.resolution == "monthly") mm <- crop(mm, cropext, snap = "out")
  }
  if (setNA & time.resolution == "monthly") r0 <- mask(r0, mm)
  if (is.na(projection(r0))) projection(r0) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
  r0 <- setZ(r0, files$date)

  ## need to determine if "filename" was passed in
  dots <- list(...)
  if ("filename" %in% names(dots)) {
    r0 <- writeRaster(r0, ...)
  }

  if (nfiles == 1) r0 <- r0[[1L]]
  r0

}






.readsst1 <- function(date, time.resolution = c("daily", "monthly"), varname = c("sst", "anom", "err", "ice"),
                      xylim = NULL,
                      lon180 = TRUE,
                      returnfiles = FALSE,
                      verbose = TRUE,
                      ...) {

  datadir <- getOption("default.datadir")
  time.resolution <- match.arg(time.resolution)
  varname <- match.arg(varname)

  if (! varname == "sst" & time.resolution == "monthly") stop("only sst available for monthly data")
  files <- .sstfiles1(time.resolution = time.resolution)
  if (returnfiles) return(files)

  if (missing(date)) date <- min(files$date)
  date <- timedateFrom(date)
  ## from this point one, we don't care about the input "date" - this is our index into all files and that's what we use
  ##findex <- .processDates(date, files$date, time.resolution)
  ##date <- files$date[findex]
  files <- .processFiles(date, files, time.resolution)

  ## process xylim
  cropit <- FALSE
  if (!is.null(xylim)) {
    cropit <- TRUE
    cropext <- extent(xylim)
    ##rtemplate <- crop(rtemplate, cropext)
  }

  nfiles <- nrow(files)
  r <- vector("list", nfiles)
  if (time.resolution == "daily") {
    rtemplate <- raster(files$fullname[1L], varname = varname)
    if (lon180) rtemplate <- .rotate(rtemplate)
    for (ifile in seq_len(nfiles)) {
      r0 <- raster(files$fullname[ifile], varname = varname)
      if (lon180) r0 <- .rotate(r0)
      if(cropit) r0 <- crop(r0, cropext)
      r0[r0 < -2] <- NA
      r[[ifile]] <- r0
    }
    ## monthly
  } else {
    ##landmask <- readBin(file.path(datadir, "sst", "oimonth_v2", "lstags.onedeg.dat"), "numeric", size = 4, n = 360 * 180, endian = "big")
    landmask <- raster(file.path(datadir, "sst", "oiv2", "lsmask.nc"))
    for (ifile in seq_len(nfiles)) {
      fname <- files$fullname[ifile]
      ##con <- gzfile(fname, open = "rb")
      ##version <- readBin(con, "integer", size = 4, n = 1, endian = "big")
      ##date <-  readBin(con, "integer", size = 4, n = 7, endian = "big")
      ##d <- readBin(con, "numeric", size = 4, n = 360 * 180, endian = "big")
      ##close(con)
      ## d[d > 500] <- NA
      ##d[landmask < 1] <- NA
      ##sst <- flip(raster(t(matrix(d,   360, 180)),
      ##                   xmn = 0, xmx = 360, ymn = -90, ymx = 90,
      ##                   crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"), "y")
      sst <- raster(fname, band = files$band[ifile])
      sst[!landmask] <- NA
      if (lon180) sst <- .rotate(sst)
      if (cropit) sst <- crop(sst,cropext)
      r[[ifile]] <- sst
    }



  }

  if (nfiles > 1) r <- brick(stack(r), ...) else r <- r[[1L]]
  names(r) <- paste("sst", time.resolution, format(files$date, "%Y%m%d"), sep = "_")
  r <- setZ(r, files$date)

  return(r)


}

