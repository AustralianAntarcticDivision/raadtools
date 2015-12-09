##' Read Chlorophyll-a for the Southern Ocean
##'
##' Ocean colour Chlorophyll-a data. Default is to read from the Johnson Improved
##' chlorophyll-a estimates using Southern Ocean-specific calibration
##' algorithms, but the original MODIS and SeaWIFs products are also available via the argument \code{product}.
##'
##' Dates are matched to file names by finding the nearest match in
##' time within a short duration. If \code{date} is greater than
##' length 1 then the sorted set of unique matches is returned.
##'
##' @param date date or dates of data to read, see Details
##' @param time.resolution time resolution data to read, weekly or monthly
##' @param product choice of product, see Details
##' @param platform (modis[a] or seawifs)
##' @param xylim spatial extents to crop from source data, can be anything accepted by \code{\link[raster]{extent}}
##' @param returnfiles ignore options and just return the file names and dates
##' @param verbose print messages on progress etc.
##' @param ... passed to brick, for \code{filename}
##' @references  Johnson, R, PG Strutton, SW Wright, A McMinn, and KM
##' Meiners (2013) Three improved satellite chlorophyll algorithms for
##' the Southern Ocean, J. Geophys. Res. Oceans, 118,
##' doi:10.1002/jgrc.20270
##' \url{http://onlinelibrary.wiley.com/doi/10.1002/jgrc.20270/full}
##'
##' @export
##' @return \code{\link[raster]{raster}} object
##' @seealso \code{\link{chlafiles}} for details on the repository of
##' data files, \code{\link[raster]{raster}} for the return value
##' @examples
##' \dontrun{
##' d <- readchla(c("2003-01-01", c("2003-06-01")),
##'          xylim = extent(100, 150, -70, -30))
##' }
##' @export
readchla <- function(date, time.resolution = c("weekly", "monthly"),
                     product = c("johnson", "oceancolor"),
                     platform = c("MODISA", "SeaWiFS"), 
                     xylim = NULL,
                     
                     ##lon180 = TRUE,
                     returnfiles = FALSE,
                     latest = FALSE,
                     verbose = TRUE,
                     ...) {
  
  warning("readchla is going to be deprecated, it only works with a static collection of data, no longer updated")
  time.resolution <- match.arg(time.resolution)
  product <- match.arg(product)
  files <- chlafiles(time.resolution = time.resolution, product = product)
  if (returnfiles) return(files)
  
  if (missing(date)) date <- min(files$date)
  if (latest) date <- max(files$date)
  date <- timedateFrom(date)
  ## from this point one, we don't care about the input "date" - this is our index into all files and that's what we use
  ##findex <- .processDates(date, files$date, time.resolution)
  files <- .processFiles(date, files, time.resolution)
  
  
  
  rtemplate <- if (product == "oceancolor") raster(files$fullname[1L], band = files$band[1L]) else raster(files$fullname[1L])
  ##if (lon180) rtemplate <- .rotate(rtemplate)
  
  ## process xylim
  cropit <- FALSE
  if (!is.null(xylim)) {
    cropit <- TRUE
    cropext <- extent(xylim)
    ##rtemplate <- crop(rtemplate, cropext)
  }
  
  nfiles <- nrow(files)
  r <- vector("list", nfiles)
  
  for (ifile in seq_len(nfiles)) {
    r0 <- if (product == "oceancolor") raster(files$fullname[ifile], band = files$band[ifile]) else raster(files$fullname[ifile])
    ##if (lon180) r0 <- .rotate(r0)
    if(cropit) r0 <- crop(r0, cropext)
    ## r0[r0 < -2] <- NA
    r[[ifile]] <- r0
    ##if (verbose & ifile %% 10L == 0L) .progressreport(ifile, nfiles)
  }
  
  if (nfiles > 1)
    r <- brick(stack(r), ...)
  else r <- r[[1L]]
  names(r) <- basename(files$fullname)
  if (is.na(projection(r))) projection(r) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0" 
  
  r <- setZ(r, files$date)
  return(r)
}

##' Chlorophyll-a for the Southern Ocean
##'
##' This function generates a list of available chlorophyll-a files, including SeaWiFS and MODIS.
##' @title Chlorophyll-a
##' @param time.resolution weekly (8day) or monthly
##' @param product choice of chla product, see \code{readchla}
##' @param platform (modis[a] or seawifs)
##' @param ... reserved for future use, currently ignored
##' @return data.frame
##' @export
chlafiles <- function(time.resolution = c("weekly", "monthly"),
                      product = c("johnson", "oceancolor"),
                      platform = c("MODISA", "SeaWiFS"), ...) {
  ##datadir <- getOption("default.datadir")
  product <- match.arg(product)
  platform <- match.arg(platform)
  time.resolution <- match.arg(time.resolution)
  time.resolution <- c(weekly = "8d", monthly = "monthly")[time.resolution]
  ftx <- .allfilelist()
  cfiles <- grep("data_local/chl", ftx, value = TRUE)
  cfiles1 <- grep(product, cfiles, value = TRUE)
  cfiles2 <- grep(tolower(gsub("A$", "", platform)), cfiles1, value = TRUE )
  cfiles3 <- grep(time.resolution, cfiles2, value = TRUE)
  dates <- timedateFrom(strptime(substr(basename(cfiles3), 2, 8), "%Y%j"))
  
  
  chlf <- data.frame(fullname= cfiles3, date = dates,  stringsAsFactors = FALSE)[order(dates), ]
  
}

# .updatechlafiles <- function(datadir = getOption("default.datadir"), preferModis = TRUE) {
#   tr <- c(monthly = "monthly", weekly = "8d")
#   
#   
#   ## first johnson
#   for (i in seq_along(tr)) {
#     dirpath <- file.path("chl", "johnson", c("modis", "seawifs"), tr[i])
#     
#     fs <- gsub(datadir, "", list.files(file.path(datadir, dirpath), full.names = TRUE))
#     fs <- gsub("^/", "", fs)
#     
#     if (!length(fs) > 0) {
#       warning(sprintf("no files fould for %s at %s", tr[i], dirpath))
#       next;
#     }
#     dates <- timedateFrom(strptime(substr(basename(fs), 2, 8), "%Y%j"))
#     
#     
#     chlf <- data.frame(file = fs, date = dates,  stringsAsFactors = FALSE)[order(dates), ]
#     ## implementing preferModis
#     dupes <- which(duplicated(chlf$date)) - !preferModis
#     if (length(dupes) > 0) chlf <- chlf[-dupes, ]
#     save(chlf, file = file.path(datadir, "cache", sprintf("johnson_%s_chlafiles.Rdata", names(tr[i]))))
#   }
#   ## now oceancolor
#   
#   for (i in seq_along(tr)) {
#     dirpath <- file.path("chl", "oceancolor", c("modis", "seawifs"), tr[i], "netcdf")
#     
#     fs <- list.files(file.path(datadir, dirpath), full.names = TRUE)
#     
#     if (!length(fs) > 0) {
#       warning(sprintf("no files fould for %s at %s", tr[i], dirpath))
#       next;
#     }
#     xfs <- .expandFileDateList(fs)
#     fs <- gsub(datadir, "", xfs$file)
#     fs <- gsub("^/", "", fs)
#     
#     dates <- xfs$date  ##timedateFrom(strptime(substr(basename(fs), 2, 8), "%Y%j"))
#     chlf <- data.frame(file = fs, date = dates,  band = xfs$band, stringsAsFactors = FALSE)[order(dates), ]
#     ## implementing preferModis
#     dupes <- which(duplicated(chlf$date)) - !preferModis
#     if (length(dupes) > 0) chlf <- chlf[-dupes, ]
#     save(chlf, file = file.path(datadir, "cache", sprintf("oceancolor_%s_chlafiles.Rdata", names(tr[i]))))
#   }
#   
# }
