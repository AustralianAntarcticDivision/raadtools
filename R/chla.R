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
##' @param product choice of product, see Details
##' @param xylim spatial extents to crop from source data, can be anything accepted by \code{\link[raster]{extent}}
#'  @param algorithm johnson or nasa
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
#' @export
readchla <- function(date, product = c("MODISA", "SeaWiFS"),
                     xylim = NULL,
                     algorithm = c("johnson", "nasa")) {
  product <- match.arg(product)
  d <- readchla_mean(date, product = product, xylim = xylim)

  algorithm <- match.arg(algorithm)  
  thename <- sprintf("chla_%s", algorithm)
  d <- d[, c("bin_num", thename)]
  names(d) <- c("bin_num", "value")
  NROWS <- product2nrows(product)
  gridmap <- raster::raster(raster::extent(-180, 180, -90, 90), ncol = NROWS * 2, nrow = NROWS, crs = "+init=epsg:4326")
  if (!is.null(xylim)) gridmap <- crop(gridmap, xylim)
  bin_chl(d$bin_num, d$value, NROWS, gridmap)

}

bin_chl <- function(bins, value, NROWS, gridmap) {
  bins <- tibble(bin_num = bins, value = value)
  ll <- coordinates(gridmap)
  bins <- tibble(bin_num = roc::lonlat2bin(ll[,1], ll[, 2], NUMROWS = 4320), gridcell = seq(ncell(gridmap))) %>% 
    dplyr::inner_join(bins, "bin_num")
  gridmap[bins$gridcell] <- bins$value
  trim(gridmap)
}


#' @export
readchla_mean <- function(date,
#                     algorithm = c("johnson", "oceancolor"),
                     product = c("MODISA", "SeaWiFS"), 
                     xylim = NULL,
                     returnfiles = FALSE,
                     latest = TRUE,
                     verbose = TRUE,
                     ...) {
  largs <- list(...)
  if (missing(date)) {
      files <- oc_sochla_files(product = product)
      date <- if (latest) max(files$date) else min(files$date)
  }  
  
  ## here read_oc_sochla should take the bins it needs
  init <- roc::initbin(product2nrows(product))
  bin_sub <- tibble::tibble(bin_num = roc::crop_init(init, xylim))
  if ("time.resolution" %in% names(largs)) stop("time.resolution is not supported, enter the dates directly - underlying temporal resolution is daily")
 # bins <- purrr::map_df(date, read_oc_sochla, bins = bin_sub, product = product) %>% 
  
   bins <- read_oc_sochla(date, bins = bin_sub, product = product) %>% 
    dplyr::select(-date) %>% 
    dplyr::group_by(bin_num) %>% 
    dplyr::summarize_all(mean)
  
  bins
  
}  


##' @export
readchla_old <- function(date, time.resolution = c("weekly", "monthly"),
                         product = c("johnson", "oceancolor"),
                         platform = c("MODISA", "SeaWiFS"), 
                         xylim = NULL,
                         
                         ##lon180 = TRUE,
                         returnfiles = FALSE,
                         latest = FALSE,
                         verbose = TRUE,
                         ...) {
  
  ## Note, fixing this can used the old updatechlafiles below
  warning("readchla only works with a static collection of data, no longer updated")
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
  
  #if (product == "oceancolor") stop('sorry MODISA not currently supported')
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
  cfiles3 <- grep("nc$", cfiles3, value = TRUE)
  dates <- timedateFrom(strptime(substr(basename(cfiles3), 2, 8), "%Y%j"))
  if (product == "oceancolor") {
    return(cfiles3)
    xfs <- .expandFileDateList(cfiles3)
    # nc <- ncdf4::nc_open(cfiles3)
    #  dates <- 
    # dates <- timedateFrom(strptime(substr(basename(cfiles3), 2, 8), "%Y%j"))
    dates <- xfs$date
    cfiles3 <- xfs$fullname
  }
  chlf <- data.frame(fullname= cfiles3, date = dates,  stringsAsFactors = FALSE)[order(dates), ]
  chlf
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
