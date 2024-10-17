#' Calculate time since melt from transitions data frame #'
#' @param date Date: one or more dates of interest 
#' @param trx : the transitions, in the form of a data.frame, string (giving the file name of the database file), or a connection to the database file #'
#' @return A raster stack, with one layer per date 
#' @noRd
#' @keywords internal
calc_time_since_melt <- function(date, trx) {
  psproj <- "+proj=stere +lat_0=-90 +lat_ts=-70 +lon_0=0 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs"
  ice_template <- raster::raster(nrows = 332, ncols = 316, crs = psproj)
  raster::extent(ice_template) <- c(-3950000, 3950000, -3950000, 4350000)
  
  date <- as.Date(date)
  if (is.character(trx)) {
    dbh <- DBI::dbConnect(duckdb::duckdb(dbdir = trx, read_only = TRUE))
    trx <- dplyr::collect(dplyr::tbl(dbh, "transitions")) ## get the whole table
    DBI::dbDisconnect(dbh, shutdown = TRUE) ## close after use if we were given a filename, but not if we were given the db handle directly
  } else if (inherits(trx, "DBIConnection")) {
    trx <- dplyr::collect(dplyr::tbl(trx, "transitions"))
  }
  ## pre-sort it by idx and date (ascending)
  trx <- trx[order(trx$idx, trx$date, decreasing = FALSE), ]
  
  ## reconstruct land mask
  land_idx <- setdiff(seq_len(332 * 316), unique(trx$idx[!is.na(trx$state)]))
  
  out <- lapply(date, function(d) {
    ## find most recent state of each cell (prior to or at d)
    this <- trx[trx$date <= d, ]
    this <- this[c(which(diff(this$idx) > 0), nrow(this)), ]
    ## that's the last entry prior to or at d
    ## but for cells that are still frozen at date d, when did they next melt? for this, want the first entry after d
    this2 <- trx[trx$date > d, ]
    this2 <- this2[c(1L, which(diff(this2$idx) > 0) + 1L), ]
    time_since_melt <- ice_template
    tv <- rep(NA_integer_, prod(dim(time_since_melt)))
    melted_idx <- which(this$state == -1)
    tv[this$idx[melted_idx]] <- d - this$date[melted_idx]
    ## open water, never frozen
    ow_idx <- this$idx[which(this$state == -2)]
    tv[ow_idx] <- 2^15 - 3L ## 32765 = open ocean zone
    melt_later_idx <- which(this2$state == -1 & !this2$idx %in% this$idx[melted_idx]) ## i.e. melted in future, not already melted
    tv[this2$idx[melt_later_idx]] <- d - this2$date[melt_later_idx]
    pf_idx <- this$idx[which(this$state >= 1 & !this$idx %in% this2$idx[melt_later_idx])] ## permanently frozen over the full data record
    tv[pf_idx] <- -2^15 ## -32768 = never-melted ice
    ##tv[this$idx[is.na(this$state)]] <- 199 ## no transitions observed
    tv[intersect(land_idx, which(is.na(tv)))] <- 2^15 - 2L ## 32766 = land
    ## previously we used 32767 = missing data, NA, when exporting to netcdf
    raster::values(time_since_melt) <- tv
    time_since_melt
  })
  raster::stack(out)
}





##' Read data from derived sea ice data products.
##'
##'
##' Derived sea ice data is read from files managed by \code{\link{derivicefiles}}.
##'
##' Currently available products are
##'
##' \describe{
##' }
##'
##' Dates are matched to file names by finding the nearest match in
##' time within a short duration. If \code{date} is greater than
##' length 1 then the sorted set of unique matches is returned.
##' @param date date or dates of data to read, see Details
##' @param time.resolution time resoution data to read, daily or monthly
##' @param product choice of sea ice product, see Details
##' @param xylim spatial extents to crop from source data, can be anything accepted by \code{\link[raster]{extent}}
##' @param latest if TRUE and date input missing, return the latest time available, otherwise the earliest
##' @param returnfiles ignore options and just return the file names and dates
##' @param ... passed to brick, primarily for \code{filename}
##' @param inputfiles input the file set to avoid rescanning that (for extract point-in-time)
##' @details 
##' time_since_melt
##' 
##' 32767 (treated as missing data)
##' 
##' 32766 = land
##' 
##' 32765 = open-ocean zone
##' 
##' -32768 = ice that hasn't melted during the data period
##' 
##' In terms of missing data 32767,  in the nc file, so should be NA once read into R): these are 
##' either open water in the sea ice zone that hasn't re-frozen during the data period, or missing sea ice data that 
##' couldn't be interpolated. 
##' @export
##' @return \code{\link[raster]{raster}} object
##' @seealso \code{\link{derivicefiles}} for details on the repository of
##' data files, \code{\link[raster]{raster}} for the return value
readderivice <- function(date,
                    time.resolution = c("daily"),
                    product = c("time_since_melt"),
                    xylim = NULL,
                
                    latest = TRUE,
                    returnfiles = FALSE, ..., inputfiles = NULL) {
  
  time.resolution <- match.arg(time.resolution)
  product <- match.arg(product)
  
  ## get file names and dates and full path
  if (is.null(inputfiles)) {
    files <- derivicefiles(product = product, ...)
  } else {
    files <- inputfiles
  }
  ##files$fullname <- file.path(datadir, files$file)
  if (returnfiles) return(files)
  if (missing(date)) {
    if (latest) date <- max(files$date)  else date <- min(files$date)
  }

  date <- timedateFrom(date)
  files <- .processFiles(date, files, time.resolution)

  ## doesn't matter which file for "time_since_melt"
  if (product == "time_since_melt") {
    
    out <- raster::stack(lapply(files$date, calc_time_since_melt, trx = files$fullname[1L]))
  } else {
   print("only product 'time_since_melt' currently supported ")    
  }
  setZ(out, files$date)
}




##' Load metadata and location of files of derived sea ice data products.
##'
##' This function loads the latest cache of stored files for
##' ice products, currently only daily NSIDC southern hemisphere is available. 
##' @param product which derived product
##' @param ... reserved for future use, currently ignored
##' @export
##' @return data.frame of \code{file} and \code{date}
derivicefiles <- function(product = "time_since_melt", ...) {
  
 product <- match.arg(product)
 ftx <- raadfiles::get_raad_filenames(all = TRUE)
idx0 <- grep("smmr_ssmi_nasateam/time_since_melt/time_since_melt.duckdb", ftx$file)
 cfiles0 <- file.path(ftx$root[idx0], ftx$file[idx0])
 
if (length(cfiles0) < 1 || !file.exists(cfiles0)) stop("no time_since_melt resource found")

 dbh <- DBI::dbConnect(duckdb::duckdb(dbdir = cfiles0, read_only = TRUE))
 trx <- dplyr::collect(dplyr::arrange(dplyr::distinct(dplyr::tbl(dbh, "transitions"), date), date))
 trx$date <- as.POSIXct(trx$date, tz = "UTC")
 DBI::dbDisconnect(dbh, shutdown = TRUE) ## close after use if we were given a filename, but not if we were given the db handle directly
 trx$fullname <- cfiles0
 trx
}
