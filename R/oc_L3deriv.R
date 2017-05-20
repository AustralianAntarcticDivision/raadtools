#' Read Johnson L3 bins
#'
#' @details relies on processing done here 'file.path(getOption("default.datadir"), "data_local/acecrc.org.au/ocean_colour")'
#' @param date date/s to read
#' @param time.resolution daily for now
#' @param xylim ignored
#' @param latest return only the latest day
#' @param returnfiles give the files instead of the data 
#' @param ... ignored
#' @param inputfiles speed up the read by inputting the files
#'
#' @return tibble of the values in L3 bin form
#' @export
#' @examples 
#'  x <- readchla_johnson(date = "2012-01-01")
#'  ##plot(roc::bin2lonlat(x$bin_num, 4320), col = palr::chlPal(x$chla_johnson), pch = ".")
readchla_johnson <- function(date, time.resolution = c("daily"), xylim = NULL, 
                              
                             latest = FALSE, returnfiles = FALSE, ..., inputfiles = NULL) {
  if (is.null(inputfiles)) {
  files <- chla_johnsonfiles()
  }
  if (missing(date)) date <- min(files$date)
  date <- timedateFrom(date)
  files <- files[findInterval(date, files$date), ]
  if (latest) files <- tail(files, 1L)
  if (returnfiles) return(files)
  dplyr::bind_rows(lapply(files$fullname, readRDS))
}
  
  
#' Derived ocean colour files
#'
#' For L3 bin derived products. 
#' @param time.resolution temporal resolution (base is daily)
#' @param product which product
#'
#' @return data frame
#' @export
#'
chla_johnsonfiles <- function(time.resolution = c("daily"), product = "MODISA") {
    if (!product == "MODISA") stop("only MODISA supported currently")
    fs <- data.frame(fullname = list.files(pattern = "^modis_.*.rds$", file.path(getOption("default.datadir"), "data_local/acecrc.org.au/ocean_colour/modis_daily"), full.names = TRUE, recursive = TRUE), 
                     stringsAsFactors = FALSE)
    fs$date <- as.POSIXct(strptime(basename(fs$fullname), "modis_%Y%j"), tz = "GMT")
    fs$file <- gsub(sprintf("%s/", getOption("default.datadir")), "", fs$fullname)
    fs
  }
  