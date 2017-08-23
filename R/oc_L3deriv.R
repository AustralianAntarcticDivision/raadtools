#' Southern Ocean Chlorophyll-a 
#' 
#' Read Southern Ocean Chlorophyll-a  in L3 bin form. 
#' 
#' 
#' @details relies on processing done here https://github.com/AustralianAntarcticDivision/ocean_colour
#' @param date date/s to read
#' @param time.resolution daily for now
#' @param xylim extent in longitude latitude using 'raster::extent'
#' @param latest return only the latest day
#' @param returnfiles give the files instead of the data 
#' @param ... ignored
#' @param inputfiles speed up the read by inputting the files
#'
#' @return tibble of the values in L3 bin form
#' @export
read_oc_sochla <- function(date, time.resolution = c("daily"), bins = NULL, 
                              product = c("MODISA", "SeaWiFS"), 
                             latest = TRUE, returnfiles = FALSE, ..., inputfiles = NULL) {
  time.resolution <- match.arg(time.resolution)
  if (is.null(inputfiles)) {
  files <- oc_sochla_files(product = product)
  }
  
  if (missing(date)) {
    if (latest) date <- max(files$date) else date <- min(files$date)
  }
  date <- timedateFrom(date)
  files <- files[findInterval(date, files$date), ]
 
  if (returnfiles) return(files)
  purrr::map_df(files$fullname, read_oc_sochla_day, bins = bins)
}
  
product2nrows <- function(x) {
  c(MODISA = 4320, SeaWiFS = 2160)[x]
}
read_oc_sochla_day <- function(file, bins = NULL) {
  d <- readRDS(file)
  if (!is.null(bins)) {
    d <- dplyr::inner_join(d, bins, "bin_num")
  }
  d
}
  
#' Derived ocean colour files
#'
#' For L3 bin derived products. 
#' @param time.resolution temporal resolution (base is daily)
#' @param product which product
#'
#' @return data frame
#' @export
#' @importFrom tibble as_tibble
oc_sochla_files <- function(time.resolution = c("daily"), 
                            product = c("MODISA", "SeaWiFS")) {
  product <- match.arg(product)
  pat <- sprintf("^%s_.*.rds$", c(MODISA = "modis", SeaWiFS = "seawifs")[product])
    fs <- data.frame(fullname = list.files(pattern = pat, file.path(getOption("default.datadir"), 
sprintf("data_local/acecrc.org.au/ocean_colour/%s_daily", c(MODISA = "modis", SeaWiFS = "seawifs")[product])), full.names = TRUE, recursive = TRUE), 
                     stringsAsFactors = FALSE)
    fs$date <- as.POSIXct(strptime(basename(fs$fullname), 
                                   sprintf("%s_%s", c(MODISA = "modis", SeaWiFS = "seawifs")[product], "%Y%j"), tz = "GMT"))
    fs$file <- gsub(sprintf("%s/", getOption("default.datadir")), "", fs$fullname)
    tibble::as_tibble(fs)
}
  
chla_johnsonfiles <- function(...) {
  .Defunct("oc_sochla_files")
}