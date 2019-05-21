
# importFrom utils tail
.snapout1 <- function(x, min, max) {
  if (x[1] > min) x <- c(x[1] - 1, x)
  if (tail(x, 1L) < max) x <- c(x, tail(x, 1) + 1)
  x
}
# duplicated from sosoc/croc
.seqfl <- function(fl) seq(fl[1], fl[length(fl)])

#' Crop L3 init object with an extent
#' 
#' Crop L3 list, returns bins that fall within the extent. 
#' 
#' duplicated from sosoc/croc where it is called crop_init
#' @param x .init_bin object
#' @param ext raster extent object, or object to create an extent from
#' @return  integer vector of bins
#' @examples
#' \dontrun{ init <- .init_bin(24)
#' .crop_init(init, extent(100, 110, -50, -45))
#' }
#' @noRd
#' @importMethodsFrom raster extent
.crop_init <- function(x, ext) {
  ext <- extent(ext)
  nrows <- length(x$basebin)
  ilat <- which(x$latbin >= raster::ymin(ext) & x$latbin <= raster::ymax(ext) )
  ilat <- .snapout1(ilat, 1L, nrows)
  
  basebin <- x$basebin[ilat]
  latbin <- x$latbin[ilat]
  listofbins <- vector("list", length(basebin))
  for (i in seq_along(basebin)) {
    firstbin <- .lonlat2_bin(raster::xmin(ext), latbin[i], nrows)
    lastbin <- .lonlat2_bin(raster::xmax(ext), latbin[i], nrows)
    firstlast <- .snapout1(c(firstbin, lastbin), basebin[i], basebin[+1] - 1)
    listofbins[[i]] <- .seqfl(firstlast)
  }
  
  listofbins <- unlist(listofbins)
  listofbins 
}


#' Latitude to row
#'
#' duplicated from sosoc/croc where it is called .lat2row
#' @param lat latitude
#' @param NUMROWS number of rows in the grid
#'
#' @noRd
.lat2_row <- function(lat, NUMROWS) {
  row <- as.integer((90 + lat) * NUMROWS/180.0)
  row[row >= NUMROWS] <- NUMROWS - 1;
  row + 1
}


#' Generate bin number from longitude latitude. 
#' 
#' Bin number from longitude and latitude for a given grid with NUMROWS unique latitudes. 
#' duplicated from sosoc/croc where it is called lonlat2bin
#' @param lon longitude
#' @param lat latitude
#' @param NUMROWS number of rows
#' @noRd
.lonlat2_bin <- function(lon, lat, NUMROWS) {
  ibin <- .init_bin(NUMROWS)
  row <- .lat2_row(lat, NUMROWS)
  col <- (lon + 180) * ibin$numbin[row] / 360
  ##col[col >= ibin$numbin[row]] <- ibin$numbin[row] - 1
  as.integer(ibin$basebin[row] + col)
}


' Initialize values for a particular binning
#' 
#' Set up the basic values for the bin scheme for given number of rows. 
#' 
#' duplicated from sosoc/croc where it is called initbin
#' @param NUMROWS relevant number of L3 bin rows
#' @noRd
.init_bin <- function(NUMROWS = 2160) {
  ## TODO options for lon-lat sub-sets
  latbin <- (((seq(NUMROWS) - 1) + 0.5) * 180 / NUMROWS ) - 90
  ## this will overflow at 2^31-1
  #numbin <- as.integer(2 * NUMROWS * cos(latbin * pi/180) + 0.5)
  numbin <- trunc(2 * NUMROWS * cos(latbin * pi/180) + 0.5)
  basebin <- cumsum(c(1L, numbin[-length(numbin)]))
  totbins = basebin[NUMROWS] + numbin[NUMROWS] - 1
  list(latbin = latbin, numbin = numbin, basebin = basebin, totbins = totbins)
}


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
  } else {
    files <- inputfiles
  }
  
  if (missing(date)) {
    if (latest) date <- max(files$date) else date <- min(files$date)
  }
  date <- timedateFrom(date)
  files <- files[findInterval(date, files$date), ]
 
  if (returnfiles) return(files)
 l <- furrr::future_map_dfr(files$fullname, read_oc_sochla_day, bins = bins)
l
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
  pat <- ".rds$"
  pat2 <- sprintf("acecrc.org.au/ocean_colour/%s_daily", c(MODISA = "modis", SeaWiFS = "seawifs")[product])
  fs <- dplyr::filter(allfiles(), stringr::str_detect(.data$file, pat2)) %>% 
    dplyr::filter(stringr::str_detect(.data$file, pat)) %>% 
    dplyr::transmute(fullname = file.path(.data$root, .data$file), root = .data$root)
  
    fs[["date"]] <- as.POSIXct(strptime(basename(fs$fullname), 
                                   sprintf("%s_%s", c(MODISA = "modis", SeaWiFS = "seawifs")[product], "%Y%j"), tz = "GMT"))
    fs <- fs[order(fs$date), ]
   fs[c("date", "fullname", "root")]
}
  
chla_johnsonfiles <- function(...) {
  .Defunct("oc_sochla_files")
}
