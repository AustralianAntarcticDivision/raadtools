##' Load metadata and location of files of Rapid Response imagery. 
##'
##' This function loads the latest cache of stored files for
##' rapid response products.
##' @param ... reserved for future use, currently ignored
##' @export
##' @details We acknowledge the use of Rapid Response imagery from the Land 
##' Atmosphere Near-real time Capability for EOS (LANCE) system operated by 
##' the NASA/GSFC/Earth Science Data and Information System (ESDIS) with funding provided by NASA/HQ. 
##' @examples
##' \dontrun{
##' rf <- rapid_responsefiles()
##' range(rf$date)
##' }
##' @return data.frame of \code{file} and \code{date}
##' @export
rapid_responsefiles <- function(product = c("aqua", "terra"), ...) {

  product <- match.arg(product)
  ## this should be in raadfiles, but see what happens with https://github.com/AustralianAntarcticDivision/bowerbird/issues/1
  files <- raadfiles:::get_raw_raad_filenames()
  files <- files %>% 
    dplyr::filter(stringr::str_detect(.data$file, "lance-modis.eosdis.nasa.gov")) %>% 
    dplyr::filter(stringr::str_detect(.data$file, "Antarctica")) %>% 
    dplyr::filter(stringr::str_detect(.data$file, product)) %>% 
    dplyr::filter(stringr::str_detect(.data$file, "tif$"))
  
 files %>% transmute(date = as.POSIXct(as.Date(stringr::str_extract(.data$file, "[0-9]{7}"), "%Y%j"), tz = "GMT"), 
                     fullname = file.path(.data$root, .data$file))
}

##' Read MODIS Rapid Response RGB images
##'
##' MODIS Rapid Response, Antarctica
##'
##' @title MODIS Rapid Response images
##' @param date date of image to load
##' @param latest if TRUE return the latest time available, ignoring the 'date' argument
##' @param returnfiles return just the list of files
##' @param ... other arguments for \code{\link[raster]{brick}}
##' @export
readrapid_response <- function(date, product = c("aqua", "terra"), latest = FALSE, returnfiles = FALSE, ...) {
  product <- match.arg(product)
  files <- rapid_responsefiles(product = product)
  ## something's wrong with the files
  if (returnfiles) return(files)
  
  if (missing(date)){
    date <-  if (latest)  max(files$date) else min(files$date)
  }
  files <- .processFiles(date, files, "daily")
  
  nfiles <- nrow(files)
  if (nfiles > 1L) {
    warning("only one time-step can be read, for now")
    files <- files[1L,]
  }
  return(brick(files$fullname[1L], ...))
}
