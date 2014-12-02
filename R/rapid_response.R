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
rapid_responsefiles <- function(...) {
  datadir <- getOption("default.datadir")
  ftx <- .allfilelist()
  cfiles1 <- grep("lance-modis.eosdis.nasa.gov", ftx, value = TRUE)
  cfiles2 <- grep("Antarctica", cfiles1, value = TRUE)
  cfiles <- grep("tif$", cfiles2, value = TRUE)
  data.frame(file = gsub(datadir, "", cfiles), date = timedateFrom(strptime(basename(cfiles), "Antarctica.%Y%j")),
             fullname = cfiles, stringsAsFactors = FALSE)
  
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
readrapid_response <- function(date, latest = FALSE, returnfiles = FALSE, ...) {
  files <- rapid_responsefiles()
  if (returnfiles) return(files)
  
  if (missing(date)) date <- min(files$date)
  if (latest) date <- max(files$date)
  files <- .processFiles(date, files, "daily")
  
  nfiles <- nrow(files)
  if (nfiles > 1L) {
    warning("only one time-step can be read, for now")
    ## use pct = TRUE to return a palette image time series")
    
    
    files <- files[1L,]
  }
  return(brick(files$fullname[1L], ...))
  
  
  # this won't work until raster has plot(Brick, legend)
  # ## otherwise we build a single-band palette, somehow
  # r <- vector("list", nfiles)
  # for (i in seq_along(r)) {
  #     x <- brick(files$fullname[i])
  #     if (i == 1L) {
  #         ## this is not actually very efficient
  #         vals <- values(x) / 256
  #         col <- rgb(vals[,1], vals[,2], vals[,3])
  #         f <- factor(col)
  #     }
  #     r[[i]] <- setValues(raster(x), as.integer(unclass(f)))
  
  # }
  
  # r <- brick(stack(r, quick = TRUE), ...)
  # r@legend@colortable <- levels(f)
  # x
}
