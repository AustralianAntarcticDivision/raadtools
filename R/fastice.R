
##' Data frame of all available fast ice files.
##'
##' A data frame with file, date, fullname
##' @title fast ice files
#' @param product which product
#' @param mask if TRUE return mask file name
##' @param ... reserved for future use, currently ignored
##' @return data frame
##' @export
fasticefiles <- function(product = "binary_fast_ice", mask = FALSE, ...) {
  product <- match.arg(product)
  #pref <- file.path("fastice", "fraser_fastice", product)
  #fs <- list.files(file.path(datadir, pref), pattern = "img$")
  allfiles <- .allfilelist()

  cfiles <- grep("data.aad.gov.au", allfiles, value = TRUE)
  cfiles1 <- grep("3656", allfiles, value = TRUE)
  if (mask) {
    f <- grep("geo",cfiles1, value = TRUE)
    return(grep("coastmask.img$", f, value = TRUE))
  }
  cfiles2 <- grep("sqc.img$", allfiles, value = TRUE)
  
  dates <- as.POSIXct(strptime(basename(cfiles2), "%Y_%j"), tz = "GMT")
  data.frame(date = dates, fullname = cfiles2, stringsAsFactors = FALSE)
}

##' Read fast ice data, optionally with a mask
##'
##' Fast ice data on original Equal Area Cylindrical grid
##' @title Fast ice data
##' @param date date or dates to read (can be character, POSIXt, or Date)
##' @param time.resolution fixed at roughly "3 weekly"
##' @param xylim extent in native space of the grid
##' @param returnfiles return the file details only
##' @param ... reserved for future use, currently ignored
##' @return RasterBrick with 
##' 0: Southern Ocean, pack ice or icebergs, corresponding to light blue in the PNG files.
##' 1: Antarctic continent (including ice shelves), as defined using the Mosaic of Antarctica product, corresponding to white in the PNG files.
##' 2: Fast ice, as classified from a single 20-day MODIS composite image, corresponding to dark blue in the PNG files
##' 3: Fast ice, as classified using a single 20-day AMSR-E composite image, corresponding to yellow in the PNG files
##' 4: Fast ice, as classified using the previous or next 20-day MODIS composite images, corresponding to red in the PNG files
##' @references \url{http://data.aad.gov.au/aadc/metadata/metadata.cfm?entry_id=modis_20day_fast_ice}
##' @export
readfastice <-
  function(date, time.resolution = "weekly3",
           xylim = NULL, returnfiles = FALSE, ...) {
    
    dims <- c(4300, 425)
    
    gridmask <- t(matrix(readBin(fasticefiles(mask = TRUE), "integer", size = 2, n = prod(dims), endian = "little"), dims[1]))
    read0 <- function(x) {
      projstring <- "+proj=cea +lon_0=91 +lat_0=-90 +lat_ts=-65 +datum=WGS84"
      ## bbox in cea
      bb <- structure(c(-4751610.61938822, 3822717.4673464, -13464081.4706772,
                        -14314422.8015431), .Dim = c(2L, 2L))
      topleft <- bb[1,]
      botright <- bb[2,]
      
      
      d <- readBin(x, "integer", size = 1, n = prod(dims), endian = "little")
      d <- t(matrix(d, dims[1]))
      d[gridmask == 1] <- NA
      raster(d, crs = projstring, xmn = topleft[1], xmx = botright[1], ymn = botright[2], ymx = topleft[2])
    }
    
    files <- fasticefiles()
    
    if (missing(date)) date <- min(files$date)
    date <- timedateFrom(date)
    
    ## it would be nice here to trim down if there were input dates
    if (returnfiles) return(files)
    files <- .processFiles(date, files, time.resolution)
    
    cropit <- FALSE
    if (!is.null(xylim)) {
      cropit <- TRUE
      cropext <- extent(xylim)
    }
    
    nfiles <- nrow(files)
    r <- vector("list", nfiles)
    
    for (ifile in seq_len(nfiles)) {
      r0 <- read0(files$fullname[ifile])
      if (cropit) {
        r0 <- crop(r0, cropext)
      }
      r[[ifile]] <- r0
    }
    r <- if (nfiles > 1) brick(stack(r), ...) else r[[1L]]
    names(r) <- sprintf("fastice_%s", format(files$date, "%Y%m%d"))
    
    setZ(r, files$date)
    
  }
