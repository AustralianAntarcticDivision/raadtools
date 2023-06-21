

#' Fast ice files
#' 
#' Provided by Alex Fraser
#'
#' This function wraps a raadfiles function `fasticefiles`, but expands the file list out to individual bands. 
#' @param ... ignored
#'
#' @return
#' @export
#' @importFrom raadfiles fasticefiles
#' @examples
#' fraser_fasticefiles()
fraser_fasticefiles <- function(...) {
  files <- raadfiles::fasticefiles("circum_fast_ice")
  on.exit(sink(NULL), add = TRUE)
  sink(tempfile())
  time <- lapply(files$fullname, function(x) raster::getZ(raster::brick(x)))
  files <- files[rep(seq_len(nrow(files)), lengths(time)), ]
  files$date <- as.POSIXct(as.Date("1970-01-01") + unlist(time), tz = "UTC")
  files$band <- unlist(lapply(lengths(time), seq_len))
  
  return(tibble::tibble(fullname = files$fullname,
                        date = files$date, band = files$band))
}
#' Fast ice data
#'
#' High-resolution mapping of circum-Antarctic landfast sea ice distribution, 2000–2018. 
#' 
#' Fast ice data on original polar stereographic grid, the product "circum_fast_ice" 
#' is 1000m resolution published in  [Alex Fraser et al. (2020)](https://doi.org/10.5194/essd-12-2987-2020).  
#' 
#' @section Circumpolar product from 2020: 
#' 
#'  Classified surface type: 
#'  
#' - 0: pack ice or ocean
#' - 1: continent
#' - 2: islands
#' - 3: ice shelf
#' - 4: fast ice
#' - 5: manual fast ice edge
#' - 6: auto fast ice edge
#' 
#' @section Old binary product: 
#' 
#' - 0: Southern Ocean, pack ice or icebergs, corresponding to light blue in the PNG files.
#' - 1: Antarctic continent (including ice shelves), as defined using the Mosaic of Antarctica product, corresponding to white in the PNG files.
#' - 2: Fast ice, as classified from a single 20-day MODIS composite image, corresponding to dark blue in the PNG files
#' - 3: Fast ice, as classified using a single 20-day AMSR-E composite image, corresponding to yellow in the PNG files
#' - 4: Fast ice, as classified using the previous or next 20-day MODIS composite images, corresponding to red in the PNG files
#' 
#' \url{http://data.aad.gov.au/aadc/metadata/metadata.cfm?entry_id=modis_20day_fast_ice}
#' 
#' @title Fast ice data
#' @param date date or dates to read (can be character, POSIXt, or Date)
#' @param product 'circum_fast_ice' or 'binary_fast_ice'
#' @param time.resolution fixed, the underlying time step is 15 days
#' @param xylim extent in native space of the grid
#' @param returnfiles return the file details only
#' @param ... reserved for future use, currently ignored
#' @return RasterBrick see Details 
#' @export
#' @examples 
#' ## read a particular date, it's circumpolar grid with 7 discrete numerc classes
#' fice <- readfastice("2015-10-01")
#' ## hone in on Davis
#' ex <- c(1742836L, 3315135L, 129376L, 1136611L)
#' davis_ice <- crop(fice, extent(c(1742836L, 3315135L, 129376L, 1136611L)))
#' plot(davis_ice >= 4) #, col = c("brown", "white", grey(c(0.2, 0.5, 0.8))), breaks = c(0, 1, 3, 4, 5, 6))
#' 
#' ## compare 5 years change
#' davis_ice2 <- crop(readfastice("2010-10-01"), extent(ex))
#' par(mfrow = c(2, 1))
#' plot(davis_ice >= 4)
#' plot(davis_ice2 >= 4) 
readfastice <- function(date, product = c("circum_fast_ice", "binary_fast_ice"), 
                        xylim = NULL, latest = TRUE, returnfiles = FALSE, ..., 
                        inputfiles = NULL) {
 product <- match.arg(product)
 if (is.null(inputfiles)) {
   files <- fraser_fasticefiles()
 } else {
   files <- NULL
 }
 if (returnfiles) return(files)
 if (missing(date)) {
   if (latest) {
     date <- max(files$date)
   } else {
     date <- min(files$date)
   }
 }
  if (product == "circum_fast_ice") {
    
    out <- readfastice_circum(date, xylim = xylim, latest = latest, returnfiles = returnfiles, inputfiles = files)
  }
  if (product == "binary_fast_ice") {
    out <- 
  readfastice_binary(date, xylim = xylim, latest = latest, returnfiles = returnfiles, inputfiles = files)   
  }
out
  }

readfastice_circum <- function(date, time.resolution = "weekly3", 
                               xylim = NULL, latest = TRUE, returnfiles = FALSE, ..., 
                               inputfiles = NULL) {
  read0 <- function(x, band) {
    #x <- raadfiles::fasticefiles()$fullname[1]
    on.exit(sink(NULL), add = TRUE)
    sink(tempfile())
    r <- flip(raster::raster(x, band = band), "y")
    prj <- "+proj=stere +lat_0=-90 +lat_ts=-70 +lon_0=0 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs"
    ext <- raster::extent(-2691000,  2934000, -2390000,  2310000)
    projection(r) <- prj
    setExtent(r, ext)
  }
  if (!is.null(inputfiles)) {
    files <- inputfiles
  } else {
    files <- raadfiles::fasticefiles("circum_fast_ice")
  }
  if (returnfiles) return(files)
  
  if (missing(date)) {
   date <- min(files$date)
   if (latest) date <- max(files$date)
 }
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
    r0 <- read0(files$fullname[ifile], files$band[ifile])
    if (cropit) {
      r0 <- crop(r0, cropext)
    }
    r[[ifile]] <- r0
  }
  r <- if (nfiles > 1) brick(stack(r), ...) else r[[1L]]
  names(r) <- sprintf("fastice_%s", format(files$date, "%Y%m%d"))
  
  setZ(r, files$date)
  
}
readfastice_binary <-
  function(date, time.resolution = "weekly3", 
           xylim = NULL, latest = TRUE, returnfiles = FALSE, ..., 
           inputfiles = NULL) {
    
    dims <- c(4300, 425)
    
    gridmask <- t(matrix(readBin(fasticefiles(mask = TRUE, product = "binary_fast_ice"), "integer", size = 2, n = prod(dims), endian = "little"), dims[1]))
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
    
    if (!is.null(inputfiles)) {
            files <- inputfiles
    } else {
      files <- fasticefiles(product = "binary_fast_ice")
    }
    if (returnfiles) return(files)
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
