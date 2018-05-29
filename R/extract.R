setOldClass("trip")
.read.generic  <- function(x, y, ...) {
  ## read function "x", takes "y" as Date, POSIXct, character
  x(y, ...)
}

.standard.assumeXYT.TimeError <- function() {
  stop("invalid times in data, ensure that y is a data.frame with values of longitude, latitude, and date-times")
}


.determine.time.resolution <- function(x, ...) {
  rng <- range(difftime(x[-1L], x[-length(x)], units = "days"))
  a <- round(min(rng))
  if (a == 0) {
    a <- round(24 * as.numeric(min(rng)))
    return(sprintf("%shourly", a))
  }
  if (a == 1) {
    return("daily")
  }
  if (a %in% 5:9) {
    val = "weekly"
  } else {
    val = "monthly"
  }
  val
  
}



.interp <- function(x1, x2, proportion) {
  x1 * (1 - proportion) + x2 * proportion
}

.calcProportion <- function(xmin, xmax, x) {
  (unclass(x) - unclass(xmin) ) / (unclass(xmax) - unclass(xmin))
}


.big.extract <-  function (x, y,  ctstime = FALSE, fact = NULL, verbose = TRUE, ...) {
    result <- rep(as.numeric(NA), nrow(y))
    ## progress
    pb <- progress::progress_bar$new(
      format = "getting ready                  [:bar] :percent in :elapsed",
      total = 10, clear = FALSE, width= 80)
    pb$tick(0) ## ---------------------------------------------
    
    resize <- FALSE
    
    if (!is.null(fact)) resize <- TRUE
    notime <- FALSE
    pb$tick(0) ## ---------------------------------------------
    ## TODO, will have to figure out how to do this
    args <- list(...)
    if ("xylim" %in% names(args)) {
      warning("xylim argument ignored (determined automatically from the input data)")
      args$xylim <- NULL
    }
    pb$tick(0) ## ---------------------------------------------
    if ("time.resolution" %in% names(args)) {
      files <- x(returnfiles = TRUE, time.resolution = args$time.resolution, ...)
    } else {
      files <- x(returnfiles = TRUE, ...)
    }
    if (length(files) == 1L) {
      notime <- TRUE
    }
    pb$tick(0) ## ---------------------------------------------
    ## data.frame input has  assumed structure
    ## we assume y is lon,lat,time
    y1 <- SpatialPoints(as.matrix(y[,1:2]), CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
    pb$tick(0) ## ---------------------------------------------
    if (notime) {
      ## assume we want topo/bathy values
      thisx1 <- x(xylim = xylim, ...)
      if (resize) thisx1 <- aggregate(thisx1, fact = fact, fun = 'mean')
      return(extract(thisx1, y1, ...))
    }
    pb$tick(0) ## ---------------------------------------------
    times <- try(timedateFrom(y[[3L]]))
    y <- y1
    ## chuck a
    if (inherits(times, "try-error") | any(is.na(times))) {
      ##.standard.assumeXYT.TimeError()
    }
  
    
    
    
    pb$tick(0) ## ---------------------------------------------

    dummy <- x(inputfiles = files, ...)
    yp <- spTransform(y1, projection(dummy))
    pb$tick(0) ## ---------------------------------------------
    xylim <- extent(yp)
  ## expand out a bit for single-location queries
  if (xmax(xylim) == xmin(xylim) | ymax(xylim) == ymin(xylim)) {
  xylim <- xylim + res(dummy)
  }
    dx <- xmax(xylim)-xmin(xylim)
    dy <- ymax(xylim)-ymin(xylim)
    xylim <- xylim + c(dx, dy) / 10
    pb$tick(0) ## ---------------------------------------------
    
    ## TODO, this is awful need a fix
    time.resolution <- .determine.time.resolution(files$date)
    ## TODO somehow manage climatology exceptions
    ## unique indexes
    pb$tick(0) ## ---------------------------------------------
    findex <- suppressWarnings(.processDates(times, files$date, timeres = time.resolution))
    windex <- .indexDates(times, files$date)
    pb$tick(0) ## ---------------------------------------------
    ## this won't always work, need to zap anything out of range . . .
    if (max(times) == max(files$date[findex])) findex <- c(findex, max(findex) + 1)
    findex <- findex[findex <= nrow(files)]
    date <- files$date[findex]
    l <- list(...)
    if ("inputfiles" %in% names(l)) warning("using inputfiles explicitly is deprecated, please don't do it")
    mess1 <- ""
    pb$tick(0) ## ---------------------------------------------
    ## progress
    
    pb <- progress::progress_bar$new(
      format = "extracting :what file :ith of :nn [:bar] :percent in :elapsed",
      total = length(date), clear = FALSE, width= 80)
    pb$tick(0, tokens = list(what = time.resolution, ith = 1, nn = length(date)))
    
    
    ## interpolate in time?
    if (ctstime) {
      ## we need to store start and end values
      resm <- cbind(result, result)
      thisx1 <- x(date[1L], verbose = FALSE, inputfiles = files, xylim = xylim, ...)  ## inputfiles direct
      #print("first read")
      #print(thisx1)
      if(resize) thisx1 <- aggregate(thisx1, fact = fact, fun = "mean")
      for (i in seq_along(date)[-1]) {
        
        thisx2 <- x(date[i], verbose = FALSE, inputfiles = files, xylim = xylim,  ...)
        #print("reading later files")
        #print(thisx2)
        ## TODO check do we have to store the time-value BEFORE aggregating
        ##t2 <- getZ(thisx2)
        if(resize) thisx2 <- aggregate(thisx2, fact = fact, fun = "mean")
        ## findInterval is too hard to use reliably
        ## asub <- findInterval(times, date) == (i - 1)
        asub <- windex == findex[i]
        
        ## interpolation in time, controlled in space by "method" for xy
        if (any(asub)) {resm[asub, ] <- suppressWarnings(extract(stack(thisx1, thisx2), y[asub, ], ...))}
        ## use date since agggregate smashes the Z
        result[asub] <- .interp(resm[asub,1], resm[asub,2], .calcProportion(date[i-1L], date[i], times[asub]))
        ## setup to do the next loop
        thisx1 <- thisx2
        ## report happy times
        if (interactive() & verbose) {
          pb$tick(tokens = list(what = time.resolution, ith = i, nn = length(date)))
          

        }
      }
      ##                      message("", appendLF = TRUE)
     # if (interactive() & verbose) cat("\n")
    } else {

      ## TODO, fix up the if/else here with an exception for the first/last for ctstime
      for (i in seq_along(date)) {
        thisx <- x(date[i], verbose = FALSE, inputfiles = files, xylim = xylim,  ...)
       
        if(resize) thisx <- aggregate(thisx, fact = fact, fun = "mean")
        asub <- windex == findex[i]
        ## no interpolation in time, controlled by "method" for xy
        if (any(asub)) {result[asub] <- suppressWarnings(extract(thisx, y[asub, ], ...))}
        
        if (interactive() & verbose) {
          pb$tick(tokens = list(what = time.resolution, ith = i, nn = length(date)))
          
        }
      }
      ##message("", appendLF = TRUE)
      ##if (interactive() & verbose)   cat("\n")
      
    }
    result

}



##' Extract methods for raadtools read functions
##'
##' Extract data from read functions in various ways.
##' @title extract
##' @param x A raadtools read function.
##' @param y Object to use for querying from the raadtools read functions, such as a vector of character, Date, or POSIXt values,  data.frame, trip, etc.
##' @param ctstime specify whether to find the nearest value in time (\code{FALSE}), or interpolate between slices (\code{TRUE})
##' @param fact integer. Aggregation factor expressed as number of cells in each direction (horizontally and vertically). Or two integers (horizontal and vertical aggregation factor). See Details in \code{\link[raster]{aggregate}}
##' @param verbose report on progress or keep quiet
##' @param ... Additional arguments passed to the read function.
##' @return data values extracted by the read functions
##' @seealso \code{\link{readsst}} and \code{\link{extract}}
##' @examples
##' 
##' a <- structure(list(x = c(174, 168, 156, 111, 99, 64, 52, 46, -4,
##' -15, -30, -38, -47, -62, -87, -127, -145, -160, -161), y = c(-72,
##' -39, -50, -58, -35, -38, -48, -60, -48, -35, -37, -51, -68, -72,
##' -69, -54, -40, -49, -54)), .Names = c("x", "y"), row.names = c(NA,
##' -19L), class = "data.frame")
##'
##' a$time <- structure(c(5479, 5479, 5479, 5479, 5479, 5479, 5479, 5479, 5479,
##' 5479, 5479, 5489, 5529, 5529, 5529, 5579, 5579, 5579, 5579), class = "Date")
##' extract(readsst, a)
##' extract(readsst, a, method = "bilinear")
##' a$time <-  sort(as.Date("2005-01-01") + sample(c(0, 0, 0, 8, 20, 50), nrow(a), replace = TRUE))
##' extract(readsst, a)
##' @name extract
##' @export
##' @aliases extract,function,data.frame-method
setMethod("extract", signature(x = 'function', y = 'data.frame'), .big.extract)

longlat_coords <- function(x) {
  x <- as(x, "SpatialPoints")
  if (!raster::couldBeLonLat(x)) {
    x <- sp::spTransform(x, sp::CRS("+init=epsg:4326"))
  }
  as.data.frame(coordinates(x))
}
.trip.extract <- function(x, y, ...) {
  xyt <- longlat_coords(y)
  xyt[["time"]] <- y[[y@TOR.columns[1L]]]
  extract(x, xyt, ...)
}
setMethod("extract", signature(x = 'function', y = 'trip'), .trip.extract)

