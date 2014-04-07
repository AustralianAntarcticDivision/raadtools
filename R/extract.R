
.read.generic  <- function(x, y, ...) {
              ## read function "x", takes "y" as Date, POSIXct, character
                  x(y, ...)
          }

.standard.assumeXYT.TimeError <- function() {
    stop("invalid times in data, ensure that y is a data.frame with values of longitude, latitude, and date-times")
}

## x is dates from files
.determine.time.resolution <- function(x, ...) {
    rng <- range(difftime(x[-1L], x[-length(x)], units = "days"))
    a <- round(min(rng))
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


## functions for basic linear interpolation between two  time steps
 .interp <- function(x1, x2, proportion) {
     x1 * (1 - proportion) + x2 * proportion
 }

.calcProportion <- function(xmin, xmax, x) {
    (unclass(x) - unclass(xmin) ) / (unclass(xmax) - unclass(xmin))
}



##############################################################
#' Extract methods for raadtools read functions
#'
#' Extract data from read functions in various ways.
#'
#' @param x A raadtools read function.
#' @param y Object to use for querying from the raadtools read
#' functions, such as a vector of character, Date, or POSIXt values,
#' data.frame, trip, etc.
#' @param ctstime specify whether to find the nearest value in time (\code{FALSE}), or interpolate between slices (\code{TRUE})
#' @param fact integer. Aggregation factor expressed as number of cells in each direction (horizontally and vertically). Or two integers (horizontal and vertical aggregation factor). See Details in \code{\link[raster]{aggregate}}
#' @param verbose report on progress or keep quiet
#' @param ... Additional arguments passed to the read function.
#' @return data values extracted by the read functions
#' @seealso \code{\link{readsst}} and \code{\link{extract}}
#' @examples
#' x <- extract(readsst)
#' a <- structure(list(x = c(174, 168, 156, 111, 99, 64, 52, 46, -4,
#' -15, -30, -38, -47, -62, -87, -127, -145, -160, -161), y = c(-72,
#' -39, -50, -58, -35, -38, -48, -60, -48, -35, -37, -51, -68, -72,
#' -69, -54, -40, -49, -54)), .Names = c("x", "y"), row.names = c(NA,
#' -19L), class = "data.frame")
#'
#' #a$time <- structure(c(5479, 5479, 5479, 5479, 5479, 5479, 5479, 5479, 5479,
#' #5479, 5479, 5489, 5529, 5529, 5529, 5579, 5579, 5579, 5579), class = "Date")
#' #extract(readsst, a)
#'
#' #extract(readsst, a, method = "bilinear")
#'
#' #extract(readsst, time.resolution = "daily")
#' a$time <-  sort(as.Date("2005-01-01") + sample(c(0, 0, 0, 8, 20, 50), nrow(a), replace = TRUE))
#' #extract(readsst, a)
#' #extract(readchla, time.resolution = "weekly")
#' #extract(readchla, a, time.resolution = "weekly")
#' ##extract(readwind, a, time.resolution = "weekly")
#' #extract(readwind)
#' ##readwind(dironly = TRUE)
#' @export
#' @docType methods
#' @rdname raadtools-extract
#' @import methods
#' @aliases extract extract,function,Date-method
#' extract,function,POSIXt-method extract,function,character-method
#' extract,function,data.frame-method extract,function,missing-method
#' @exportMethod extract
setMethod("extract", signature(x = 'function', y = 'missing'), function(x, y, ctstime = FALSE, fact = NULL, verbose = TRUE, ...) x(...))
#' @exportMethod extract
setMethod("extract", signature(x = 'function', y = 'POSIXt'), .read.generic)
##' @exportMethod extract
setMethod("extract", signature(x = 'function', y = 'Date'), .read.generic)
##' @exportMethod extract
setMethod("extract", signature(x = 'function', y = 'character'), .read.generic)
##' @exportMethod extract
setMethod("extract", signature(x = 'function', y = 'data.frame'),
          function(x, y, ...) {
              .local <- function (x, y,  ctstime = FALSE, fact = NULL, verbose = TRUE, ...) {
                  result <- rep(as.numeric(NA), nrow(y))


                  resize <- FALSE

                  if (!is.null(fact)) resize <- TRUE
                  notime <- FALSE
                  if (length(x(returnfiles = TRUE)) == 1L) {
                      notime <- TRUE
                  }

                  ## data.frame input has  assumed structure
                  ## we assume y is lon,lat,time
                  y1 <- SpatialPoints(as.matrix(y[,1:2]), CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))


                  if (notime) {
                      ## assume we want topo/bathy values
                      thisx1 <- x()
                      if (resize) thisx1 <- aggregate(thisx1, fact = fact, fun = 'mean')
                      return(extract(thisx1, y1, ...))
                  }
                  times <- try(timedateFrom(y[,3]))
                  y <- y1
                  ## chuck a
                  if (inherits(times, "try-error") | any(is.na(times))) {
                      ##.standard.assumeXYT.TimeError()
                  }
                  ## TODO, will have to figure out how to do this
                  args <- list(...)
                  if ("time.resolution" %in% names(args)) {
                      files <- x(returnfiles = TRUE, time.resolution = args$time.resolution)
                  } else {
                      files <- x(returnfiles = TRUE)
                  }
                  ## TODO, this is awful need a fix
                  time.resolution <- .determine.time.resolution(files$date)
                  ## TODO somehow manage climatology exceptions
                  ## unique indexes
                  findex <- suppressWarnings(.processDates(times, files$date, timeres = time.resolution))
                  ##files <- .processFiles(times, files, timeres = time.resolution)
                  ## all indexes (need to integrate in general processing setup with above)
                  windex <- integer(length(times))
                  for (i in seq_along(times)) {
                     windex[i] <- which.min(abs(times[i] - files$date))
                  }
                  ## this won't always work, need to zap anything out of range . . .
                  if (max(times) == max(files$date[findex])) findex <- c(findex, max(findex) + 1)
                  findex <- findex[findex <= nrow(files)]
                  date <- files$date[findex]

                  mess1 <- ""
                  ## interpolate in time?
                  if (ctstime) {
                      ## we need to store start and end values
                      resm <- cbind(result, result)
                      thisx1 <- x(date[1L], verbose = FALSE)
                      if(resize) thisx1 <- aggregate(thisx1, fact = fact, fun = "mean")
                      for (i in seq_along(date)[-1]) {
                          thisx2 <- x(date[i], verbose = FALSE)
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
    ##                          message(paste(rep("\b", nchar(mess1)), collapse = ""), appendLF = FALSE)
                              cat(paste(rep("\b", nchar(mess1)), collapse = ""))
                              mess1 <- sprintf("%s file %i of %i", time.resolution, i, length(date))
  ##                            message(mess1, appendLF = FALSE)
                              cat(mess1)

                              flush.console()
                          }
                      }
##                      message("", appendLF = TRUE)
                      cat("\n")
                  } else {
                      ## TODO, fix up the if/else here with an exception for the first/last for ctstime
                      for (i in seq_along(date)) {
                          thisx <- x(date[i], verbose = FALSE, ...)
                          if(resize) thisx <- aggregate(thisx, fact = fact, fun = "mean")
                          asub <- windex == findex[i]
                          ## no interpolation in time, controlled by "method" for xy
                          if (any(asub)) {result[asub] <- suppressWarnings(extract(thisx, y[asub, ], ...))}

                            if (interactive() & verbose) {
##                              message(paste(rep("\b", nchar(mess1)), collapse = ""), appendLF = FALSE)
                              cat(paste(rep("\b", nchar(mess1)), collapse = ""))
                              mess1 <- sprintf("%s file %i of %i", time.resolution, i, length(date))
                              ##message(mess1, appendLF = FALSE)
                              cat(mess1)
                              flush.console()
                          }
                      }
                      ##message("", appendLF = TRUE)
                      cat("\n")

                  }
                  result
              }
              .local(x, y, ...)
          }
          )




##' Extract cell values from a given data source by point coordinates and times.
##'
##' This function reads data values from a datasource, one of "oisst",
##' "aviso" and "nsidc". The \code{Query} must be a data.frame with
##' 3-columns of longitude, latitude and date/date-time.
##' @title extractxyt
##' @rdname raadtools-deprecated
##' @param datasource name of the data source to extract from
##' @param Query data.frame of 3-columns, longitude,latitude,date-time
##' @param ... arguments passed to the read functions
##' @seealso Read functions \code{\link{readsst}} ("oisst"),
##' \code{\link{readcurr}} ("aviso"), \code{\link{readice}} ("nsidc").
##' @return numeric vector, one for each row of \code{Query}
##' @export
extractxyt <- function(datasource, Query, ...) {
    .Deprecated("extract")
    ## Query MUST be a 3 column data.frame of long/lat points
    xy <- as.matrix(Query[,1:2])
    date <- timedateFrom(Query[,3])
    if (all(is.na(date))) stop("no datetimes are non-missing")
    Query <- SpatialPointsDataFrame(SpatialPoints(xy, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")), data.frame(time = date), match.ID = FALSE)

    ## readcurr won't work except for magonly
    ## otherwise we need to get the template and check first
    datafun <- switch(datasource,
                      oisst = readsst,
                      nsidc = readice,
                      aviso = readcurr)
    if (is.null(datafun)) stop(sprintf("%s not available", datasource))
    files <- datafun(returnfiles = TRUE)

     ## find indices into files that are requested
    windex <- integer(length(date))
    for (i in seq_along(date)) {
      windex[i] <- which.min(abs(date[i] - files$date))
    }
    dtime <- abs(difftime(date, files$date[windex], units = c("days")))

    ## THIS IS BROKEN, HOW TO DO IT?
    ##dtimetest <- 4
##    if (all(dtime > dtimetest)) stop(sprintf("no data file within %.1f days of %s", dtimetest))
    ##if (any(dtime > dtimetest)) {
    ##  warning(sprintf("%i input dates have no corresponding data file within %f days of available files", sum(dtime > dtimetest), dtimetest))
  ##    windex <- windex[dtime <= dtimetest]
    ##}

      ## work through all the unique indexes

    uindex <- unique(windex)
    extracteddata <- numeric(nrow(Query))



    for (ij in seq_along(uindex)) {
        thisindex <- windex == uindex[ij]
        d0 <- datafun(files$date[uindex[ij]], ...)
         ## get the cellnumbers just once
        if (ij == 1L) {
            extraction <- suppressWarnings(extract(d0, Query, cellnumbers = TRUE))
            cn <- extraction[,1]
            extracteddata[thisindex] <- extraction[thisindex,2]
            ##cat("cn:\n")
            ##print(cn)
        } else {
            extracteddata[thisindex] <- extract(d0, cn[thisindex])
        }
        ## this part is fine
        ##print(getZ(d0))
        ##cat("ij:\n")
        ##print(ij)
        ##cat("thisindex:\n")
        ##print(thisindex)
        ##cat("extracteddata:\n")
        ##print(extracteddata)
    }

    extracteddata
}




## ## multi-function extract method
## exportMethod extract
## setMethod("extract", signature(x = 'list', y = 'data.frame'),
##           function(x, y, ...) {
##            .local <- function (x, y,  ctstime = FALSE, fact = NULL, ...) {

##                vals <- vector("list", length(x))
##                ## the idea is to loop over a list of read functions ...
##             for (i in seq_along(x)) {


##                 vals[[i]] <- extract(x[[i]], y, ctstime = ctstime, fact = fact, ...)
##             }

##                names(vals) <- sapply(x, function(x1) as.character(substitute(x1)))
##             as.data.frame(vals)
##            }
##            .local(x, y, ...)
##        }



##           )

## @exportMethod extract
## setMethod("extract", signature(x = 'function', y = 'SpatialPolygons'),
##           function(x, y, ...) {
##            .local <- function (x, y, datetimelim, ...) {
##                ## we need some kind of time limit
##                if (missing(datetimelim)) {

##                    res <- extract(x(), y, ...)
##                } else {

##                ## how to differentiate the dots arguments for the read function and extract?
##                ## will need to process explicitly

##                }

##                res
##            }
##        }
##           )


## useful scenarios for y
## data.frame of xyt (assume longlat with subtle test)
## SPointsDF with time
## SLinesDF
## SPolyDF
## trip
##


## super simple nn cases
##setMethod("extract", signature(x = "function", y = "SpatialPoints"),
##          function(x, y, ...) {


  ##        }
  ##        )



## all points-based scenarios can be either nn or bilinear
## all line/poly-based scenarios need somehow to provide a time span??

## possible other scenarios for y
##  matrix of xy
##  matrix of xyt
##  dataframe or list of xy
## list of xyt


## ## @exportMethod extract
## setMethod("extract", signature(x = 'function', y = 'Spatial'),
##           function(x, y, ...) {
##               .local <- function (x, y,  ctstime = FALSE, fact = NULL, daterange = NULL, verbose = TRUE, ...) {
##                   result <- rep(as.numeric(NA), nrow(y))


##                   ## if daterange is null just give the end points
##                   files <- x(returnfiles = TRUE)
##                   if (is.null(daterange)) dates <- timedateFrom(daterange)


##                   ## args <- list(...)
##                   ## if ("time.resolution" %in% names(args)) {
##                   ##     files <- x(returnfiles = TRUE, time.resolution = args$time.resolution)
##                   ## } else {
##                   ##     files <- x(returnfiles = TRUE)
##                   ## }
##                   ## TODO, this is awful need a fix
##                   time.resolution <- .determine.time.resolution(files$date)
##                   ## TODO somehow manage climatology exceptions
##                   ## unique indexes
##                   findex <- suppressWarnings(.processDates(times, files$date, timeres = time.resolution))
##                   ## all indexes (need to integrate in general processing setup with above)
##                   windex <- integer(length(times))
##                   for (i in seq_along(times)) {
##                       windex[i] <- which.min(abs(times[i] - files$date))
##                   }
##                   ## this won't always work, need to zap anything out of range . . .
##                   if (max(times) >= max(files$date[findex])) findex <- c(findex, max(findex) + 1)
##                   date <- files$date[findex]
##                   resize <- FALSE
##                   ## TODO careful checks that resizing makes a difference
##                   if (!is.null(fact)) resize <- TRUE
##                   mess1 <- ""
##                   ## interpolate in time?
##                   if (ctstime) {
##                       ## we need to store start and end values
##                       resm <- cbind(result, result)
##                       thisx1 <- x(date[1L], verbose = FALSE)
##                       if(resize) thisx1 <- aggregate(thisx1, fact = fact, fun = "mean")
##                       for (i in seq_along(date)[-1]) {
##                           thisx2 <- x(date[i], verbose = FALSE)
##                           ## TODO check do we have to store the time-value BEFORE aggregating
##                           ##t2 <- getZ(thisx2)
##                           if(resize) thisx2 <- aggregate(thisx2, fact = fact, fun = "mean")
##                           ## findInterval is too hard to use reliably (for this black duck)
##                           ## asub <- findInterval(times, date) == (i - 1)
##                           asub <- windex == findex[i]

##                           ## interpolation in time, controlled in space by "method" for xy
##                           if (any(asub)) {resm[asub, ] <- suppressWarnings(extract(stack(thisx1, thisx2), y[asub, ], ...))}
##                           ## use date since agggregate smashes the Z
##                           result[asub] <- .interp(resm[asub,1], resm[asub,2], .calcProportion(date[i-1L], date[i], times[asub]))
##                           ## setup to do the next loop
##                           thisx1 <- thisx2
##                           ## report happy times
##                           if (interactive() & verbose) {
##                               message(paste(rep("\b", nchar(mess1)), collapse = ""), appendLF = FALSE)
##                               mess1 <- sprintf("%s file %i of %i", time.resolution, i, length(date))
##                               message(mess1, appendLF = FALSE)
##                               flush.console()
##                           }
##                       }
##                       message("", appendLF = TRUE)
##                   } else {
##                       ## TODO, fix up the if/else here with an exception for the first/last for ctstime
##                       for (i in seq_along(date)) {
##                           thisx <- x(date[i], verbose = FALSE, ...)
##                           if(resize) thisx <- aggregate(thisx, fact = fact, fun = "mean")
##                           asub <- windex == findex[i]
##                           ## no interpolation in time, controlled by "method" for xy
##                           if (any(asub)) {result[asub] <- suppressWarnings(extract(thisx, y[asub, ], ...))}

##                             if (interactive() & verbose) {
##                               message(paste(rep("\b", nchar(mess1)), collapse = ""), appendLF = FALSE)
##                               mess1 <- sprintf("%s file %i of %i", time.resolution, i, length(date))
##                               message(mess1, appendLF = FALSE)
##                               flush.console()
##                           }
##                       }
##                       message("", appendLF = TRUE)

##                   }
##                   result
##               }
##               .local(x, y, ...)
##           }
##           )
