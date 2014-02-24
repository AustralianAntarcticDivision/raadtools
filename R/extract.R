
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
    if (a %in% c(7, 8)) {
        val = "weekly"
} else {
    val = "monthly"
}
    val
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
#' ##@param method "simple" or "bilinear"
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
#' @aliases extract,function,Date-method
#' extract,function,POSIXt-method extract,function,character-method
#' extract,function,data.frame-method extract,function,missing-method
#' @exportMethod extract
setMethod("extract", signature(x = 'function', y = 'missing'), function(x, y, ...) x(...))
#' @exportMethod extract
setMethod("extract", signature(x = 'function', y = 'POSIXt'), .read.generic)
##' @exportMethod extract
setMethod("extract", signature(x = 'function', y = 'Date'), .read.generic)
##' @exportMethod extract
setMethod("extract", signature(x = 'function', y = 'character'), .read.generic)
##' @exportMethod extract
setMethod("extract", signature(x = 'function', y = 'data.frame'),
          function(x, y, ...) {
           .local <- function (x, y,  contintime = FALSE, fact = NULL, ...)
          ##buffer = NULL, small = FALSE, cellnumbers = FALSE, fun = NULL, na.rm = TRUE,  layer, nl, df = FALSE, factors = FALSE, ...)
    {
        .interp <- function(x1, x2, proportion) {
            x1 * (1 - proportion) + x2 * proportion
        }
        .calcProportion <-
            function(xmin, xmax, x) {
                (unclass(x) - unclass(xmin) ) / (unclass(xmax) - unclass(xmin))
            }


        ## dataframes have no metadata so let's do our best
              res <- rep(as.numeric(NA), nrow(y))
              times <- try(timedateFrom(y[,3]))
              ## we assume y is lon,lat,time
              y <- SpatialPoints(as.matrix(y[,1:2]), CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))

              if (inherits(times, "try-error") | any(is.na(times))) {
                  .standard.assumeXYT.TimeError()
              }


              ## hmm, will have to figure out how to do this
              ## process args
              args <- list(...)
              if ("time.resolution" %in% names(args)) {
                  files <- x(returnfiles = TRUE, time.resolution = args$time.resolution)
              } else {
                  files <- x(returnfiles = TRUE)

              }

              time.resolution <- .determine.time.resolution(files$date)

              ## manage climatology exceptions
              ## . . .dunno yet
##              findex <- suppressWarnings(raadtools:::.processDates(times, files$date, timeres = time.resolution))

             findex <- suppressWarnings(.processDates(times, files$date, timeres = time.resolution))
              ## this won't always work, need to zap anything out of range . . .
        if (max(times) >= max(files$date[findex])) findex <- c(findex, max(findex) + 1)
              date <- files$date[findex]
resize <- FALSE
        if (!is.null(fact)) resize <- TRUE
        mess1 <- ""
              if (contintime) {
                  ## we need to store start and end values
                  resm <- cbind(res, res)
                  thisx1 <- x(date[1L], verbose = FALSE)

                  if(resize) thisx1 <- aggregate(thisx1, fact = fact, fun = "mean")
##browser()
                  ## bug in here ....
                  for (i in seq_along(date)[-1]) {
                      thisx2 <- x(date[i], verbose = FALSE)
                      ## we have to store the time-value BEFORE aggregating
                      ##t2 <- getZ(thisx2)
                      if(resize) thisx2 <- aggregate(thisx2, fact = fact, fun = "mean")
                      asub <- findInterval(times, date) == (i - 1)
                      ## interpolation in time, controlled by "method" for xy
##                      if (any(asub)) {resm[asub, ] <- suppressWarnings(extract(stack(thisx1, thisx2), y[asub, ]))}
                      if (any(asub)) {resm[asub, ] <- suppressWarnings(extract(stack(thisx1, thisx2), y[asub, ], ...))}
                      ##if (any(asub)) {resm[asub, ] <- suppressWarnings(extract(stack(thisx1, thisx2), y[asub, ]), ...)}
                      ##res[asub] <- .interp(resm[asub,1], resm[asub,2], .calcProportion(getZ(thisx1), getZ(thisx2), times[asub]))
                      ## use date since agggregate smashes the Z
                      res[asub] <- .interp(resm[asub,1], resm[asub,2], .calcProportion(date[i-1L], date[i], times[asub]))
                      thisx1 <- thisx2
##browser()
                      cat(paste(rep("\b", nchar(mess1)), collapse = ""))

                      mess1 <- sprintf("%s file %i of %i", time.resolution, i, length(date))

                      cat(mess1)
                      flush.console()

                  }
                  cat("\n")


              } else {
                  for (i in seq_along(date)) {
                      thisx <- x(date[i], verbose = FALSE, ...)
                      if(resize) thisx <- aggregate(thisx, fact = fact, fun = "mean")
                      asub <- findInterval(times, date) == i
                      ## no interpolation in time, controlled by "method" for xy
                      if (any(asub)) {res[asub] <- suppressWarnings(extract(thisx, y[asub, ], ...))}
                          cat(paste(rep("\b", nchar(mess1)), collapse = ""))

                      mess1 <- sprintf("%s file %i of %i", time.resolution, i, length(date))

                      cat(mess1)
                      flush.console()
                      print(thisx)
                      cat("asub:\n")
                      print(asub)
                      cat("res:\n")
                      print(res)

                  }
                  cat("\n")
          }
              res
         }
          .local(x, y, ...)
       }
 )

## multi-function extract method
##' @exportMethod extract
setMethod("extract", signature(x = 'list', y = 'data.frame'),
          function(x, y, ...) {
           .local <- function (x, y,  contintime = FALSE, fact = NULL, ...) {

               vals <- vector("list", length(x))
               ## the idea is to loop over a list of read functions ...
            for (i in seq_along(x)) {


                vals[[i]] <- extract(x[[i]], y, contintime = contintime, fact = fact, ...)
            }

               names(vals) <- sapply(x, function(x1) as.character(substitute(x1)))
            as.data.frame(vals)
           }
           .local(x, y, ...)
       }


          )

## ##' @exportMethod extract
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
