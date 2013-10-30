
.read.generic  <- function(x, y, ...) {
              ## read function "x", takes "y" as Date, POSIXct, character
                  x(y, ...)
          }

.standard.assumeXYT.TimeError <- function() {
    stop("invalid times in data, ensure that y is a data.frame with values of longitude, latitude, and date-times")
}

## x is dates from files
.determine.time.resolution <- function(x, ...) {
    rng <- range(difftime(x[-length(x)], x[1L], units = "days"))
    round(min(rng))
}
##############################################################
#' Extract methods for raadtools read functions
#'
#' Extract data from read functions in various ways.
#'
#' @param x A raadtools read function.
#'
#' @param y One of various means of querying from the raadtools read
#' functions, such as a vector of character, Date, or POSIXt values,
#' data.frame, trip, etc.
#'
#' @param ... Additional arguments passed to the read function.
#'
#' @return data values extracted by the read functions
#'
#' @seealso \code{\link{readsst}} and \code{\link{extract}}
#'
#' @export
#' @docType methods
#' @rdname raadtools-extract
#' @import methods
#' @aliases extract,function,Date-method
#' extract,function,POSIXt-method extract,function,character-method
#' extract,function,data.frame-method extract,function,missing-method
#' @exportMethod extract
setMethod("extract", signature(x = 'function', y = 'missing'), function(x, ...) x(...))
#' @exportMethod extract
setMethod("extract", signature(x = 'function', y = 'POSIXt'), .read.generic)
##' @exportMethod extract
setMethod("extract", signature(x = 'function', y = 'Date'), .read.generic)
##' @exportMethod extract
setMethod("extract", signature(x = 'function', y = 'character'), .read.generic)
##' @exportMethod extract
setMethod("extract", signature(x = 'function', y = 'data.frame'),
          function(x, y, ...) {
              ## dataframes have no metadata so let's do our best
              res <- rep(as.numeric(NA), nrow(y))
              times <- try(timedateFrom(y[,3]))
              if (inherits(times, "try-error") | any(is.na(times))) {
                  .standard.assumeXYT.Timeerror()
              }
              ## let's ignore time.resolution
              files <- x(returnfiles = TRUE)
              time.resolution <- .determine.time.resolution(files$date)

              findex <- .processDates(times, files$date, timeres = "daily")
              date <- files$date[findex]

              for (i in seq_along(date)) {
                  thisx <- x(date[i])
                  asub <- findInterval(times, date) == i

                  res[asub] <- extract(thisx, subset(y[,1:2], asub))

              }
          )

## useful scenarios for y
## data.frame of xyt (assume longlat with subtle test)
## SPointsDF with time
## SLinesDF
## SPolyDF
## trip
##


## super simple nn cases
setMethod("extract", signature(x = "function", y = "SpatialPoints"),
          function(x, y, ...) {


          }
          )



## all points-based scenarios can be either nn or bilinear
## all line/poly-based scenarios need somehow to provide a time span??

## possible other scenarios for y
##  matrix of xy
##  matrix of xyt
##  dataframe or list of xy
## list of xyt
