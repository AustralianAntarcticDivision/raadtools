
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
    if (a == 1) "daily"
    if (a %in% c(7, 8)) "weekly" else     "monthly"
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
#' @param method "simple" or "bilinear"
#' @param ... Additional arguments passed to the read function.
#'
#' @return data values extracted by the read functions
#'
#' @seealso \code{\link{readsst}} and \code{\link{extract}}
#' @examples
#' x <- extract(readsst)
#' a <- structure(list(x = c(174, 168, 156, 111, 99, 64, 52, 46, -4,
#' -15, -30, -38, -47, -62, -87, -127, -145, -160, -161), y = c(-72,
#' -39, -50, -58, -35, -38, -48, -60, -48, -35, -37, -51, -68, -72,
#' -69, -54, -40, -49, -54)), .Names = c("x", "y"), row.names = c(NA,
#' -19L), class = "data.frame")
#'
#' a$time <- structure(c(5479, 5479, 5479, 5479, 5479, 5479, 5479, 5479, 5479,
#' 5479, 5479, 5489, 5529, 5529, 5529, 5579, 5579, 5579, 5579), class = "Date")
#' extract(readsst, a)
#'
#' extract(readsst, a, method = "bilinear")
#'
#' extract(readsst, time.resolution = "daily")
#' a$time <-  sort(as.Date("2005-01-01") + sample(c(0, 0, 0, 8, 20, 50), nrow(a), replace = TRUE))
#' extract(readsst, a)
#' extract(readchla, time.resolution = "weekly")
#' extract(readchla, a, time.resolution = "weekly")
#' ##extract(readwind, a, time.resolution = "weekly")
#' extract(readwind)
#' ##readwind(dironly = TRUE)
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
           .local <- function (x, y,  ...)
          ##buffer = NULL, small = FALSE, cellnumbers = FALSE, fun = NULL, na.rm = TRUE,  layer, nl, df = FALSE, factors = FALSE, ...)
    {
              ## dataframes have no metadata so let's do our best
              res <- rep(as.numeric(NA), nrow(y))
              times <- try(timedateFrom(y[,3]))
              ## we assume y is lon,lat,time
              y <- SpatialPoints(as.matrix(y[,1:2]), CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))

              if (inherits(times, "try-error") | any(is.na(times))) {
                  .standard.assumeXYT.TimeError()
              }
              ## let's ignore time.resolution
              files <- x(returnfiles = TRUE)
              time.resolution <- .determine.time.resolution(files$date)

              findex <- suppressWarnings(.processDates(times, files$date, timeres = time.resolution))
              date <- files$date[findex]


              for (i in seq_along(date)) {
                  thisx <- x(date[i], verbose = FALSE, ...)
                  asub <- findInterval(times, date) == i
                  if (any(asub)) {res[asub] <- suppressWarnings(extract(thisx, y[asub, ], ...))}

              }
              res
          }
          .local(x, y, ...)
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
