
.read.generic  <- function(x, y, ...) {
              ## read function "x", takes "y" as Date, POSIXct, character
                  x(y, ...)
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
              r <- x(y[,3])
              for (i in seq_len(nrow(y))) res[i] <- extract(subset(r, i), y[i,1:2])
              res
          }
          )

## possible scenarios
