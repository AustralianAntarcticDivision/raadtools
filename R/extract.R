
## method takes a function that reads multiple time slices

##' @import methods
setMethod("extract", signature(x = 'function', y = 'POSIXt'),
          function(x, y, ...) {
                  x(y)
          }
      )

##' @import methods
setMethod("extract", signature(x = 'function', y = 'data.frame'),
          function(x, y, ...) {
              ## y better be x,y,t and in the right projection
              res <- rep(as.numeric(NA), nrow(y))
              r <- x(y[,3])
              for (i in seq_len(nrow(y))) res[i] <- extract(subset(r, i), y[i,1:2])
              res
          }
          )
