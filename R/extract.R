#' Extract values at points in space and time
#'
#' Extract values from a raadtools reader at points that each carry their own
#' date-time. `x` is a read function such as [readsst()], `y` a data.frame of
#' longitude, latitude and date-time in that column order.
#'
#' Only the time slices the points actually need are read, and each is read
#' once. The work is done by [xyt::extract_xyt()]; this is the raadtools front
#' door onto it, so that `extract(readsst, xyt)` goes on meaning what it has
#' always meant.
#'
#' `xyt` takes further arguments that are passed through here: `method` for the
#' spatial interpolation, `fact` to aggregate each slice before reading,
#' `tolerance` to return `NA` for points that fall too far from any slice in
#' time, and `map` to spread the reads over `mirai` daemons. See
#' [xyt::extract_xyt()].
#'
#' @param x a raadtools read function
#' @param y a data.frame of longitude, latitude, date-time
#' @param ctstime interpolate between the two slices bracketing each point
#'   (`TRUE`), or take the value from a single slice (`FALSE`, the default)
#' @param when which slice a point takes its value from when `ctstime` is
#'   `FALSE`: `"previous"` is what raadtools has always done, `"nearest"` is
#'   what it always documented. See the note below.
#' @param ... passed to [xyt::extract_xyt()] and on to the read function
#' @return a numeric vector, one value per row of `y`
#' @note `when = "previous"` is the default here rather than xyt's own
#'   `"nearest"`, because that is what the old `findInterval()` implementation
#'   did and it is what existing results were computed with. The documentation
#'   always said "nearest in time", so the two have disagreed for years.
#'   Changing the default should be a deliberate decision, not a side effect of
#'   this move.
#' @seealso [xyt::extract_xyt()], [readsst()]
#' @examples
#'
#' a <- structure(list(x = c(174, 168, 156, 111, 99, 64, 52, 46, -4,
#' -15, -30, -38, -47, -62, -87, -127, -145, -160, -161), y = c(-72,
#' -39, -50, -58, -35, -38, -48, -60, -48, -35, -37, -51, -68, -72,
#' -69, -54, -40, -49, -54)), .Names = c("x", "y"), row.names = c(NA,
#' -19L), class = "data.frame")
#'
#' a$time <- structure(c(5479, 5479, 5479, 5479, 5479, 5479, 5479, 5479, 5479,
#' 5479, 5479, 5489, 5529, 5529, 5529, 5579, 5579, 5579, 5579), class = "Date")
#' extract(readsst, a)
#' extract(readsst, a, method = "bilinear")
#' a$time <-  sort(as.Date("2005-01-01") + sample(c(0, 0, 0, 8, 20, 50), nrow(a), replace = TRUE))
#' extract(readsst, a)
#' @name extract
#' @export
#' @aliases extract,function,data.frame-method
setMethod("extract", signature(x = "function", y = "data.frame"),
          function(x, y, ctstime = FALSE, when = "previous", ...) {
            xyt::extract_xyt(x, y, ctstime = ctstime, when = when, ...)
          })
