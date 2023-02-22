#' Data frame of file names and dates.
#' @param time.resolution
#' @param ...
#'
#' @return
#' @export
#' @examples
read_par <- function (date, time.resolution = "8daily",
                      xlim = NULL,
                      lon180 = FALSE,
                      nobsonly = FALSE,
                      latest = TRUE,
                      returnfiles = FALSE,
                      inputfiles = NULL,...) {

  if (is.null(inputfiles)) {
    files <- par_files(time.resolution = time.resolution)
  } else {
    files <- inputfiles
  }

  read_i <- function(file, xlim = NULL, lon180 = FALSE, band = 1L) {
    x <- raster(file, varname = "par")
    if (lon180) x <- raadtools:::.rotate(x)
    if (!is.null(xlim)) x <- crop(x, xlim)

    x
  }

  if (returnfiles)
    return(files)
  if (missing(date)) date <- if (latest) max(files$date) else min(files$date)
  date <- timedateFrom(date)
  files <- .processFiles(date, files, time.resolution)

  nfiles <- nrow(files)


  dots <- list(...)


  op <- options(warn = -1)
  on.exit(options(op))
  r0 <- raster::stack(lapply(split(files[c("fullname")], 1:nrow(files)),
                             function(.x) read_i(.x$fullname,xlim = xlim, lon180 = lon180)))
  if (nlayers(r0) == nrow(files)) {
    r0 <- setZ(r0, files$date)
  } else {
    if (nlayers(r0) == 2 & nrow(files) == 1) {
      r0 <- setZ(r0, rep(files$date, 2))
    }
  }

  if ("filename" %in% names(dots)) {

    r0 <- writeRaster(r0, filename = dots[["filename"]])

  }


  r0


}
