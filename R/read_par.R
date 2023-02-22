#' Read PAR from NASA ocean colour. 
#' 
#' Currently using MODISA 4km Mapped L2m, see [raadfiles::read_par()] for the actual source files. 
#' 
#' @param time.resolution '8D' currently, use the NASA tokens for time period 
#' @inheritParams readsst
#'
#' @return raster object 
#' @export
#' @importFrom raadfiles par_files
#' @examples
#' read_par(latest = FALSE)
read_par <- function (date, time.resolution = "8D",
                      xylim = NULL,
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

  read_i <- function(file, xylim = NULL, lon180 = FALSE, band = 1L) {
    x <- raster(file, varname = "par")
    x <- setExtent(x, extent(-180, 180, -90, 90))
    if (lon180) x <- raadtools:::.rotate(x)
    if (!is.null(xylim)) x <- crop(x, xylim)

    x
  }

  if (returnfiles)
    return(files)
  if (missing(date)) date <- if (latest) max(files$date) else min(files$date)
  date <- timedateFrom(date)
 # browser()
  files <- .processFiles(date, files, time.resolution)

  nfiles <- nrow(files)


  dots <- list(...)


  op <- options(warn = -1)
  on.exit(options(op))
  r0 <- raster::stack(lapply(split(files[c("fullname")], 1:nrow(files)),
                             function(.x) read_i(.x$fullname,xylim = xylim, lon180 = lon180)))
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
