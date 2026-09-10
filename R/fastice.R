#' Fast ice files
#'
#' Provided by Alex Fraser
#'
#' This function wraps a raadfiles function `fasticefiles`, but expands the file list out to individual bands.
#' @param ... ignored
#'
#' @return data.frame of \code{fullname}, \code{date} and \code{band}
#' @export
#' @importFrom raadfiles fraser_fasticefiles
#' @examples
#' fraser_fasticefiles()
fasticefiles <- function(...) {
  files <- raadfiles::fraser_fasticefiles("circum_fast_ice")
  ## one yearly file holds a run of 15-day composites, so expand to one row
  ## per composite. The variable has to be named: these files also carry 2D
  ## latitude, longitude and area variables, and GDAL will hand one of those
  ## back instead. The extent warning is expected - there is no georeferencing
  ## in the file, readfastice_circum() sets it.
  invisible(capture.output(time <- lapply(files$fullname,
                 function(x) suppressWarnings(terra::time(.rast_nc(x, subds = .fastice_var))))))
  files <- files[rep(seq_len(nrow(files)), lengths(time)), ]
  files$date <- as.POSIXct(as.Date("1970-01-01") + unlist(time), tz = "UTC")
  files$band <- unlist(lapply(lengths(time), seq_len))
  
  return(tibble::tibble(fullname = files$fullname,
                        date = files$date, band = files$band))
}


## the classified surface type; the other variables in these files are
## latitude, longitude and area
.fastice_var <- "Fast_Ice_Time_series"


#' Fast ice data
#'
#' High-resolution mapping of circum-Antarctic landfast sea ice distribution, 2000-2018.
#'
#' Fast ice data on original polar stereographic grid, the product "circum_fast_ice"
#' is 1000m resolution published in  [Alex Fraser et al. (2020)](https://doi.org/10.5194/essd-12-2987-2020).
#'
#' @section Circumpolar product from 2020:
#'
#'  Classified surface type:
#'
#' - 0: pack ice or ocean
#' - 1: continent
#' - 2: islands
#' - 3: ice shelf
#' - 4: fast ice
#' - 5: manual fast ice edge
#' - 6: auto fast ice edge
#'
#' @title Fast ice data
#' @param date date or dates to read (can be character, POSIXt, or Date)
#' @param product 'circum_fast_ice' or 'binary_fast_ice'
#' @param time.resolution fixed, the underlying time step is 15 days
#' @param xylim extent in native space of the grid
#' @param latest if TRUE (and date not supplied) return the latest time available, otherwise the earliest
#' @param returnfiles return the file details only
#' @param inputfiles input the files data base to speed up initialization
#' @param ... passed on, primarily for \code{filename}
#' @return \code{SpatRaster}, see Details
#' @export
#' @examples
#' ## read a particular date, it's circumpolar grid with 7 discrete numerc classes
#' fice <- readfastice("2015-10-01")
#' ## hone in on Davis
#' ex <- c(1742836L, 3315135L, 129376L, 1136611L)
#' davis_ice <- terra::crop(fice, terra::ext(1742836, 3315135, 129376, 1136611))
#' plot(davis_ice >= 4) #, col = c("brown", "white", grey(c(0.2, 0.5, 0.8))), breaks = c(0, 1, 3, 4, 5, 6))
#'
#' ## compare 5 years change
#' davis_ice2 <- terra::crop(readfastice("2010-10-01"), terra::ext(ex))
#' par(mfrow = c(2, 1))
#' plot(davis_ice >= 4)
#' plot(davis_ice2 >= 4)
readfastice <- function(date, product = c("circum_fast_ice"),
                        xylim = NULL, latest = TRUE, returnfiles = FALSE, ...,
                        inputfiles = NULL) {
  product <- match.arg(product)
  files <- if (is.null(inputfiles)) fasticefiles() else inputfiles
  if (returnfiles) return(files)
  if (missing(date)) {
    if (latest) {
      date <- max(files$date)
    } else {
      date <- min(files$date)
    }
  }
  if (product == "circum_fast_ice") {
    
    out <- readfastice_circum(date, xylim = xylim, latest = latest, returnfiles = returnfiles, inputfiles = files, ...)
  }
  # if (product == "binary_fast_ice") {
  #   out <-
  # readfastice_binary(date, xylim = xylim, latest = latest, returnfiles = returnfiles, inputfiles = files)
  # }
  out
}

readfastice_circum <- function(date, time.resolution = "weekly3",
                               xylim = NULL, latest = TRUE, returnfiles = FALSE, ...,
                               inputfiles = NULL) {
  prj <- "+proj=stere +lat_0=-90 +lat_ts=-70 +lon_0=0 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs"
  ex <- terra::ext(-2691000, 2934000, -2390000, 2310000)
  
  read0 <- function(x, band) {
    ## no flip here. raster read these in file order and flipped afterwards;
    ## GDAL reads netCDF bottom-up by default and has already done it, checked
    ## against a file built the same way (2D lat/lon, no 1D y/x coordinates).
    ## The extent warning is expected, since the extent comes from here.
    
    invisible(capture.output(r <- suppressWarnings(.rast_nc(x, subds = .fastice_var)[[band]])))
    r <- terra::flip(r, "vertical")
    terra::ext(r) <- ex
    terra::crs(r) <- prj
    r
  }
  
  files <- if (!is.null(inputfiles)) inputfiles else fasticefiles()
  if (returnfiles) return(files)
  
  if (missing(date)) {
    date <- min(files$date)
    if (latest) date <- max(files$date)
  }
  date <- timedateFrom(date)
  
  files <- .processFiles(date, files, time.resolution)
  
  nfiles <- nrow(files)
  r <- vector("list", nfiles)
  
  for (ifile in seq_len(nfiles)) {
    r[[ifile]] <- crop_if_needed(read0(files$fullname[ifile], files$band[ifile]), xylim)
  }
  r <- if (nfiles > 1) terra::rast(r) else r[[1L]]
  names(r) <- sprintf("fastice_%s", format(files$date, "%Y%m%d"))
  
  .utctime(r) <- files$date
  .write_if_filename(r, ...)
}