


##' Read data from the Sokolov/Rintoul Southern Ocean fronts analysis.
##'
##'
##' Sokolov Rintoul
##' @param date date or dates of data to read, see Details
##' @param time.resolution time resoution data to read, daily or monthly
##' @param product choice of product, see Details
##' @param xylim spatial extents to crop from source data, can be anything accepted by \code{\link[terra]{ext}}, see Details
##' @param lon180 if TRUE, data originally in Pacific view will be returned in Atlantic view (-180:180)
##' @param returnfiles ignore options and just return the file names and dates
##' @param RAT if \code{TRUE} data is returned as a categorical raster with the region names as its levels, see \code{\link[terra]{levels}}
##' @param setNA is \code{TRUE} NaN values are replaced with NA
##' @param trim if \code{TRUE} the map is cropped to no contiguous margins of missing data
##' @param ... reserved for future use, currently ignored
##' @export
##' @return \code{SpatRaster}
##' @examples
##' \dontrun{
##' b <- readfronts(c("1993-01-01", "2005-01-02"), lon180 = FALSE)
##' }
readfronts <- function(date,
                       time.resolution = c("weekly"),
                       product = c("sokolov"),
                       xylim = NULL,
                       lon180 = TRUE,
                       setNA = FALSE,
                       trim = TRUE,
                       returnfiles = FALSE, RAT = TRUE, ...) {
  time.resolution <- match.arg(time.resolution)
  ftx <- .allfilelist()
 cfiles <- grep("fronts", ftx, value = TRUE)
  cfiles1 <- grep("ACCfronts.nc", cfiles, value = TRUE)
  if (length(cfiles1) < 1) stop("file ACCfronts.nc not found!")
  cfiles1 <- cfiles1[1]
  product <- match.arg(product)
  wks <- seq(timedateFrom("1992-10-14"), by = "7 days", length = 854)
  ## get file names and dates and full path
  files <- tibble::tibble(fullname = cfiles1,
                      date = wks, band = seq_along(wks))
    if (missing(date)) date <- min(files$date)
  
  date <- timedateFrom(date)
 
  ##frontname <- c("sBdy", "SACCF_S", "SACCF_N", "PF_S", "PF_M", "PF_N", "SAF_S",
  ##          "SAF_M", "SAF_N", "SAZ_S", "SAZ_M", "SAZ_N")
  
  if (returnfiles) return(files)
  
  
  ##findex <- .processDates(date, files$date, time.resolution)
  ##date <- files$date[findex]
  files <- .processFiles(date, files, time.resolution)
  nfiles <- nrow(files)
  
  proj <- "+proj=merc +ellps=WGS84"
  if (!lon180) proj <- paste(proj, "+over")
  ## the source grid carries no usable georeferencing, so the extent is set
  ## explicitly - full-globe Mercator in the 0-360 view
  ##extreme.points <- as.matrix(expand.grid(c(-180, 180), c(-82, -30.24627)))
  ##epoints.merc <- project(extreme.points, proj)
  full_ext <- terra::ext(0, 2 * 20037508, -16925422, -3513725)
  ## default crop, dropping the empty northern strip
  default_ext <- terra::ext(0, 2 * 20037508, -11087823.8567493, -3513725)

  rat <- data.frame(ID = 0:12, name = c("south of sBdy", "between SACCF-S & sBdy", "SACCF-N & SACCF-S",
                                        "PF-S & SACCF-N", "PF-M & PF-S", "PF-N & PF-M", "SAF-S & PF-N",
                                        "SAF-M & SAF-S", "SAF-N & SAF-M", "SAZ-S & SAF-N", "SAZ-M & SAZ-S",
                                        "SAZ-N & SAZ-M", "north of SAZ-N"), stringsAsFactors = FALSE)

  l <- vector("list", nfiles)
  for (i in seq_along(l)) {
    r0 <- .rast_nc(files$fullname[i])[[files$band[i]]]
    terra::ext(r0) <- full_ext
    terra::crs(r0) <- proj
    r0 <- crop_if_needed(r0, if (is.null(xylim)) default_ext else xylim)

    if (lon180) r0 <- .rotate_projected(r0)

    if (setNA) r0[is.nan(r0)] <- NA
    ## lots of cells are wasted with nodata and with float32 becoming 64.
    ## NaN goes to NA here, exactly as mode(v) <- "integer" used to do.
    ## The on-disk type is a writeRaster(datatype = "INT4S") argument now.
    terra::values(r0) <- as.integer(terra::values(r0))
    if (RAT) levels(r0) <- rat
    l[[i]] <- r0
  }

  r <- terra::rast(l)
  if (trim) r <- terra::trim(r)

  names(r) <- format(files$date, "%Y-%m-%d")
  .utctime(r) <- files$date
  r
}
