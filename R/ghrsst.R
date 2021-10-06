
# 
# changes in 2015?
# 
# 286
# 20151013-JPL-L4UHfnd-GLOB-v01-fv04-MUR.nc
# 317
# 20151113090000-JPL-L4_GHRSST-SSTfnd-MUR-GLOB-v02.0-fv04.1.nc


#' GHRSST L4 files
#'
#' Data frame of file names and dates. 
#' @return data.frame
#' @export
ghrsstfiles <- function() {
  raadfiles::ghrsst_daily_files()
}

#' Read GHRSST
#'
#' SST in Kelvin
#' @param date datetime
#' @param time.resolution daily
#' @param varname one of "analysed_sst", "analysis_error", "mask", "sea_ice_fraction"
#' @param setNA ignored
#' @param lon180 currently ignored
#' @param ... arguments passed to raster brick
#' @inheritParams raadtools
#' @return RasterStack or RasterLayer
#' @export
readghrsst  <- function (date, time.resolution = c("daily"),
                         xylim = NULL, lon180 = TRUE,
                         varname = c("analysed_sst", "analysis_error", "mask", "sea_ice_fraction"),
                         setNA = TRUE,
                         latest = TRUE,
                         returnfiles = FALSE, ..., inputfiles = NULL) {
  if (!lon180) stop("only lon180 is supported")
  time.resolution <- match.arg(time.resolution)
  varname <- match.arg(varname)

  files <- if (is.null(inputfiles))  ghrsstfiles() else inputfiles
  if (returnfiles)  return(files)
  
  
  if (missing(date)) date <- if (latest) max(files$date) else min(files$date)
  date <- timedateFrom(date)
  files <- .processFiles(date, files, time.resolution)

  
xylim <- force(xylim)
  nfiles <- nrow(files)
#  if (is.null(xylim)) xylim <- extent(-180, 180, -90, 90)
  pb <- progress::progress_bar$new(
    format = "  extracting [:bar] :percent in :elapsed",
    total = nfiles, clear = FALSE, width= 60)
  pb$tick(0)
  read_fun <- function(xfile, ext, varname = "", band = 1) {
    pb$tick()
    crop_if_needed(setExtent(raster(xfile, varname = varname), extent(-180, 180, -90, 90)), ext)
  }
  
  if (inherits(xylim, "BasicRaster")) {
    out <- raster::brick(purrr::map(purrr::transpose(files), 
                                    ~do_it_vapour_ghrsst(.x, xylim, varname))
    )
    return(raster::setZ(out, files$date))  
  }
  
   r0 <- stack(lapply(seq_len(nfiles), function(xi) read_fun(files$fullname[xi], ext = xylim, varname = varname)))
  if (is.na(projection(r0))) projection(r0) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
  r0 <- setZ(r0, files$date)
  
  ## need to determine if "filename" was passed in
  dots <- list(...)
  if ("filename" %in% names(dots)) {
    r0 <- writeRaster(r0, ...)
  }
  
  if (nfiles == 1) {
    r0 <- r0[[1L]]
    r0 <- setZ(r0, files$date)
  }
 
  #wtf
 # r0 <- setZ(r0, getZ(r0) + ISOdatetime(1981, 1, 1, 0, 0, 0, tz = "GMT")) ##1981-01-01 00:00:00)
  ## Kelvin
   r0 
  
}

do_it_vapour_ghrsst <- function(files, grid, varname) {
  gdalio::gdalio_set_default_grid(grid)
  sds <- gdalio::vrt(grep(varname, vapour::vapour_sds_names(files$fullname[1])$subdataset, value = TRUE), 
                     projection = "OGC:CRS84", extent = c(-180, 180, -90, 90))
  
  v <- gdalio::gdalio_data(sds)
  raster::setValues(grid, v[[1]])
}