# The differences between the two projection pairs (EPSG 3411/3412 and EPSG
# 3413/3976) are minimal - the diagonal distance across a WGS 84/NSIDC Sea Ice
# Polar Stereographic 25 km grid cell differs about 1 m from the original NSIDC
# Sea Ice Polar Stereographic grid cell. However, users should note that EPSG
# codes 3411 and 3412 are deprecated, and NSIDC encourages all new products to
# use EPSG codes 3413 and 3976.
# 


## note, we might use a date-controlled way to change the SRS to for the older ones (3411 and 3412)
.north_nsidc_vrt <- '<VRTDataset rasterXSize="304" rasterYSize="448"> 
  <VRTRasterBand dataType="Byte" band="1" subClass="VRTRawRasterBand"> 
    <SourceFilename relativetoVRT="0">%s</SourceFilename> 
      <ImageOffset>300</ImageOffset> 
      <PixelOffset>1</PixelOffset> 
      <LineOffset>304</LineOffset> 
      </VRTRasterBand> 
      <SRS dataAxisToSRSAxisMapping="1,2">EPSG:3413</SRS>
      <GeoTransform> -3.8375000000000000e+06,  2.5000000000000000e+04,  0.0000000000000000e+00,  5.8375000000000000e+06,  0.0000000000000000e+00, -2.5000000000000000e+04</GeoTransform>
      </VRTDataset>'


.south_ndsic_vrt <- '<VRTDataset rasterXSize="316" rasterYSize="332"> 
  <VRTRasterBand dataType="Byte" band="1" subClass="VRTRawRasterBand"> 
    <SourceFilename relativetoVRT="0">%s</SourceFilename> 
    <ImageOffset>300</ImageOffset> 
    <PixelOffset>1</PixelOffset> 
    <LineOffset>316</LineOffset> 
  </VRTRasterBand> 
  <SRS dataAxisToSRSAxisMapping="1,2">EPSG:3976</SRS>
  <GeoTransform> -3.9500000000000000e+06,  2.5000000000000000e+04,  0.0000000000000000e+00,  4.3500000000000000e+06,  0.0000000000000000e+00, -2.5000000000000000e+04</GeoTransform>
</VRTDataset>'

#' Area of pixels in sea ice
#'
#' Read the NSIDC pixel-area files for either hemisphere. 
#' Only 25km product is supported. Tool name "psn25area_v3.dat and pss25area_v3.dat": 
#' \url{http://nsidc.org/data/polar-stereo/tools_geo_pixel.html#pixel_area}. 
#' @param product "nsidc" the 25km NSIDC passive microwave
#' @param ... ignored
#' @param hemisphere south (default) or north
#'
#' @return raster with the area of the cells in m^2
#' @export
#' @seealso readice
#' @examples
#' readice_area()
#' readice_area(hemisphere = "north")
readice_area <- function(product = "nsidc", hemisphere = "south", ...) {
  f <- dplyr::filter(raadfiles::get_raad_filenames(), stringr::str_detect(file, "polar-stereo")) |> 
    dplyr::transmute(fullname = file.path(root, file))
  template <- readice(product = product, hemisphere = hemisphere)
  ## find dat file
  south <- hemisphere == "south"
  patt <- if(south) "pss25area" else "psn25area"
  
  datfile <- dplyr::filter(f, stringr::str_detect(fullname, patt)) |> dplyr::pull(fullname)
  # http://nsidc.org/data/polar-stereo/tools_geo_pixel.html#pixel_area
  
  # Grids that determine the area of a given pixel for the 25 km grids for
  # either hemisphere (psn for the Northern Hemisphere and pss for the Southern
  # Hemisphere). The arrays are in binary format and are stored as 4-byte
  # integers scaled by 1000 (divide by 1000 to get square km).
  #
  # psn25area_v3.dat: 304 columns x 448 rows pss25area_v3.dat: 316 columns x 332
  # rows
  # 
  
  ## CONFIRM http://rpubs.com/cyclemumner/365281
  ## range((dat/1000) -  values(ice_area))
 ## [1] 0.01916043 0.03261678
  #datfile <- "/rdsi/PUBLIC/raad/data/sidads.colorado.edu/pub/DATASETS/seaice/polar-stereo/tools/pss25area_v3.dat"
  con <- file(datfile, open = "rb")
  on.exit(close(con))
  dat <- readBin(con, "integer", size = 4, n =  file.info(datfile)$size/4)
  setNames(raster::setValues(raster::raster(template), dat * 1000), "NSIDC_true_area_m2")
}

#' Read from NSIDC 25km polar sea ice.
#'
#' Sea ice at 25km for either hemisphere. 
#' 
#' This function relies on the file-listing of [icfiles()]. 
#' 
#' Currently available products are
#'
#' \describe{
#' \item{'nsidc'}{daily or monthly NSIDC concentration data, processed by the SMMR/SSMI NASA Team}
#' }
#'
#' Dates are matched to file names by finding the nearest match in
#' time within a short duration. If \code{date} is greater than
#' length 1 then the sorted set of unique matches is returned.
#' @param date date or dates of data to read, see Details
#' @param time.resolution time resoution data to read, daily or monthly
#' @param product choice of sea ice product, see Details
#' @param hemisphere north or south (or both, if 'both' xylim should be an actual raster or terra grid)
#' @param xylim spatial extents to crop from source data, can be anything accepted by \code{\link[raster]{extent}}
#' @param setNA mask zero and values greater than 100 as NA
#' @param rescale rescale values from integer range?
#' @param latest if TRUE and date input is missing, return the latest time available otherwise the earliest
#' @param returnfiles ignore options and just return the file names and dates
#' @param ... passed to brick, primarily for \code{filename}
#' @param extension default for product "amsr" is "hdf" but can be "tif" , extension = "hdf"
#' @param inputfiles input the files data base to speed up initialization
#' @param resample warper resampling method used when 'xylim' is a full grid
#' @details For NSIDC data a \code{\link[raster]{ratify}}ied raster is returned if \code{setNA} and 
#' \code{rescale} are both set to \code{FALSE}.  Use \code{levels(x)} to return the data.frame of values 
#' and levels (there's no straight-through rule, all numeric values are explicit along with special
#' values like "Unused"). 
#' The values used are documented here \url{http://nsidc.org/data/docs/daac/nsidc0051_gsfc_seaice.gd.html}
#' 
#' If 'both' is specified for hemisphere or if 'xylim' is a full raster grid,
#' the warper is applied to VRT versions of the NSIDC files, which allows them to
#' be combined in one reprojection step. In this case 'xylim' can be specified, to
#' give a projected grid of any form. ' If not supplied (when hemisphere = 'both')
#' then longlat raster at 0.25 degrees is assumed. ('xylim' can be specified as '
#' a target grid and with only north or south hemisphere applied). When the warper
#' is used, 'setNA' and 'resample' behave the same ' way, though exact results will
#' be different depending on the value of 'resample'.
#' @export
#' @examples 
#' library(raadtools)
#'  
#' ice <- readice(latest = TRUE)
#' 
#' ## can read one or other hemisphere in native projection
#' readice(hemisphere = "south")
#' readice(hemisphere = "north")
#' ## or we can read both, and get longlat by default
#' readice(hemisphere = "both")
#' ## or set our own grid and read to that
#' ## spex::buffer_extent(extent(c(-.5, .5, -1, 1)* rad * pi), 25000)
#' tm_ex <- c(-.5, .5, -1, 1) * 20025000 
#' tm_template <- raster(extent(tm_ex), res = 25000, crs = "+proj=tmerc")
#' readice(hemisphere = "both", xylim = tm_template)
#' 
#' ## this means we can run extract on global ice, and get 0 in the middle
#' ## extract(readice, data.frame(176, c(-72, 84), as.Date("2020-04-03") + c(0, 100)))
#' ## [1]  80 NA
#' ## extract(readice, data.frame(176, c(-72, 84), as.Date("2020-04-03") + c(0, 100)), hemisphere = "both")
#'  ## [1]  78.0 94.4  ## it's interpolated from the original data
#' @return \code{\link[raster]{raster}} object 
#' @seealso \code{\link{icefiles}} for details on the repository of
#' data files, \code{\link[raster]{raster}} for the return value
#' @name readice
readice_daily <- function(date,
                    time.resolution = "daily",
                    product = "nsidc",
                    hemisphere = c("south", "north", "both"), 
                    xylim = NULL,
                    setNA = TRUE, rescale = FALSE, 
                    latest = TRUE,
                    returnfiles = FALSE,  ..., inputfiles = NULL, resample = "bilinear") {
  
  if (rescale) {
    yes <- getOption("raadtools.message.rescale")
    if (yes) message("since v2 of NSIDC 25km sea ice, 'rescale' no longer has meaning, ignored (data are returned in range 0,100)")
    options(raadtools.message.rescale = FALSE)
  }
#  time.resolution <- match.arg(time.resolution)
 product <- match.arg(product)
 hemisphere <- match.arg(hemisphere)
 opt <- getOption("raadtools.both.hemisphere.message")
 if (hemisphere == "both" && is.null(xylim)) {
   if (is.null(opt) || !opt) {
     options("raadtools.both.hemisphere.message" = TRUE)
     message("for both hemispheres, 'xylim' may be specified - assuming global longlat at 0.25 degree")
   }
   xylim <- raster::raster()
   raster::res(xylim) <- 0.25
 }
 if (time.resolution != "daily") stop("readice for non-daily data is defunct, see 'readice_monthly()', 'readice_daily()' and similarly specific functions")
  time.resolution <- match.arg(time.resolution)
  if (!is.null(inputfiles)) {
    files <- inputfiles
  } else {
    .get_both_hemisphere_files <- function() {
      north = icefiles(hemisphere = "north")
      south = icefiles(hemisphere = "south")
      #north$fullname <- vapour::vapour_vrt(north$fullname, sds = 1)
      #south$fullname <- vapour::vapour_vrt(north$fullname, sds = 1)
      
      tibble::tibble(date = north$date, 
                     fullname  = split(rbind(north$fullname, south$fullname), rep(seq(1, nrow(north)), each = 2L)))
    }
    ## get file names and dates and full path
    files <- switch(hemisphere, 
                    north = icefiles(hemisphere = "north"), 
                    south = icefiles(hemisphere = "south"), 
                    both = .get_both_hemisphere_files())
  }
  if (returnfiles) return(files)
  if (missing(date)) {
    if (latest) date <- max(files$date)  else date <- min(files$date)
  }
  date <- timedateFrom(date)
  files <- .processFiles(date, files, time.resolution)
  
  
  dimension <- NULL

  if (inherits(xylim, "SpatRaster")) {
   # dimension <- dim(xylim)[2:1]
   # projection <- xylim@ptr$get_crs("wkt")
   # ex <- xylim@ptr$extent@.xData$vector
   xylim <- raster::raster(xylim)
  }
  if (inherits(xylim, "BasicRaster")) { 
    #dimension <- dim(xylim)[2:1]
    #projection <- comment(raster::crs(xylim))
    ex <- c(raster::xmin(xylim), raster::xmax(xylim), raster::ymin(xylim), raster::ymax(xylim))
    
  }
  if (!is.null(dimension)) {
    ## with the warper we currently get value 0,250 as they are natively in the netcdf (gdal per se returns them 0,1)
    list_of_fullname <- lapply(files$fullname, function(.x) vapour::vapour_vrt(.x, sds = 1))
    

    out <- lapply(list_of_fullname, function(.x)  
      vapour::vapour_warp_raster_dbl(.x, extent = ex, dimension = dimension, projection = projection, resample = resample))   
   # browser()
    rs <-  100  ## we've lost the ability to not scale with netcdf
   if (setNA) out <- lapply(out, function(.x) {.x[.x > 250] <- NA; .x[!.x > 0] <- NA;  raster::setValues(xylim[[1]],.x * rs)})

    return(setZ(raster::brick(out), files$date))
  }
  read_ice_v2(files, setNA, xylim, ...)
  ##read_ice_internal(files, hemisphere, rescale, setNA, xylim, ...) 
}
#' @name readice
#' @export
readice <- readice_daily
#' @name readice
#' @export
readice_monthly <- function(date,
                    time.resolution = "monthly",
                    product = "nsidc",
                    hemisphere = c("south", "north"), 
                    xylim = NULL,
                    setNA = TRUE, rescale = TRUE, 
                    latest = TRUE,
                    returnfiles = FALSE,  ..., inputfiles = NULL) {
  
  #  time.resolution <- match.arg(time.resolution)
  product <- match.arg(product)
  hemisphere <- match.arg(hemisphere)
  if (time.resolution != "monthly") warning("time.resolution argument is deprecated, see readice_daily() and similar for specific functions")
  
  time.resolution <- match.arg(time.resolution)
  if (!is.null(inputfiles)) {
    files <- inputfiles
  } else {
    ## get file names and dates and full path
    files <- switch(hemisphere, 
                    north = raadfiles::nsidc_north_monthly_files(), 
                    south = raadfiles::nsidc_south_monthly_files())
  }
  if (returnfiles) return(files)
  # if (product == "amsr" & .Platform$OS.type == "windows") warning("sorry, AMSR2 files are HDF4 so this is unlikely to work on your machine")
  if (missing(date)) {
    if (latest) date <- max(files$date)  else date <- min(files$date)
  }
  date <- timedateFrom(date)
  files <- .processFiles(date, files, time.resolution)
  #browser()
  #read_ice_internal(files, hemisphere, rescale, setNA, xylim,  ...) 
  if (!rescale) message("`rescale = FALSE` is not supported for NSIDC v2")
#  raster::plot(brick(files$fullname, band = 1))
  read_ice_v2(files,  setNA, xylim)
}


read_ice_v2 <- function(files, setNA, xylim = NULL, ...) {
  #out <- raster::brick(files$fullname, band = 1L)
  suppressWarnings(out <- raster::brick(lapply(files$fullname, raster::raster)))
  out <- raster::calc(out, fun = 
   function(x) {
    x <- x * 100; 
    if (setNA) {
    x[!x > 0] <- NA;
    x[!x <= 100] <- NA}; 
      x
  }, ...)
  # if (setNA && nrow(files) > 1) {
  #   message("`setNA = FALSE` is not supported for reading multiple dates")
  # } 
  # if (setNA && nrow(files) == 1) {
  #   out[!out > 0] <- NA
  #   out[!out < 100] <- NA
  # }
  # 
 # browser(0)

  if (!is.null(xylim)) out <- raster::crop(out, raster::extent(xylim))
  setZ(out, files$date)
  
}
read_ice_internal <- function(files, hemisphere, rescale, setNA, xylim = NULL,  ...) {
  ## check that files are available
  ## NSIDC projection and grid size for the Southern Hemisphere
  prj  <- "+proj=stere +lat_0=-90 +lat_ts=-70 +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs " 
  if(hemisphere == "north") prj <-    "+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs"
  nsidcdims <- if (hemisphere == "south") c(332L, 316L) else c(448L, 304L)
  ext <- if (hemisphere == "south") c(-3950000, 3950000, -3950000, 4350000) else c(-3837500, 3762500, -5362500, 5837500)
  
  res <-  c(25000, 25000)
  
  #if (product == "ssmi") stop("sorry SSMI data is temporarily unavailable")
  
  rtemplate <- raster(extent(ext), nrows =  nsidcdims[1L], ncols = nsidcdims[2L], crs = prj)
  ## process xylim
  cropit <- FALSE
  if (!is.null(xylim)) {
    cropit <- TRUE
    cropext <- extent(xylim)
  }
  
  nfiles <- nrow(files)
  r <- vector("list", nfiles)
  
  fname <- files$fullname
  r <- vector("list", length(fname))
  #for (ifile in seq_len(nfiles)) {
   do_read <- function(ifile) { 
    dat <- tail(readBin(fname[ifile], "integer", size = 1, n = prod(nsidcdims) + 300L, endian = "little", signed = FALSE), 
                -300L)
   r100 <- dat > 250
    #r0 <- dat < 1
    if (rescale) {
      dat <- dat/2.5  ## rescale back to 100
    }
    if (setNA) {
      dat[r100] <- NA
      ##dat[r0] <- NA
    }

   #r0 <- raster(t(matrix(dat, nsidcdims[1])), template = rtemplate)
   r0 <- raster::setValues(rtemplate, dat)
   
    # 251  Circular mask used in the Arctic to cover the irregularly-shaped data gap around the pole (caused by the orbit inclination and instrument swath)
    # 252	Unused
    # 253	Coastlines
    # 254	Superimposed land mask
    # 255	Missing data
    # 
    ## ratify if neither rescale nor setNA set
    if (!setNA && !rescale) {
      ##r <- ratify(r)
      rat <- data.frame(ID = 0:255, icecover = c(0:250, "ArcticMask", "Unused", "Coastlines", "LandMask", "Missing"), 
                        code = 0:255, stringsAsFactors = FALSE)
      levels(r0) <- rat
    } 
    
    
    if (cropit) r0 <- crop(r0, cropext)
    #r[[ifile]] <- r0
    r0
   }
    r <- furrr::future_map(seq_along(fname), do_read)
  

  r <- raster::stack(r)
  projection(r) <- prj
  names(r) <- basename(files$fullname)
  r <- setZ(r, set_utc_format(files$date))

  if ("filename" %in% names(list(...))) r <- writeRaster(r, ...)
  r
  
}


#' Load metadata and location of files of sea ice data products.
#'
#' This function loads the latest cache of stored files for
#' ice products. 
#' 
#' The 'fullname' is the path to the raw NSIDC binary file, 'vrt_dsn' a VRT string
#' describing the fullname as a GDAL DSN string. 
#' @param time.resolution daily or monthly files?
#' @param product choice of sea ice product, see \code{\link{readice}}
#' @param hemisphere north or south
#' @param ... reserved for future use, currently ignored
#' @export
#' @importFrom raadfiles nsidc_south_monthly_files nsidc_north_monthly_files 
#' @examples
#' \dontrun{
#' icf <- icefiles()
#' icf[nrow(icf), ]
#' }
#' @return data.frame of \code{file} and \code{date}
icefiles <- function(time.resolution = "daily", 
                     product = "nsidc", hemisphere =c("south", "north"), ...) {
  
  if (product != "nsidc") stop("readice no longer supports AMSR or SSM/I, see specfic read functions for those")
  if (time.resolution != "daily") stop("readice no longer supports monthly time resolution, see specific read function for monthly data")
  
  hemisphere <- match.arg(hemisphere)
  
  files <- switch(hemisphere, 
                  north = raadfiles::nsidc_north_daily_files(), 
                  south = raadfiles::nsidc_south_daily_files())

  ## a bit of a kludge but saves us from the dates that don't exist in the new NSIDC NetCDF files
  rawdate <- as.integer(as.Date(files$date))
  bad <- rawdate %in% bad_nsidc
  files <- files[!bad, ]
  files$vrt_dsn <- sprintf(switch(hemisphere, north = .north_nsidc_vrt, south = .south_ndsic_vrt), files$fullname)
  files

}
# 
# ## system.time(icf <- icefiles(hemisphere = "south"))
# #user  system elapsed 
# #3.060   0.028   3.138
# # unique(dirname(dirname(icf$fullname)))
# # [1] "/rdsi/PRIVATE/raad/data/sidads.colorado.edu/pub/DATASETS/nsidc0051_gsfc_nasateam_seaice/final-gsfc/south/daily"
# # [2] "/rdsi/PRIVATE/raad/data/sidads.colorado.edu/pub/DATASETS/nsidc0081_nrt_nasateam_seaice"                        
# # [3] "/rdsi/PRIVATE/raad/data/sidads.colorado.edu/pub/DATASETS/nsidc0081_nrt_nasateam_seaice/F18_uncalibrated"    
# 
# ## system.time(icf2 <- .nsidc_south_daily_dbfiles())
# .nsidc_south_daily_dbfiles <- function() {
#   datadir <- getOption("default.datadir")
#   db <- dplyr::src_sqlite(file.path(datadir, "admin", "filelist", "allfiles.sqlite"))
#   tab <- dplyr::tbl(db, "file_list") %>% ## split the string search into two simpler parts makes it faster
#     filter(fullname %like% "%s.bin") %>% 
#     filter(fullname %like% "%nasateam_seaice%") %>% 
#     collect() %>% filter(!grepl("monthly", fullname)) %>% 
#     #filter(grepl("nrt_s.bin", fullname) | grepl("v1.1_s.bin", fullname)) %>% 
#     mutate(file = fullname, fullname = file.path(datadir, file)) %>% 
#     mutate(date = as.POSIXct(strptime(basename(fullname), "nt_%Y%m%d"), tz = "GMT"))
#   tab %>% arrange(date) %>% distinct(date, .keep_all = TRUE)
# }
