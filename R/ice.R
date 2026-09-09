# The differences between the two projection pairs (EPSG 3411/3412 and EPSG
# 3413/3976) are minimal - the diagonal distance across a WGS 84/NSIDC Sea Ice
# Polar Stereographic 25 km grid cell differs about 1 m from the original NSIDC
# Sea Ice Polar Stereographic grid cell. However, users should note that EPSG
# codes 3411 and 3412 are deprecated, and NSIDC encourages all new products to
# use EPSG codes 3413 and 3976.
# 


#' Area of pixels in sea ice
#'
#' Read the NSIDC pixel-area files for either hemisphere. 
#' Only 25km product is supported. Tool name "psn25area_v3.dat and pss25area_v3.dat": 
#' \url{http://nsidc.org/data/polar-stereo/tools_geo_pixel.html#pixel_area}. 
#' @param product "nsidc" the 25km NSIDC passive microwave
#' @param ... ignored
#' @param hemisphere south (default) or north
#'
#' @return \code{SpatRaster} with the area of the cells in m^2
#' @export
#' @seealso readice
#' @examples
#' readice_area()
#' readice_area(hemisphere = "north")
readice_area <- function(product = "nsidc", hemisphere = "south", ...) {
  f <- dplyr::filter(raadfiles::get_raad_filenames(), stringr::str_detect(file, "polar-stereo")) %>% 
    dplyr::transmute(fullname = file.path(root, file))
  template <- .without_shim_warning(readice(product = product, hemisphere = hemisphere))
  ## find dat file
  south <- hemisphere == "south"
  patt <- if(south) "pss25area" else "psn25area"
  
  datfile <- dplyr::filter(f, stringr::str_detect(fullname, patt)) %>%  dplyr::pull(fullname)
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
  setNames(terra::setValues(terra::rast(template[[1]]), dat * 1000), "NSIDC_true_area_m2")
}


#' Load metadata and location of files of sea ice data products.
#'
#' This function loads the latest cache of stored files for
#' ice products. 
#' 
#' The 'fullname' is the path to the source file.
#'
#' \code{product = "cdr"} (the default) is the NOAA/NSIDC Climate Data Record,
#' G02202 V6. \code{product = "nsidc"} is NSIDC-0051 v2, which is what
#' \code{icefiles} used to return and what \code{\link{read_nsidc_ice_daily}}
#' still reads.
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
                     product = c("cdr", "nsidc"), hemisphere =c("south", "north"), ...) {

  product <- match.arg(product)
  if (time.resolution != "daily") stop("readice no longer supports monthly time resolution, see specific read function for monthly data")
  
  hemisphere <- match.arg(hemisphere)

  if (product == "cdr") {
    files <- switch(hemisphere,
                    north = raadfiles::nsidc_cdr_north_daily_files(),
                    south = raadfiles::nsidc_cdr_south_daily_files())
    ## bad_nsidc indexes dud NSIDC-0051 dates and says nothing about the CDR
    ## archive, so it is not applied here.
    return(files)
  }

  files <- switch(hemisphere, 
                  north = raadfiles::nsidc_north_daily_files(), 
                  south = raadfiles::nsidc_south_daily_files())

  ## a bit of a kludge but saves us from the dates that don't exist in the new NSIDC NetCDF files
  rawdate <- as.integer(as.Date(files$date))
  bad <- rawdate %in% bad_nsidc
  files[!bad, ]

}

## The CDR files carry eight subdatasets and the concentration is not the first
## of them - band 1 is cdr_seaice_conc_interp_spatial_flag. Address it by name
## rather than by position.
.cdr_conc_dsn <- function(fullname) {
  sprintf('NETCDF:"%s":cdr_seaice_conc', fullname)
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
