#' Altimetry products.
#'
#' Functions `read_sla_daily` and so on for "ugosa, adt, ugos, sla, vgos, vgosa, err".
#'
#'
#' The labels 'u', 'v' are used by another function `readcurr()` which reads the 'ugos' and 'vgos' components and corresponds to an older
#' but analogous dataset. 
#' 
#' 
#' 'ugos' is Absolute geostrophic velocity: zonal component, 'surface_geostrophic_eastward_sea_water_velocity' in m/s. 
#' 
#' 'vgos' is Absolute geostrophic velocity: meridian component, 'surface_geostrophic_northward_sea_water_velocity' in m/s. 
#' 
#' 'ugosa' is Geostrophic velocity anomalies: zonal component, 'surface_geostrophic_eastward_sea_water_velocity_assuming_sea_level_for_geoid' in m/s. 
#' 
#' 'vgosa' is Geostrophic velocity anomalies: meridian component, 'surface_geostrophic_northward_sea_water_velocity_assuming_sea_level_for_geoid' in m/s.
#' 
#' 'adt' is Absolute dynamic topography, 'sea_surface_height_above_geoid' in metres
#' 
#'  'sla' is Sea level anomaly, 'sea_surface_height_above_sea_level' in metres.
#' 
#' For the raw files see `raadfiles::altimetry_daily_files` and for example do 
#' 
#' 'files <- read_sla_daily(returnfiles = TRUE); ncdf4::nc_open(files$fullname[1])' which will give a full NetCDF metadata print out of the source files (all the variables are in each file). 
#'
#' @inheritParams raadtools
#' @name read_adt_daily
#' @aliases read_ugosa_daily read_ugos_daily read_sla_daily read_vgos_daily read_vgosa_daily read_err_daily
#' @return a raster layer
#' @export
#' @examples
#' a <- read_adt_daily(sort(Sys.Date() - 1:50),
#' xylim = extent(100, 150, -70, -40))
#' \dontrun{
#' animate(a, pause = 0,
#' col = colorRampPalette(c("dodgerblue", "white", "firebrick"))(21),
#' breaks = c(seq(min(cellStats(a, min)), 0, length = 11),
#'            seq(0.001, max(cellStats(a, max)), length = 10)))
#' }
read_ugosa_daily <- function(date, xylim = NULL, latest = TRUE, returnfiles = FALSE, ..., inputfiles = NULL) {
  varname <- "ugosa"
  read_copernicus_daily(date = date, xylim = xylim, latest  = latest, returnfiles = returnfiles, varname = varname, ..., inputfiles = inputfiles)
}

#' @export
read_adt_daily <- function(date, xylim = NULL, latest = TRUE, returnfiles = FALSE, ..., inputfiles = NULL) {
  varname <- "adt"
  read_copernicus_daily(date = date, xylim = xylim, latest  = latest, returnfiles = returnfiles, varname = varname, ..., inputfiles = inputfiles)
}
#' @name read_adt_daily
#' @export
read_ugos_daily <- function(date, xylim = NULL, latest = TRUE, returnfiles = FALSE, ..., inputfiles = NULL) {
  varname <- "ugos"
  read_copernicus_daily(date = date, xylim = xylim, latest  = latest, returnfiles = returnfiles, varname = varname, ..., inputfiles = inputfiles)
}
#' @name read_adt_daily
#' @export
read_sla_daily <- function(date, xylim = NULL, latest = TRUE, returnfiles = FALSE, ..., inputfiles = NULL) {
  varname <- "sla"
  read_copernicus_daily(date = date, xylim = xylim, latest  = latest, returnfiles = returnfiles, varname = varname, ..., inputfiles = inputfiles)
}
#' @name read_adt_daily
#' @export
read_vgos_daily <- function(date, xylim = NULL, latest = TRUE, returnfiles = FALSE, ..., inputfiles = NULL) {
  varname <- "vgos"
  read_copernicus_daily(date = date, xylim = xylim, latest  = latest, returnfiles = returnfiles, varname = varname, ..., inputfiles = inputfiles)
}


#' @name read_adt_daily
#' @export
read_vgosa_daily <- function(date, xylim = NULL, latest = TRUE, returnfiles = FALSE, ..., inputfiles = NULL) {
  varname <- "vgosa"
  read_copernicus_daily(date = date, xylim = xylim, latest  = latest, returnfiles = returnfiles, varname = varname, ..., inputfiles = inputfiles)
}

#' @name read_adt_daily
#' @export
read_err_daily <- function(date, xylim = NULL, latest = TRUE, returnfiles = FALSE, ..., inputfiles = NULL) {
  varname <- "err"
  read_copernicus_daily(date = date, xylim = xylim, latest  = latest, returnfiles = returnfiles, varname = varname, ..., inputfiles = inputfiles)
}



read_copernicus_daily <- function(date, xylim = NULL, latest = TRUE, returnfiles = FALSE, varname, lon180 = FALSE, ..., inputfiles = NULL) {
  if (is.null(inputfiles)){
    files <- raadfiles::altimetry_daily_files()
  } else {
    files <- inputfiles
  }
  if (returnfiles) return(files)
  if (missing(date)) {
    if (latest) {
      date <- max(files$date)
    } else {
      date <- min(files$date)
    }
  }
  date <- timedateFrom(date)
  files <- .processFiles(date, files, "daily")
  read0 <- function(x, varname) raster(x)

  nfiles <- nrow(files)
  ## progress
  pb <- progress::progress_bar$new(
    format = "  extracting [:bar] :percent in :elapsed",
    total = nfiles, clear = FALSE, width= 60)
  pb$tick(0)
  read_fun <- function(xfile, ext, msk, rot, varname = "", band = 1) {
    pb$tick()
    ## override passed in rot
    if (copernicus_is_atlantic(xfile) && !lon180) rot <- TRUE
    if (!copernicus_is_atlantic(xfile) && lon180) rot <- TRUE
    mask_if_needed(crop_if_needed(rotate_if_needed(raster(xfile, varname = varname, band = band), rot), ext), msk)
  }

  msk <- NULL
  rot <- FALSE
  files$band <- 1
  op <- options(warn = -1)
  r0 <- brick(stack(lapply(seq_len(nrow(files)), function(xi)
    read_fun(files$fullname[xi], ext = xylim, msk = msk, rot = rot, varname = varname, band = files$band[xi]))), ...)
  options(op)
  r0 <- setZ(r0, files$date)
 r0
  }
