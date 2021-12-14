
altimetry_antarctica_daily_files <- function() {
    files <- raadfiles::altimetry_antarctica_files()
  nc <- RNetCDF::open.nc(files$fullname[1])    
  time <- RNetCDF::var.get.nc(nc, "time")
  times <- RNetCDF::utcal.nc(RNetCDF::att.get.nc(nc, "time", "units"), value = time)
  out <- tibble::tibble(date = ISOdatetime(times[,"year"], times[, "month"], times[, "day"], times[, "hour"], times[, "minute"], times[, "second"], tz = "UTC"))
  out$fullname <- files$fullname[1L]
  out$band <- seq_along(out$date)
  out$root <- files$root[1L]
  out
}
read_altimetry_antarctica_daily <- function(date, xylim = NULL, latest = TRUE, returnfiles = FALSE, varname, lon180 = FALSE, ..., inputfiles = NULL) {
  if (is.null(inputfiles)){
    files <- altimetry_antarctica_daily_files()
    
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
    rrr <- raster(xfile, varname = varname, band = band)
    rrr <- raster::setExtent(flip(flip(t(rrr), "x"), "y"), raster::extent(c(-1, 1, -1, 1) * 4375000 ))
    rrr <- crop_if_needed(rrr, ext)
    rrr[rrr > 1e36] <- NA
    rrr
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
