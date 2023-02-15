altimetry_daily_varname_files <- function(varname) {
  files <- raadfiles::altimetry_daily_files()
  ## FIXME: we have to split via date, because .vrt_dsn() assumes extent is constant
  filelist <- split(files, as.Date(files$date) >= as.Date("2020-06-04"))
  
  ## all of these now work correctly, MDSumner 2023-02-15
#  plot(read_adt_daily("2020-06-03"))
#  plot(read_adt_daily("2020-06-03", lon180 = T))
#  plot(read_adt_daily("2020-06-04", lon180 = T))
#  plot(read_adt_daily("2020-06-04", lon180 = F))
  
  for (i in seq_along(filelist)) {
    filelist[[i]]$vrt_dsn <- .vrt_dsn(filelist[[i]]$fullname, sds = varname, bands = 1L)
  }

  dplyr::arrange(do.call(rbind, filelist), "date")
}


#' Altimetry products.
#'
#' Functions `read_sla_daily` and so on for "ugosa, adt, ugos, sla, vgos, vgosa, err".
#'
#' sla is sea level anomaly, for the raw files see `raadfiles::altimetry_daily_files`
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

vlen <- function(x, y) sqrt(x * x + y * y)
#' @name read_adt_daily
#' @export
read_mag_daily <- function(date, xylim = NULL, latest = TRUE, returnfiles = FALSE, ..., inputfiles = NULL) {
  u <- read_ugos_daily(date = date, xylim = xylim, latest  = latest, returnfiles = returnfiles,  ..., inputfiles = inputfiles)
  if (returnfiles) return(u)
  v <- read_vgos_daily(date = date, xylim = xylim, latest  = latest, returnfiles = returnfiles,  ..., inputfiles = inputfiles)
  vlen(u, v)
}

#' @name read_adt_daily
#' @export
read_dir_daily <- function(date, xylim = NULL, latest = TRUE, returnfiles = FALSE, ..., inputfiles = NULL) {
  u <- read_ugos_daily(date = date, xylim = xylim, latest  = latest, returnfiles = returnfiles,  ..., inputfiles = inputfiles)
  if (returnfiles) return(u)
  v <- read_vgos_daily(date = date, xylim = xylim, latest  = latest, returnfiles = returnfiles,  ..., inputfiles = inputfiles)
  (90 - atan2(v, u) * 180/pi) %% 360
}

read_copernicus_daily <- function(date, xylim = NULL, latest = TRUE, returnfiles = FALSE, varname, lon180 = FALSE, ..., inputfiles = NULL) {
  if (is.null(inputfiles)){
    files <- altimetry_daily_varname_files(varname)
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

  nfiles <- nrow(files)
  ## progress
  pb <- progress::progress_bar$new(
    format = "  extracting [:bar] :percent in :elapsed",
    total = nfiles, clear = FALSE, width= 60)
  pb$tick(0)

  op <- options(warn = -1)
  if (is.null(xylim)) {
    xylim <- c(0, 360, -90, 90)
    if (lon180) xylim <- c(-180, 180, -90, 90)
  }
  r0 <- brick(stack(lapply(seq_len(nrow(files)), function(xi)
    read_fun(files$vrt_dsn[xi], ext = xylim,  varname = varname, progress = pb))), ...)
  options(op)
  r0 <- setZ(r0, files$date)
 r0
  }
