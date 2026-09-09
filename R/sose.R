#' SOSE varnames
#'
#' Available variable names from SOSE.
#'
#' These varnames can be used in `read_sose(varname = )`.
#' @return character vector
#' @export
#'
#' @param iteration SOSE iteration, passed to [raadfiles::sose_monthly_files()]
#' @examples
#' sose_monthly_varnames()
sose_monthly_varnames <- function(iteration = "")  {
  files <- raadfiles::sose_monthly_files(varname = "all", iteration = iteration)
  sort(unique(gsub("\\.nc$", "", unlist(lapply(strsplit(basename(files$fullname), "_"), tail, 1)))))
}


#' Read SOSE Southern Ocean State Estimate
#'
#' Model data read from files managed by
#' \code{\link[raadfiles]{sose_monthly_files}}. Dates are matched to file names by finding
#' the nearest match in time within a short duration. If \code{date}
#' is greater than length 1 then the sorted set of unique matches is
#' returned.
#'
#' This function doesn't support xylim, lon180, or more than one date being read.
#' @param date date to read, the nearest match is used
#' @param time.resolution monthly, the only resolution available
#' @param varname variable to return from the data files, default is whatever first value
#' from [sose_monthly_varnames()]
#' @param level, defaults to 1L - there are 52
#' @param setNA unused, kept for backwards compatibility; terra applies the
#' file's own _FillValue
#' @param latest if TRUE (and date not supplied) return the latest time available, otherwise the earliest
#' @param returnfiles ignore options and just return the file names and dates
#' @param ... currently ignored
#' @param inputfiles input the files data base to speed up initialization
#' @export
#' @return \code{SpatRaster}, one layer per requested level
#' @examples
#' \dontrun{
#' read_sose(varname = "Uvel", level = 1)
#' read_sose(varname = "SeaIceArea", level = 1, latest = FALSE)
#' read_sose(varname = "Chl", level = 10, latest = FALSE)
#'
#' }
read_sose <-  function (date, time.resolution = c("monthly"),
                      varname = "", level = 1L,
                      setNA = TRUE,
                      latest = TRUE,
                      returnfiles = FALSE,  ..., inputfiles = NULL) {
  time.resolution <- match.arg(time.resolution)
  if (is.null(inputfiles)) {
    files <- raadfiles::sose_monthly_files(varname = varname)
    ## expand file  list by dates in file
    nc <- RNetCDF::open.nc(files$fullname[1L])
    on.exit(RNetCDF::close.nc(nc), add = TRUE)
    dates <- RNetCDF::var.get.nc(nc, "time")
    unit <- RNetCDF::att.get.nc(nc, "time", "units")
    cal <- RNetCDF::utcal.nc(unit, dates)
    dates <- ISOdatetime(cal[,1], cal[,2], cal[,3], cal[,4], cal[,5], cal[,6], tz = "UTC")


    files <- tibble::tibble(fullname = files$fullname[1], date = dates, band = seq_along(dates))

  } else {
    files <- inputfiles
  }
  if (returnfiles)
    return(files)

  ## every time step in the file, needed below to work out how many depth
  ## layers there are
  ntime <- nrow(files)

  if (missing(date)) date <- if (latest) max(files$date) else min(files$date)

  if (length(date) > 1) {
    message("only one date may be read at a time")
    date <- date[1L]
  }
  date <- timedateFrom(date)
  files <- .processFiles(date, files, time.resolution)
  if (nrow(files) > 1) files <- files[1L, ]
  xfile <- files$fullname[1L]
  tband <- files$band[1L]

  ## raster's lvar = 4 has no terra equivalent. Reading with md = FALSE means
  ## GDAL flattens depth and time into bands, and it names each band
  ## <variable>_Z=<depth>_<time index> whichever way round the two dimensions
  ## are stored in the file (checked both ways). So the layers for one time
  ## step are the ones whose trailing index matches, and they arrive in depth
  ## order. A file with no depth dimension is just <variable>_<time index>,
  ## which lands in the same place with one level.
  src <- .rast_nc(xfile)
  at_date <- which(sub(".*_", "", names(src)) == as.character(tband))
  nlevel <- terra::nlyr(src) / ntime
  if (length(at_date) != nlevel) {
    stop(sprintf("cannot identify the depth layers of '%s': expected %g per time step, found %i",
                 basename(xfile), nlevel, length(at_date)))
  }
  if (any(level < 1L | level > nlevel)) {
    stop(sprintf("level must be between 1 and %g for this file", nlevel))
  }

  r0 <- src[[at_date[level]]]

  terra::ext(r0) <- terra::ext(0, 2 * 20037508, -14317550, -3445832)
  terra::crs(r0) <- "+proj=merc +datum=WGS84"

  names(r0) <- sprintf("level%i", level)
  terra::depth(r0) <- level
  terra::time(r0) <- rep(as.Date(files$date), length(level))

  r0

}
