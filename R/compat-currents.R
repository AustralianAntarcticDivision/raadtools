# R/compat-currents.R
# Legacy shim for readcurr() - dispatches to terra-native CMEMS readers
# Returns raster::brick() for backward compatibility

#' Read ocean current data
#'
#' @description
#' `r lifecycle::badge("superseded")`
#'
#' \code{readcurr} is superseded by the CMEMS current readers:
#' \itemize{
#'   \item \code{\link{read_cmems_ugos_daily}} - U component
#'   \item \code{\link{read_cmems_vgos_daily}} - V component
#'   \item \code{\link{read_cmems_current_speed_daily}} - current speed
#'   \item \code{\link{read_cmems_current_direction_daily}} - current direction
#' }
#'
#' @param date date or dates of data to read
#' @param xylim spatial extents to crop
#' @param lon180 if TRUE (default), use Atlantic-centered longitude
#' @param magonly return current speed only
#' @param dironly return current direction only
#' @param uonly return U component only
#' @param vonly return V component only
#' @param latest if TRUE and date missing, return latest
#' @param returnfiles if TRUE, return file catalog
#' @param ... passed to underlying reader
#' @param inputfiles optional pre-filtered file catalog
#'
#' @return \code{RasterBrick} or \code{RasterLayer}, or tibble if \code{returnfiles = TRUE}
#'
#' @seealso \code{\link{read_cmems_ugos_daily}}, \code{\link{read_cmems_vgos_daily}},
#'   \code{\link{read_cmems_current_speed_daily}}, \code{\link{read_cmems_current_direction_daily}}
#'
#' @export
readcurr <- function(date,
                     xylim = NULL,
                     lon180 = TRUE,
                     magonly = FALSE,
                     dironly = FALSE,
                     uonly = FALSE,
                     vonly = FALSE,
                     latest = TRUE,
                     returnfiles = FALSE,
                     ...,
                     inputfiles = NULL) {

  # Validate flags
  flags <- c(magonly, dironly, uonly, vonly)
  if (sum(flags) > 1) {
    stop("only one of 'magonly', 'dironly', 'uonly', 'vonly' may be TRUE")
  }

  if (isTRUE(getOption("raadtools.shim.warn", TRUE))) {
    new_fn <- if (magonly) "read_cmems_current_speed_daily"
              else if (dironly) "read_cmems_current_direction_daily"
              else if (uonly) "read_cmems_ugos_daily"
              else if (vonly) "read_cmems_vgos_daily"
              else "read_cmems_ugos_daily/read_cmems_vgos_daily"

    .Deprecated(new_fn, package = "raadtools",
      msg = paste0(
        "'readcurr' is deprecated. ",
        "Use '", new_fn, "' for terra-native output.\n",
        "Set options(raadtools.shim.warn = FALSE) to suppress this warning."
      ))
  }

  # Dispatch based on flags
  if (magonly) {
    r <- read_cmems_current_speed_daily(
      date = date, xylim = xylim, lon180 = lon180,
      latest = latest, returnfiles = returnfiles, inputfiles = inputfiles, ...
    )
  } else if (dironly) {
    r <- read_cmems_current_direction_daily(
      date = date, xylim = xylim, lon180 = lon180,
      latest = latest, returnfiles = returnfiles, inputfiles = inputfiles, ...
    )
  } else if (uonly) {
    r <- read_cmems_ugos_daily(
      date = date, xylim = xylim, lon180 = lon180,
      latest = latest, returnfiles = returnfiles, inputfiles = inputfiles, ...
    )
  } else if (vonly) {
    r <- read_cmems_vgos_daily(
      date = date, xylim = xylim, lon180 = lon180,
      latest = latest, returnfiles = returnfiles, inputfiles = inputfiles, ...
    )
  } else {
    # Both U and V - single date only
    if (returnfiles) {
      return(read_cmems_ugos_daily(returnfiles = TRUE, inputfiles = inputfiles))
    }

    # Handle date
    files <- inputfiles %||% raadfiles::altimetry_daily_files()
    if (missing(date)) {
      date <- if (latest) max(files$date) else min(files$date)
    }
    date <- timedateFrom(date)

    # Check for multiple dates
    if (length(date) > 1) {
      warning("only one date can be read at a time unless 'magonly', 'dironly', 'uonly' or 'vonly' is TRUE; using first date")
      date <- date[1]
    }

    u <- read_cmems_ugos_daily(date, xylim = xylim, lon180 = lon180,
                                latest = latest, inputfiles = inputfiles, ...)
    v <- read_cmems_vgos_daily(date, xylim = xylim, lon180 = lon180,
                                latest = latest, inputfiles = inputfiles, ...)

    r <- c(u, v)
    names(r) <- c("U", "V")
    terra::time(r) <- rep(terra::time(u), 2)
  }

  if (returnfiles) return(r)

  # Convert to raster for backward compat
  out <- raster::brick(r)
  raster::setZ(out, terra::time(r))
}


#' @rdname readcurr
#' @export
readcurrents <- readcurr
