## AMPS - the Antarctic Mesoscale Prediction System, WRF forecast output as GRIB.
##
## The archive has changed shape three times since 2015, so nothing in here may
## assume a fixed band layout or a fixed grid. Probed against the live
## collection 2026-09-09, 31099 d1 files and 31080 d2 files:
##
##   from 2015-10-25  d1 329x435 at 30 km   d2 666x627 at 10 km
##                    254 bands in the f000 files; 250 in the later forecast
##                    hours, which drop the four leading surface bands and so
##                    carry no ice cover or land mask at all.
##                    u at 10 m is band 5, or band 1 in the 250-band files.
##   from 2018-11-16  d1 412x544 at 24 km   d2 834x786 at 8 km
##                    254 bands at every forecast hour. u at 10 m is band 5.
##   from 2021-12-13  the same grids, 294 to 298 bands: 42 hybrid-level height
##                    and pressure bands are inserted ahead of the winds, so
##                    u at 10 m is band 47.
##
## Bands are therefore addressed by NAME, never by number. GDAL names a GRIB
## band "<level>; <variable description> [<unit>]", for example
##
##   "10[m] HTGL (Specified height level above ground); u-component of wind [m/s]"
##   "1[-] SFC (Ground or water surface); Ice cover (ice=1;no ice=0) [proportion]"
##
## and those names hold across all three layouts. The band numbers above are
## recorded only so the change is on the record; nothing here uses them.
##
## Every read goes through .rast_nc(), which sets md = FALSE. That matters more
## than usual here: a bare terra::rast() on a GRIB takes the GDAL multidim
## route, which returns the layers sorted alphabetically by element and silently
## drops any band whose geometry differs from the first, so band order is lost
## and nlyr() is no longer the band count.
##
## The grid comes from the file. Files declare lat_ts = -60 rather than WRF's
## -71 because GRIB1 defines polar stereographic increments in the 60-degree
## plane; the two describe the same physical grid, since
## ((1+sin 60)/2) / ((1+sin 71)/2) = 0.959140 and 30000 * 0.959140 = 28774.
## The file's version is also the more accurate: against the corner coordinates
## AMPS publishes for the early d2 domain,
##
##   ## grid 2, Continental 10 km, WRF mass grid, 666 x 627
##   ## http://www2.mmm.ucar.edu/rt/amps/information/configuration/maps.html
##   ## lower left  (centre of the lower left mass grid cell): -50.80146 N,   49.79108 E
##   ## upper right (centre of the upper right mass grid cell): -48.20068 N, -136.12355 E
##
## the file lands within 0.001 degrees of the published lower left corner, where
## the extent this package used to hard-code was out by 0.06, about 4 km.

## The level sequence of a 3D field: the near-surface entry first, then the
## isobaric levels in file order. That is the `level` the raster-era readers
## documented - 1 is the 10 m (or 2 m) field, 2 onwards are 1000 hPa downwards.
## Hybrid (HYBL) and geopotential-metre (GPML) levels are deliberately left out;
## they are not part of that sequence and only exist in some eras.
.amps_level_bands <- function(nms, description) {
  hit <- grepl(description, nms, fixed = TRUE)
  c(which(hit & grepl("HTGL", nms, fixed = TRUE)),
    which(hit & grepl("ISBL", nms, fixed = TRUE)))
}

.amps_band <- function(nms, description, level = 1L) {
  b <- .amps_level_bands(nms, description)
  if (length(b) < 1L) {
    stop(sprintf("no '%s' band in this file: %i bands, the first named '%s'",
                 description, length(nms), nms[1L]))
  }
  if (any(level < 1L | level > length(b))) {
    stop(sprintf("'level' must be between 1 and %i for '%s' in this file",
                 length(b), description))
  }
  b[level]
}

## A surface field that has no level sequence, matched on its description.
.amps_surface_band <- function(nms, description) {
  b <- which(grepl(description, nms, fixed = TRUE))
  if (length(b) < 1L) {
    stop(sprintf("no '%s' band in this file: %i bands, the first named '%s'.\n  The later forecast hours of the early files carry no surface fields; use an f000 file.",
                 description, length(nms), nms[1L]))
  }
  b[1L]
}

## Open a file and check it brought its own georeferencing. It always does now;
## the hard-coded grid this replaced only ever described the pre-2018 domains.
.amps_src <- function(xfile) {
  src <- suppressWarnings(.rast_nc(xfile))
  if (!nzchar(terra::crs(src))) {
    stop(sprintf("'%s' carries no georeferencing; raadtools no longer supplies one",
                 basename(xfile)))
  }
  src
}

.amps_files <- function(inputfiles, lister, time.resolution) {
  files <- if (!is.null(inputfiles)) inputfiles else lister(time.resolution = time.resolution)
  ## .processFiles() indexes the catalogue with findInterval(), which needs the
  ## dates sorted; not every catalogue, or every hand-built inputfiles, is.
  if (is.unsorted(files$date)) files <- files[order(files$date), ]
  files
}

.amps_date <- function(date, files, latest) {
  if (is.null(date)) date <- if (latest) max(files$date) else min(files$date)
  timedateFrom(date)
}

#' AMPS files
#'
#' @inheritParams windfiles
#' @importFrom tibble tibble
#' @importFrom dplyr %>% arrange filter mutate
#' @importFrom raadfiles amps_d1files amps_d2files amps_model_files
#' @export amps_d1files amps_d2files
#' @export
amps_d1_icefiles <- function(data.source = "", time.resolution = "12hourly", ...) {
  files <- amps_model_files(data.source = data.source, time.resolution = time.resolution,  ...)
  ## TODO normalize file set
  ## we want the most files with the highest preference
  ##
  ## arrange(): amps_model_files() does not come back in date order, and every
  ## reader indexes its catalogue with findInterval(), which needs one.
  arrange(filter(files, grepl("f000", basename(fullname)), as.integer(hour) == 0), date)
}

#' Read AMPS sea ice cover
#'
#' The `ICEC` field of the AMPS d1 forecast files, as a fraction. This reads
#' from the 00UT and 12UT analysis files only, which is what
#' \code{\link{amps_d1_icefiles}} selects: the later forecast hours of the
#' pre-2018 files carry no surface fields at all.
#'
#' @inheritParams readamps_d1wind
#' @return \code{SpatRaster}, or the file catalogue if \code{returnfiles = TRUE}
#' @seealso \code{\link{readamps_d1wind}} for the wind fields on the same grid
#' @export
#' @examples
#' \dontrun{
#' terra::plot(readamps_d1ice(latest = TRUE))
#' }
readamps_d1ice <- function(date, time.resolution = "12hourly", xylim = NULL,
                           latest = TRUE, returnfiles = FALSE, ..., inputfiles = NULL) {

  files <- .amps_files(inputfiles, amps_d1_icefiles, time.resolution)
  if (returnfiles) return(files)

  files <- .processFiles(.amps_date(if (missing(date)) NULL else date, files, latest),
                         files, time.resolution)
  cropext <- .as_ext(xylim)

  r <- vector("list", nrow(files))
  for (ifile in seq_len(nrow(files))) {
    src <- .amps_src(files$fullname[ifile])
    ## GRIB_UNIT is [proportion], so this is already a fraction
    band <- .amps_surface_band(names(src), "Ice cover")
    r0 <- src[[band]]
    r[[ifile]] <- crop_if_needed(r0, cropext)
  }

  out <- if (length(r) > 1L) terra::rast(r) else r[[1L]]
  names(out) <- sprintf("ice_%s", format(files$date, "%Y%m%d_%H%M"))
  terra::time(out) <- files$date
  .write_if_filename(out, ...)
}

## The wind readers differ only in which catalogue they draw on, so they share
## everything below. d1 and d2 were 96 duplicated lines, and the duplication is
## what produced the undefined 'ifile' in the d2 copy.
.read_amps_wind <- function(date, files, xylim, magonly, dironly, uonly, vonly,
                            level, latest, time.resolution, ...) {

  if ((magonly + dironly + uonly + vonly) > 1) {
    stop("only one of magonly, dironly, uonly or vonly may be used, exiting")
  }
  if (length(level) > 1L) {
    warning("'level' should be of length = 1, using first supplied")
    level <- level[1L]
  }

  files <- .processFiles(.amps_date(date, files, latest), files, time.resolution)

  ## One date at a time unless the result is a single layer, because a U and V
  ## pair per date has nowhere to go in a flat layer list. terra could carry it,
  ## but that would change what these functions return.
  single <- magonly || dironly || uonly || vonly
  if (nrow(files) > 1L && !single) {
    warning("only one time index can be read at once unless 'magonly', 'dironly', 'uonly' or 'vonly' is TRUE")
    files <- files[1L, ]
  }

  cropext <- .as_ext(xylim)
  r <- vector("list", nrow(files))

  for (ifile in seq_len(nrow(files))) {
    src <- .amps_src(files$fullname[ifile])
    nms <- names(src)
    ub <- .amps_band(nms, "u-component of wind", level)
    vb <- .amps_band(nms, "v-component of wind", level)
    u <- src[[ub]]
    v <- src[[vb]]

    r0 <- if (magonly) sqrt(u * u + v * v)
          else if (dironly) .uv_direction(u, v)
          else if (uonly) u
          else if (vonly) v
          else c(u, v)

    r[[ifile]] <- crop_if_needed(r0, cropext)
  }

  out <- if (length(r) > 1L) terra::rast(r) else r[[1L]]

  if (single) {
    ## the archive is 3-hourly, so the old "%Y%m%d" gave colliding names
    ## whenever more than one forecast time from a day was read
    names(out) <- sprintf("wind_%s", format(files$date, "%Y%m%d_%H%M"))
    terra::time(out) <- files$date
  } else {
    names(out) <- sprintf("%swind_%s", c("U", "V"), format(files$date, "%Y%m%d_%H%M"))
    terra::time(out) <- rep(files$date, 2L)
  }

  .write_if_filename(out, ...)
}

#' read AMPS data
#'
#' Read from 	The Antarctic Mesoscale Prediction System (AMPS) files.

#' \code{readamps_d1wind} reads the "d1" \code{level} wind (defaults to 1).
#'
#' \code{readamps_d2wind} reads the same fields from the finer "d2" domain.
#'
#' \code{level} 1 is the 10 m wind; 2 onwards are the isobaric levels in file
#' order, starting at 1000 hPa and working up. How many there are varies with
#' the file, and an out of range \code{level} is an error rather than a silent
#' substitution.
#'
#' See \code{\link{amps_metadata}} for a description of the bands of one
#' 2015-era file. It is a snapshot, not an index: the band layout has changed
#' three times since, so these readers find their bands by name in the file at
#' hand rather than by position in that table.
#'
#' Data  http://www2.mmm.ucar.edu/rt/amps/wrf_grib/, and contain gridded forecast data from the AMPS-WRF model.
#' The 'd1' (domain 1) files were on a 30km grid and are now 24km, while the 'd2' (domain 2) files
#' were 10km and are now 8km; the change happened around 2018-11-16 and the grid comes from each file.
#' The grid domains are shown at  http://polarmet.osu.edu/AMPS/ - d2 extends out to southern Australia and the tip of South America, and
#' d1 covers just Antarctica (including Davis) and parts of the Southern Ocean.
#' There are two forecast cycles, starting at 00UT and 12UT daily. Forecasts are for +00, +03, +06, +09, +12 and +15 hours.
#' These steps can be used to see how the wind field is forecast as changing, and to obtain two different data sets each
#' 3 hour timestep (e.g. the +15h forecast for the 00UT run is at the same time as the +03h forecast from the 12UT run).
#' The 00UT and 12UT runs of the pre-2018 files are the only ones that carry the surface fields, so
#' \code{\link{readamps_d1ice}} reads from those alone.
#'
#' @param date date or dates of data to read
#' @param time.resolution ignored, kept for backward compatibility
#' @param xylim spatial extents to crop from source data, anything acceptable
#' to \code{\link[terra]{ext}}
#' @param magonly return wind magnitude only
#' @param dironly return wind direction only
#' @param uonly return the u component only
#' @param vonly return the v component only
#' @param level 1 for the 10 m wind, 2 onwards for the isobaric levels
#' @param latest if TRUE (and date not supplied) return the latest time available, otherwise the earliest
#' @param returnfiles ignore options and just return the file names and dates
#' @param inputfiles input the files data base to speed up initialization
#' @param ... passed to \code{\link[terra]{writeRaster}} if \code{filename} is given
#' @return \code{SpatRaster}, or the file catalogue if \code{returnfiles = TRUE}
#'
#' @examples
#' \dontrun{
#' af <- amps_d1files()
#' w <- readamps_d1wind(latest = TRUE, inputfiles = af)
#' vlen <- function(a, b) sqrt(a * a + b * b)
#' terra::plot(vlen(w[[1]], w[[2]]))
#'
#' ## the 850 hPa wind rather than the 10 m wind
#' readamps_d1wind(latest = TRUE, inputfiles = af, level = 8)
#' }
#' @export
readamps_d1wind <- function(date, time.resolution = "4hourly", xylim = NULL,
                            magonly = FALSE, dironly = FALSE, uonly = FALSE, vonly = FALSE,
                            latest = TRUE, returnfiles = FALSE, level = 1, ..., inputfiles = NULL) {

  files <- .amps_files(inputfiles, amps_d1files, time.resolution)
  if (returnfiles) return(files)

  .read_amps_wind(if (missing(date)) NULL else date, files, xylim,
                  magonly, dironly, uonly, vonly, level, latest, time.resolution, ...)
}

#' @rdname readamps_d1wind
#' @export
readamps_d2wind <- function(date, time.resolution = "4hourly", xylim = NULL,
                            magonly = FALSE, dironly = FALSE, uonly = FALSE, vonly = FALSE,
                            latest = TRUE, returnfiles = FALSE, level = 1, ..., inputfiles = NULL) {

  files <- .amps_files(inputfiles, amps_d2files, time.resolution)
  if (returnfiles) return(files)

  .read_amps_wind(if (missing(date)) NULL else date, files, xylim,
                  magonly, dironly, uonly, vonly, level, latest, time.resolution, ...)
}

## Builds the amps_metadata dataset from the gdalinfo of one 2015-era d1 file
## shipped in inst/extdata. See data-raw/amps_metadata.R. Kept as the record of
## that layout; the readers no longer consult it.
parse_amps_meta <- function(){
  tx <- readLines(system.file("extdata/amps/ampsfile_gdalinfo.txt", package= "raadtools"))
  idx <- grep("Description", tx)
  description <- gsub("Description = ", "", tx[idx])
  l <- lapply(idx, function(x) gsub("\\s+", "", tx[x + 1 + 1:7]))
  nms <- unlist(lapply(strsplit(l[[1]], "="), "[", 1))
  d <- bind_rows(lapply(l, function(x) tibble::as_tibble(stats::setNames(lapply(strsplit(x, "="), "[", 2), nms))), .id = "Band")
  d$Band <- as.integer(d$Band)
  d
}
