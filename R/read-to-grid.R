## Read sources straight onto a target grid with the GDAL warper.
##
## A target grid is the three things a viewer knows about what it is showing:
## a CRS, an extent in that CRS, and a size in pixels. Reading onto it, rather
## than reading the native grid and then cropping, resampling or projecting,
## lets GDAL pick an overview and read only the source pixels the grid needs,
## so a whole-ocean view at a few hundred pixels costs a few hundred pixels
## and not the full native resolution. It is also the one place where polar
## and antimeridian-crossing targets are handled properly, because the warper
## works per target pixel instead of from transformed corners.
##
## This is the data side of a viewer mode: anything that can say (crs,
## extent, dim) - a SpatRaster template, or the settled view state sent by a
## served aobcore page - can drive a reader. It has no viewer dependencies.


## Normalise a target grid specification.
##
## Accepts
##   - a SpatRaster (or Raster*) template: its CRS, extent and ncol/nrow
##   - a list with `crs`, `extent` = c(xmin, xmax, ymin, ymax) and `dim` =
##     c(ncol, nrow). `size_px` is accepted for `dim`, so the view state from
##     an aobcore served page (whose CRS is the scene's view CRS) can be given
##     as list(crs = scene$view$crs, extent = vs$extent, size_px = vs$size_px).
##
## Returns a list(crs, extent, dim) of class "raad_grid".
.as_grid <- function(x) {
  if (inherits(x, "raad_grid")) return(x)
  if (inherits(x, "BasicRaster")) x <- terra::rast(x)
  if (inherits(x, "SpatRaster")) {
    x <- list(crs = terra::crs(x),
              extent = as.vector(terra::ext(x)),
              dim = c(terra::ncol(x), terra::nrow(x)))
  }
  if (!is.list(x)) {
    stop("a target grid must be a SpatRaster template, or a list with ",
         "'crs', 'extent' = c(xmin, xmax, ymin, ymax) and 'dim' = c(ncol, nrow)",
         call. = FALSE)
  }
  dim <- x[["dim"]] %||% x[["size_px"]]
  crs <- x[["crs"]]
  extent <- x[["extent"]]
  if (inherits(extent, "SpatExtent")) extent <- as.vector(extent)

  if (!is.character(crs) || length(crs) != 1L || is.na(crs) || !nzchar(crs)) {
    stop("target grid 'crs' must be a single non-empty string", call. = FALSE)
  }
  if (!is.numeric(extent) || length(extent) != 4L || anyNA(extent) ||
      !(extent[1L] < extent[2L] && extent[3L] < extent[4L])) {
    stop("target grid 'extent' must be c(xmin, xmax, ymin, ymax) with ",
         "xmin < xmax and ymin < ymax", call. = FALSE)
  }
  if (!is.numeric(dim) || length(dim) != 2L || anyNA(dim) ||
      any(dim < 1) || any(dim != round(dim))) {
    stop("target grid 'dim' must be c(ncol, nrow), two positive whole numbers",
         call. = FALSE)
  }
  structure(list(crs = crs, extent = as.numeric(unname(extent)),
                 dim = as.integer(unname(dim))),
            class = "raad_grid")
}

## An empty SpatRaster on the grid.
.grid_rast <- function(grid) {
  grid <- .as_grid(grid)
  terra::rast(terra::ext(grid$extent), ncols = grid$dim[1L],
              nrows = grid$dim[2L], crs = grid$crs)
}

## A GDAL DSN for one variable of a netCDF file.
.nc_dsn <- function(file, varname) {
  sprintf("NETCDF:\"%s\":%s", file, varname)
}

## Read sources onto a target grid.
##
## dsn: character, GDAL data source names, one per output layer (typically
##   one per time step). Name the subdataset in the DSN (see .nc_dsn()).
## grid: anything .as_grid() accepts.
## band: band to read from each dsn, recycled to length(dsn).
## resample: GDAL resampling method.
## source_crs, source_extent: assign these to every source before warping,
##   for files whose georeferencing is missing or wrong (e.g. OISST has no
##   grid_mapping). source_extent is c(xmin, xmax, ymin, ymax). A
##   geographic source on 0,360 longitudes may be read onto a -180,180
##   target (or any other) directly: no rotate is needed.
##
## Returns a SpatRaster on the grid with one layer per dsn. Scale and offset
## are applied and nodata is NA.
.read_to_grid <- function(dsn, grid, band = 1L, resample = "bilinear",
                          source_crs = NULL, source_extent = NULL) {
  grid <- .as_grid(grid)
  if (!is.character(dsn) || !length(dsn) || anyNA(dsn)) {
    stop("'dsn' must be a character vector of data source names", call. = FALSE)
  }
  band <- rep_len(as.integer(band), length(dsn))
  if (!is.null(source_extent)) {
    source_extent <- as.numeric(source_extent)
    if (length(source_extent) != 4L) {
      stop("'source_extent' must be c(xmin, xmax, ymin, ymax)", call. = FALSE)
    }
  }
  template <- .grid_rast(grid)

  vals <- lapply(seq_along(dsn), function(i) {
    ## Subset the band in a VRT first, and warp that one-band source. Given
    ## the whole dataset and bands = , the warp reads every band: a monthly
    ## OISST file has 500+ bands, and one band took minutes instead of
    ## milliseconds.
    v <- tryCatch({
      src <- vapour::vapour_vrt(dsn[i], bands = band[i], extent = source_extent,
                                projection = source_crs)
      vapour::vapour_warp_raster_dbl(src,
                                     extent = grid$extent,
                                     dimension = grid$dim,
                                     projection = grid$crs,
                                     resample = resample)
    }, error = function(e) {
        stop(sprintf("could not read '%s' onto the target grid: %s",
                     dsn[i], conditionMessage(e)), call. = FALSE)
      })
    terra::setValues(template, v)
  })
  terra::rast(vals)
}
