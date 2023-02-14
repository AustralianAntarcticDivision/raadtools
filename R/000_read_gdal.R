
read_fun <- function(xfile, ext, varname  ="", progress = NULL) {
  if (!is.null(progress)) progress$tick()
  xdata <- vapour:::gdal_raster_data(xfile, target_ext = numeric_ext(ext))
  template <- raster::raster(terra::rast(terra::ext(ext), ncols = attr(xdata, "dimension")[1L], nrows = attr(xdata, "dimension")[2L], crs = attr(xdata, "projection")))
  raster::setValues(template, xdata[[1]])
}