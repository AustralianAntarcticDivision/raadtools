## The readers are being converted from raster to terra one at a time, so for
## as long as that takes these three are handed both kinds of object. They
## dispatch on what they were given rather than assuming, which is what lets a
## reader be converted on its own instead of in a flag day with its five
## neighbours.
##
## When the last caller returns a SpatRaster, the raster branches go.

crop_if_needed <- function(x, ext = NULL) {
  if (is.null(ext)) return(x)
  if (inherits(x, "SpatRaster")) {
    terra::crop(x, .as_ext(ext), snap = "out")
  } else {
    raster::crop(x, ext, snap = "out")
  }
}

mask_if_needed <- function(x, msk = NULL) {
  if (is.null(msk)) return(x)
  if (inherits(x, "SpatRaster")) {
    terra::mask(x, msk)
  } else {
    raster::mask(x, msk)
  }
}

rotate_if_needed <- function(x, rot = FALSE) {
  if (!rot) return(x)
  if (inherits(x, "SpatRaster")) {
    terra::rotate(x)
  } else {
    .rotate(x)
  }
}
