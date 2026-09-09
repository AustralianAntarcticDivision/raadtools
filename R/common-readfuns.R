## Shared by the readers: rotate, crop and mask, each only when asked for.
##
## These dispatched on the class of x while the readers were being converted
## one at a time. Every caller returns a SpatRaster now, so the raster
## branches are gone. .rotate() in utils.R stays, because fronts.R and
## currents.R still call it directly.

crop_if_needed <- function(x, ext = NULL) {
  if (is.null(ext)) return(x)
  terra::crop(x, .as_ext(ext), snap = "out")
}

mask_if_needed <- function(x, msk = NULL) {
  if (is.null(msk)) return(x)
  terra::mask(x, msk)
}

rotate_if_needed <- function(x, rot = FALSE) {
  if (!rot) return(x)
  terra::rotate(x)
}

## One layer from one file, rotated, cropped and masked as asked.
##
## Every multi-file reader had a private copy of this, differing only in
## whitespace, so it lives here once now. varname is optional because some
## sources have a single unnamed variable and terra will not take subds = "".
## Subsetting a band after the read costs nothing: terra does not touch the
## values until they are asked for.
read_one <- function(xfile, ext = NULL, msk = NULL, rot = FALSE,
                     varname = "", band = 1L) {
  r <- if (nzchar(varname)) .rast_nc(xfile, subds = varname) else .rast_nc(xfile)
  if (!is.na(band) && band > 1L) r <- r[[band]]
  mask_if_needed(crop_if_needed(rotate_if_needed(r, rot), ext), msk)
}
