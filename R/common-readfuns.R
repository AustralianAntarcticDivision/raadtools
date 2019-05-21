crop_if_needed <- function(x, ext = NULL) {
  if (!is.null(ext)) {
    crop(x, ext, snap = "out") 
  } else {
    x
  }
}
mask_if_needed <- function(x, msk = NULL) {
  if (!is.null(msk)) {
    x <- mask(x, msk)
  } 
  x
}
rotate_if_needed <- function(x, rot = FALSE) {
  if (rot) .rotate(x) else x
}