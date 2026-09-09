#' Read a single SRTM tile
#' 
#' Read a tile by x and y index. 
#' 
#' See [raadfiles::srtm_files()] for filename in `fullname` and `x`, `y` for tile indices. 
#' 
#' @return SpatRaster
#' @keywords internal
read_srtm_tile <- function(tile_x = 39, tile_y = 6) {
  files <- raadfiles::srtm_files()
  .rast_nc(dplyr::filter(files, x == tile_x, y == tile_y)$fullname[1])
}