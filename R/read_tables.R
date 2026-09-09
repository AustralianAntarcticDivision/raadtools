#' Read surface currents as table
#' 
#' Read meridional and zonal components of surface currents (m/s) from altimetry products. Input
#' is optional, with `date` and `xylim`. As with the grid reading functions [read_ugos_daily] and [read_vgos_daily]
#' this by default will return the latest data available and for the entire world. 
#' 
#' Please note that the coordinates are in longitude latitude, but the velocity components are in m/s. You cannot
#' meaningfully transform the x,y coordinates and use the velocity components without taking into account
#' rotation in the transformation (we might write some helpers for this ...). 
#' 
#' Argument `lon180` may be used to specify Pacific or Atlantic orientation. 
#'
#' @param date dates to read
#' @param xylim extent of data to read
#' @param ... arguments passed to read functions (only `lon180` relevant)
#' @param xy include coordinates of cell in the output, `TRUE` by default
#' @param cell include cell index in the output, `FALSE` by default
#' @param na.rm by default missing values are removed, set to `FALSE` to keep all
#'
#' @return data frame of u, v, x,y (longitude,latitude), cell, and date
#' @export
#'
#' @examples
#' uv <- table_uvgos("2001-01-01", xylim = terra::ext(60, 120, -60, -10))
#' plot(range(uv$x), range(uv$y), type = "n", asp = 1.1)
#' scal <- function(x) (x - min(x, na.rm = TRUE))/diff(range(x, na.rm = TRUE))
#' nn <- 56
#' arrows(uv$x, uv$y, uv$x + uv$u, uv$y + uv$v, col = grey.colors(nn)[scal(sqrt(uv$u^2 + uv$v^2)) * (nn-1) + 1], length = 0)
table_uvgos <- function(date, xylim = NULL, ..., xy = TRUE, cell = FALSE, na.rm = TRUE, latest = TRUE, res = NULL) {
  if (missing(date)) {
    rd <- range(raadfiles::altimetry_daily_files()$date)
    if (latest) {
       date <- rd[2] 
      } else {
       date <- rd[1L]      
      } 
  }
  date <- timedateFrom(date)
  ustack <- read_ugos_daily(date = date, xylim = xylim, ...)
  vstack <- read_vgos_daily(date = date, xylim = xylim, ...)
  if (!is.null(res)) {
   rs <- terra::res(ustack)
   if (res == rs[1L]) {
     
   }  else {
    fact <- res/mean(rs)
    
    ustack <- terra::aggregate(ustack, fact = fact)
    vstack <- terra::aggregate(vstack, fact = fact)
   }
  }
  uvalues <- as.vector(terra::values(ustack))
  mask <- TRUE
  if (na.rm) {
    mask <- !is.na(uvalues)
    uvalues <- uvalues[mask]
  }
  vvalues <- as.vector(terra::values(vstack))[mask]
  
  out <- tibble::tibble(u = uvalues, v = vvalues)
  ## nc is needed for the date column below whether or not cells are wanted,
  ## so it is not computed inside the if
  nc <- terra::ncell(ustack)
  if (cell || xy) {
    cells <- rep(seq_len(nc), terra::nlyr(ustack))[mask]
  }
  if (xy) {
    out[["x"]] <- terra::xFromCell(ustack, cells)
    out[["y"]] <- terra::yFromCell(ustack, cells)
  }
  if (cell) {
    out[["cell"]] <- cells
  }
  out[["date"]] <-  rep(date, each = nc)[mask]
  out
}