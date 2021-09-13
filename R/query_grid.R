#' Create a query grid
#' 
#' Create a grid of query values for of longitude, latitude, and date. 
#' 
#' This be used as the second argument to  [raadtools::extract()] for general 
#'  'point-in-time' value extraction. 
#'  
#' The ouput is a data frame of the grid expanded, a row for every combination of
#' longitude, latitude, date  - so can be quite large (!) so be careful with very 
#' fine resolution or long series.
#' @param lon vector of longitudes in the grid
#' @param lat vector of latitudes in the grid
#' @param date vector of dates in the grid
#' @export
#' @examples
#' lon <- c(100, 110, 120)
#' lat <-seq(-60, -40, by = 5)
#' dts <- seq(as.Date("2002-03-03"), by = "5 days", length.out = 10)
#' qd <- query_grid(lon = lon, lat = lat, date = dts)
#' ## use bilinear to ensure we get an interpolate value in the grid
#' qd$sst <- extract(readsst, qd, method = "bilinear")
#' ## ssha
#' qd$ssha <- extract(readssh, qd, ssha = TRUE, method = "bilinear")
#' qd$wind_mag <- extract(readwind, qd, magonly = TRUE, method = "bilinear") 
query_grid <- function(lon, lat, date) {
  tibble::tibble(x = rep(lon, length(lat) * length(date)), 
             y = rep(rep(lat,  each = length(lon)), length(date)), 
             date = rep(date, each = length(lon) * length(lat)))
}

