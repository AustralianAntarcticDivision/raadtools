#' Circumpolar ROMS files. 
#' 
#' 
#' @param ... ignored
#'
#' @return data frame of fullname, date
#' @export
#'
#' @examples
#' cpolarfiles()
cpolarfiles <- function(...) {
  dplyr::mutate(dplyr::filter(raadtools::allfiles(), 
      grepl("s_corney/cpolar", fullname)), 
      date = as.POSIXct(strptime(sprintf("197%s-01", substr(basename(fullname), 12, 14)), "%Y%m-%d")))
}
