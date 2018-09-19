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
  dplyr::transmute(dplyr::filter(allfiles(), 
                              grepl("s_corney/cpolar", file)),
                fullname = file.path(root, file), 
                date = as.POSIXct(strptime(sprintf("200%i-01", as.integer(substr(basename(fullname), 12, 14))+800), "%Y%m-%d"), tz = "GMT"))
}


cpolarfiles_old <- function(...) {
  dplyr::mutate(dplyr::filter(raadtools::allfiles(), 
                              grepl("s_corney/cpolar", fullname)), 
                date = as.POSIXct(strptime(sprintf("197%s-01", substr(basename(fullname), 12, 14)), "%Y%m-%d")) + 29 * 365.25 * 24 * 3600)
}
