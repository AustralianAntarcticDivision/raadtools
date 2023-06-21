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
  files <- dplyr::filter(allfiles(), 
                stringr::str_detect(file, "s_corney"))
files <- dplyr::filter(files, stringr::str_detect(file, "cpolar.*nc$"))
  
  files <- dplyr::transmute(files,
                fullname = file.path(root, file), 
                date = as.POSIXct(strptime(sprintf("200%i-01", as.integer(substr(basename(fullname), 12, 14))+800), "%Y%m-%d"), tz = "UTC"))
  files
}


cpolarfiles_old <- function(...) {
  dplyr::mutate(dplyr::filter(raadtools::allfiles(), 
                              grepl("s_corney/cpolar", fullname)), 
                date = as.POSIXct(strptime(sprintf("197%s-01", substr(basename(fullname), 12, 14)), "%Y%m-%d")) + 29 * 365.25 * 24 * 3600)
}
