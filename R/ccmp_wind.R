#' Title
#'
#' @inheritParams readsst
#' @return data frame of file paths
#' @export 
#'
#' @examples
#' ccmp_files()
ccmp_files <- function(time.resolution = "6hourly", ...) {
  files <- raadfiles::ccmp_6hourly_files()
  ## there are 6hourly for every time step
  files <- dplyr::slice(files, rep(seq_len(nrow(files)), each = 4L))
  files[["date"]] <- files[["date"]] + rep(c(0, 6, 12, 15), length.out = nrow(files)) * 3600
  files[["band"]] <- rep(1:4, length.out = nrow(files))
  files
}


