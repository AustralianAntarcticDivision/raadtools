

##' NCEP2 wind files
##'
##' Files containing NCEP2 wind vector data
##' @title Files containing NCEP2 wind vector data
##' @param data.source ignored, reserved for future use
##' @param time.resolution  time resolution data to read, 6hourly only for now
##' @param ... reserved for future use, currently ignored
##' @return \code{data.frame} of file names and dates
##' @export
windfiles <-
  function(data.source = "", time.resolution = c("6hourly"),  ...) {
    time.resolution <- match.arg(time.resolution)
    wf <- dplyr::rename(raadfiles::ncep2_uwnd_6hr_files(), ufullname = fullname)
    lens <- purrr::map_dbl(wf$ufullname, ~ncmeta::nc_dims(.x, "uwnd")$length[4])
    times0 <- purrr::map(lens, ~seq(0, by = 6 * 3600, length.out = .x))
    wfU <- dplyr::slice(wf, rep(dplyr::row_number(), lens))
    wfU <- dplyr::group_by(wfU, .data$date)
    wfU <- dplyr::mutate(wfU, band = dplyr::row_number())
    wfU <- dplyr::ungroup(wfU)
    wfU$date <- wfU$date + unlist(times0)

    
     wf <- tibble::tibble(date = wfU$date, ufullname = wfU$ufullname, 
                      vfullname = gsub("uwnd", "vwnd", wfU$ufullname), 
                      band = wfU$band, 
                      root = wfU$root)
     
wf 
  }


