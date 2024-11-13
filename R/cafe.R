

#' Read 'cafe' MODIS monthly data
#'
#' I don't remember what these are but they were at https://orca.science.oregonstate.edu/
#' 
#' Files are found with `raadfiles::cafe_monthly_files()`.
#' 
#' @inheritParams raadtools
#' @inheritDotParams raadtools
#' @name readcafe
#' @export
#' @examples
#' readcafe(date = "2010-01-15")
readcafe <-  function (date, time.resolution = c("monthly"),
                      xylim = NULL, lon180 = TRUE,
                      setNA = TRUE,
                      latest = TRUE,
                      returnfiles = FALSE,  ..., inputfiles = NULL) {
  time.resolution <- match.arg(time.resolution)

  if (is.null(inputfiles)) {
    files <- raadfiles::cafe_monthly_files()
  }
  if (returnfiles) {
    return(files)
  }
  if (missing(date)) date <- if (latest) max(files$date) else min(files$date)

  date <- timedateFrom(date)
  files <- .processFiles(date, files, time.resolution)
  nfiles <- nrow(files)
  ## progress
  pb <- progress::progress_bar$new(
    format = "  extracting [:bar] :percent in :elapsed",
    total = nfiles, clear = FALSE, width= 60)
  pb$tick(0)
  read_fun <- function(xfile, ext, msk, rot, varname = "", band = 1) {
    pb$tick()
    mask_if_needed(crop_if_needed(rotate_if_needed(raster(xfile), rot), ext), msk)
  }

  ## TODO determine if we need to rotate, or just shift, or not anything
  rot <- !lon180
  msk <- NULL


  if (!"band" %in% names(files)) files$band <- 1


  r0 <- brick(stack(lapply(seq_len(nrow(files)), function(xi) read_fun(files$fullname[xi], ext = xylim, msk = msk, rot = rot,  band = files$band[xi]))),
              ...)

  if (is.na(projection(r0))) projection(r0) <- "+proj=longlat +datum=WGS84"
  r0 <- raster::setExtent(r0, raster::extent(-180, 180, -90, 90))
  if (nfiles == 1) r0 <- r0[[1L]]
  r0 <- setZ(r0, files$date)
  if (setNA) {
    r0[r0 < 0] <- NA_real_
  }
  r0

}
