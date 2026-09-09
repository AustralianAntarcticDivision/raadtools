

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
  .shim_notice("readcafe")
  if (missing(date)) date <- if (latest) max(files$date) else min(files$date)

  date <- timedateFrom(date)
  files <- .processFiles(date, files, time.resolution)
  nfiles <- nrow(files)
  ## progress
  pb <- progress::progress_bar$new(
    format = "  extracting [:bar] :percent in :elapsed",
    total = nfiles, clear = FALSE, width= 60)
  pb$tick(0)
  ## TODO determine if we need to rotate, or just shift, or not anything
  rot <- !lon180
  msk <- NULL


  if (!"band" %in% names(files)) files$band <- 1


  r0 <- terra::rast(lapply(seq_len(nrow(files)), function(xi) {
    pb$tick()
    read_one(files$fullname[xi], ext = xylim, msk = msk, rot = rot,
             band = files$band[xi])
  }))

  if (!nzchar(terra::crs(r0))) terra::crs(r0) <- "EPSG:4326"
  terra::ext(r0) <- terra::ext(-180, 180, -90, 90)
  if (nfiles == 1) r0 <- r0[[1L]]
  terra::time(r0) <- files$date
  names(r0) <- format(files$date, "%Y-%m-%d")
  if (setNA) {
    r0[r0 < 0] <- NA_real_
  }
  .write_if_filename(r0, ...)

}
