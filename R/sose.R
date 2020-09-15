#' SOSE varnames
#'
#' Available variable names from SOSE.
#'
#' These varnames can be used in `read_sose(varname = )`.
#' @return chacter vector
#' @export
#'
#' @examples
#' sose_monthly_varnames()
sose_monthly_varnames <- function(iteration = "")  {
  files <- raadfiles::sose_monthly_files(varname = "all", iteration = iteration)
  sort(unique(gsub("\\.nc$", "", unlist(lapply(strsplit(basename(files$fullname), "_"), tail, 1)))))
}


#' Read SOSE Southern Ocean State Estimate
#'
#' Model data read from files managed by
#' \code{\link[raadfiles]{sose_monthly_files}}. Dates are matched to file names by finding
#' the nearest match in time within a short duration. If \code{date}
#' is greater than length 1 then the sorted set of unique matches is
#' returned.
#'
#' This function doesn't support xylim, lon180, or more than one date being read.
#' @param varname variable to return from the data files, default is whatever first value
#' from [sose_monthly_varnames()]
#' @param level, defaults to 1L - there are 52
#' @export
#' @return \code{\link[raster]{raster}} object
#' @examples
#' \dontrun{
#' read_sose(varname = "Uvel", level = 1)
#' read_sose(varname = "SeaIceArea", level = 1, latest = FALSE)
#' read_sose(varname = "Chl", level = 10, latest = FALSE)
#'
#' }
read_sose <-  function (date, time.resolution = c("monthly"),
                      varname = "", level = 1L,
                      setNA = TRUE,
                      latest = TRUE,
                      returnfiles = FALSE,  ..., inputfiles = NULL) {
  time.resolution <- match.arg(time.resolution)
  if (is.null(inputfiles)) {
    files <- raadfiles::sose_monthly_files(varname = varname)
  } else {
    files <- inputfiles
  }
  if (returnfiles)
    return(files)
  if (missing(date)) date <- if (latest) max(files$date) else min(files$date)

  if (length(date) > 1) {
    message("only one date may be read at a time")
    date <- date[1L]
  }
  date <- timedateFrom(date)
  files <- .processFiles(date, files, time.resolution)
  nfiles <- nrow(files)
  if (nfiles > 1) files <- files[1L, ]
  xfile <- files$fullname[1L]
  band <- files$band[1L]
  if (length(level) > 1) {
   r0 <- brick(xfile, band = band, lvar = 4, stopIfNotEqualSpaced = FALSE)
   r0 <- subset(r0, level)

   r0 <- setZ(r0, level)
  } else {
    r0 <- raster(xfile, band = band, level = level, stopIfNotEqualSpaced = FALSE)
    r0 <- setZ(r0, files$date)
  }
  extent(r0) <- extent(c(0, 2 * 20037508 , -14317550, -3445832))
  merc <- "+proj=merc +datum=WGS84"
  projection(r0) <- merc


  r0

}
