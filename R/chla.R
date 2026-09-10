#' Read Chlorophyll-a, NASA algorithm
#'
#' Ocean colour Chlorophyll-a from the NASA L3m (mapped) products, across the
#' SeaWiFS, MODISA and VIIRS eras. This is the legacy front door onto
#' \code{\link{read_oc_chl_daily}}; use that directly, or
#' \code{\link{read_oc_chl_8day}} and \code{\link{read_oc_chl_monthly}}, to
#' pin a particular temporal resolution.
#'
#' One layer is returned per date. Earlier versions of this function averaged
#' every requested date down into a single layer; to get that, average the
#' result yourself with \code{terra::mean()} or \code{terra::app()}.
#'
#' @param date date or dates of data to read
#' @param product sensor to read from, or "any" to draw on all three eras
#' @param xylim spatial extents to crop from source data, anything acceptable
#' to \code{\link[terra]{ext}}, ignored if grid is provided
#' @param algorithm nasa only
#' @param grid template \code{SpatRaster} to resample the output onto
#' @param latest if TRUE (and date not supplied) return the latest time available, otherwise the earliest
#' @param returnfiles ignore options and just return the file names and dates
#' @param inputfiles input the files data base to speed up initialization
#' @param ... currently ignored
#' @export
#' @return \code{SpatRaster}
#' @seealso \code{\link{read_oc_chl_daily}} for the reader this calls,
#' \code{\link{ocfiles}} for the repository of data files
#' @examples
#' \dontrun{
#' d <- readchla(c("2003-01-01", "2003-06-01"),
#'          xylim = terra::ext(100, 150, -70, -30))
#' }
readchla <- function(date, product = c("any", "MODISA", "SeaWiFS", "VIIRS"),
                     xylim = NULL,
                     algorithm = c("nasa"),
                     latest = TRUE,
                     grid = NULL, ..., returnfiles = FALSE, inputfiles = NULL) {

  if (!algorithm == "nasa") warning("only 'nasa' algorithm is currently supported")
  product <- match.arg(product)

  ## "any" leaves the file catalogue to read_oc_chl_daily(), which stitches
  ## the three sensor eras together
  if (is.null(inputfiles) && product != "any") {
    inputfiles <- ocfiles("daily", product = product, varname = "CHL", type = "L3m")
  }

  .shim_notice("readchla", "read_oc_chl_daily")

  out <- read_oc_chl_daily(
    date = date,
    xylim = if (is.null(grid)) xylim else NULL,
    latest = latest,
    returnfiles = returnfiles,
    inputfiles = inputfiles
  )

  if (returnfiles) return(out)

  if (!is.null(grid)) out <- terra::resample(out, grid)

  out
}

#' @export
readchla_old <- function(date, time.resolution = c("weekly", "monthly"),
                         product = c("johnson", "oceancolor"),
                         platform = c("MODISA", "SeaWiFS"),
                         xylim = NULL,

                         ##lon180 = TRUE,
                         returnfiles = FALSE,
                         latest = FALSE,
                         verbose = TRUE,
                         ...) {

  ## Note, fixing this can used the old updatechlafiles below
  warning("readchla only works with a static collection of data, no longer updated")
  time.resolution <- match.arg(time.resolution)
  product <- match.arg(product)
  files <- chlafiles(time.resolution = time.resolution, product = product)
  if (returnfiles) return(files)

  if (missing(date)) date <- min(files$date)
  if (latest) date <- max(files$date)
  date <- timedateFrom(date)
  ## from this point one, we don't care about the input "date" - this is our index into all files and that's what we use
  ##findex <- .processDates(date, files$date, time.resolution)
  files <- .processFiles(date, files, time.resolution)



  ## process xylim
  cropit <- FALSE
  if (!is.null(xylim)) {
    cropit <- TRUE
    cropext <- .as_ext(xylim)
  }

  nfiles <- nrow(files)
  r <- vector("list", nfiles)

  for (ifile in seq_len(nfiles)) {
    r0 <- .rast_nc(files$fullname[ifile])
    if (product == "oceancolor") r0 <- r0[[files$band[ifile]]]
    if (cropit) r0 <- crop_if_needed(r0, cropext)
    r[[ifile]] <- r0
  }

  r <- if (nfiles > 1) terra::rast(r) else r[[1L]]
  names(r) <- basename(files$fullname)
  if (!nzchar(terra::crs(r))) terra::crs(r) <- "EPSG:4326"

  .utctime(r) <- files$date
  r
}

#' Chlorophyll-a for the Southern Ocean
##'
#' This function generates a list of available chlorophyll-a files, including SeaWiFS and MODIS.
#' @title Chlorophyll-a
#' @param time.resolution weekly (8day) or monthly
#' @param product choice of chla product, see \code{readchla}
#' @param platform one of "MODISA" or "SeaWiFS"
#' @param ... reserved for future use, currently ignored
#' @return data.frame
#' @export
chlafiles <- function(time.resolution = c("weekly", "monthly"),
                      product = c("johnson", "oceancolor"),
                      platform = c("MODISA", "SeaWiFS"), ...) {

  product <- match.arg(product)
  platform <- match.arg(platform)
  time.resolution <- match.arg(time.resolution)
  time.resolution <- c(weekly = "8d", monthly = "monthly")[time.resolution]
  ftx <- .allfilelist()
  cfiles <- grep("data_local/chl", ftx, value = TRUE)
  cfiles1 <- grep(product, cfiles, value = TRUE)
  cfiles2 <- grep(tolower(gsub("A$", "", platform)), cfiles1, value = TRUE )
  cfiles3 <- grep(time.resolution, cfiles2, value = TRUE)
  cfiles3 <- grep("nc$", cfiles3, value = TRUE)
  dates <- timedateFrom(strptime(substr(basename(cfiles3), 2, 8), "%Y%j"))
  if (product == "oceancolor") {
    return(cfiles3)
  }
  chlf <- tibble::tibble(fullname= cfiles3, date = dates)[order(dates), ]
  chlf
}
