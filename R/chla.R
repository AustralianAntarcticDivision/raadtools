#' Read Chlorophyll-a, NASA algorithm
#' 
#' Ocean colour Chlorophyll-a data, provide an input of daily dates and these will be averaged into one layer. 
#' @param date date or dates of data to read, see Details
#' @param product choice of product, see Details

#' @param xylim spatial extents to crop from source data, can be anything accepted by \code{\link[raster]{extent}}, ignored if grid is provided
#' @param algorithm nasa only
#' @param grid template raster object for output
#' @param latest if TRUE (and date not supplied) return the latest time available, otherwise the earliest
#' @param returnfiles 	ignore options and just return the file names and dates
#' @param inputfiles 	input the files data base to speed up initialization 
#' @param ... currently ignored
#' @seealso readCHL_month
#' @export
#' @return \code{\link[raster]{raster}} object
#' @seealso \code{\link{chlafiles}} for details on the repository of
#' data files, \code{\link[raster]{raster}} for the return value
#' @examples
#' \dontrun{
#' d <- readchla(c("2003-01-01", c("2003-06-01")),
#'          xylim = extent(100, 150, -70, -30))
#' }
#' @export
readchla <- function(date, product = c("any", "MODISA", "SeaWiFS", "VIIRS"),
                     xylim = NULL,
                     algorithm = c("nasa"),

                     latest = TRUE, 
                     grid = NULL, ..., returnfiles = FALSE, inputfiles = NULL) {
  
  if (!algorithm == "nasa") warning("only 'nasa' algorithm is currently supported")
  product <- match.arg(product)
  
  if (is.null(inputfiles)) {
    files <- ocfiles("daily", product = product, varname = "CHL", type = "L3m")
  } else {
    files <- inputfiles
  }
  if (returnfiles) return(files)

  
  if (missing(date)) date <- if (latest) max(files$date) else min(files$date)
  date <- timedateFrom(date)
  
  files <- files[match(as.Date(date), as.Date(files$date)), ]
  files <- dplyr::arrange(dplyr::distinct(files), date)
  if (nrow(files) < 1) {
    warning("no data available in Southern Ocean for these date/s")
    return(NULL)
  }
  template <- raster(files$fullname[1], varname = "chlor_a")  
  if (!is.null(xylim)) {
      template <- raster::crop(template, extent(xylim))
  }
  ex <- c(raster::xmin(template), raster::xmax(template), raster::ymin(template), raster::ymax(template))
  wdata <- vapour::gdal_raster_data(.vrt_ds0( files$fullname, "chlor_a"), target_ext = ex, target_res = res(template), resample = "average")
  
  out <- raster::setValues(template, wdata[[1]])
  return(setZ(out, date[1]))

}

#' @importFrom dplyr .data
#' @export
readchla_mean <- function(date,
#                     algorithm = c("johnson", "oceancolor"),
                     product = c("MODISA", "SeaWiFS"),
                     xylim = NULL,
                     returnfiles = FALSE,
                     latest = TRUE,
                     verbose = TRUE,
                     ...) {
  largs <- list(...)
  if ("time.resolution" %in% names(largs)) stop("time.resolution is not supported, enter the dates directly - underlying temporal resolution is daily")

  files <- oc_sochla_files(product = product)

  if (missing(date)) {
      date <- if (latest) max(files$date) else min(files$date)
  }

  ## here read_oc_sochla should take the bins it needs
  ## removed dep on sosoc/croc 2018-09-19
  init <- .init_bin(product2nrows(product))
  if (is.null(xylim)) {
    bin_sub <- NULL
  } else {
    ## removed dep on sosoc/croc 2018-09-19
    bin_sub <- tibble::tibble(bin_num = .crop_init(init, xylim))
  }
 # bins <- purrr::map_df(date, read_oc_sochla, bins = bin_sub, product = product) %>%

   bins <- read_oc_sochla(date, bins = bin_sub, product = product, inputfiles = files) %>%
    dplyr::select(-"date") %>%
    dplyr::group_by_at("bin_num") %>%
    dplyr::summarize_all(mean)

  bins

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

  terra::time(r) <- as.Date(files$date)
  r
}

#' Chlorophyll-a for the Southern Ocean
##'
#' This function generates a list of available chlorophyll-a files, including SeaWiFS and MODIS.
#' @title Chlorophyll-a
#' @param time.resolution weekly (8day) or monthly
#' @param product choice of chla product, see \code{readchla}
#' @param platform (modis[a] or seawifs)
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
    xfs <- .expandFileDateList(cfiles3)
    # nc <- ncdf4::nc_open(cfiles3)
    #  dates <-
    # dates <- timedateFrom(strptime(substr(basename(cfiles3), 2, 8), "%Y%j"))
    dates <- xfs$date
    cfiles3 <- xfs$fullname
  }
  chlf <- tibble::tibble(fullname= cfiles3, date = dates)[order(dates), ]
  chlf
}
