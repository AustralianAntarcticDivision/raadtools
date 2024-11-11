.multi_era_chlafiles <- function() {
  f1 <- try(ocfiles("daily", product = "MODISA", type = "L3m"), silent = TRUE)
  f2 <- try(ocfiles("daily", product = "VIIRS", type = "L3m"), silent = TRUE)
  f3 <- try(ocfiles("daily", product = "SeaWiFS", type = "L3m"), silent = TRUE)
  files <- NULL
  if (!inherits(f1, "try-error")) files <- rbind(files, f1)
  if (!inherits(f2, "try-error")) files <- rbind(files, f2)
  if (!inherits(f3, "try-error")) files <- rbind(files, f3)
  if (is.null(files)) stop("no ocfiles found!")
  
  dplyr::arrange(dplyr::distinct(files, date, .keep_all = TRUE), date)
  }


#' Read daily chlorphyll-a
#' 
#' Data is read as daily files in a way consistent with other read functions that can 
#' be used by `extract()`. (`readchla` for example cannot be used this way as it returns a mean value for the period asked for). 
#' 
#' This function will read from the entire era available to SeaWiFS, MODISA, and VIIRS. 
#' 
#' To read from particular eras use the output of `ocfiles()` for inputfiles  argument. 
#' 
#' @inheritParams readsst
#' 
#' @return
#' @export
#'
#' @examples
#' read_chla_daily(latest = FALSE) ## we should see SeaWiFS
#' read_chla_daily() ## we should see MODISA (or VIIRS)
read_chla_daily <-  function (date, time.resolution = c("daily"),
                      xylim = NULL, lon180 = TRUE,
                      varname = c("chlor_a"),
                      setNA = TRUE,
                      latest = TRUE,
                      returnfiles = FALSE,  ..., inputfiles = NULL) {
  time.resolution <- match.arg(time.resolution)

  #varname <- match.arg(varname)
 if (is.null(inputfiles)) {
    files <- .multi_era_chlafiles()
  } else {
    files <- inputfiles
  }
  if (returnfiles)
    return(files)
  if (missing(date)) date <- if (latest) max(files$date) else min(files$date)
  
  
  
  
  files <- .processFiles(date, files, time.resolution)
  nfiles <- nrow(files)
  read_fun <- function(xfile, ext, rot, varname = "", band = 1) {
    r <- terra::rast(xfile, varname)
    terra::ext(r) <- terra::ext(-180, 180, -90, 90)  ## slight noise in the source
    if(!is.null(rot) && rot) r <- terra::rotate(r, left = FALSE)
    if(!is.null(ext)) r <- terra::crop(r, terra::ext(ext))
    raster::raster(r)
  }
  
  ## TODO determine if we need to rotate, or just shift, or not anything
  rot <- !lon180
  msk <- NULL

  
  if (!"band" %in% names(files)) files$band <- 1
  

  r0 <- brick(stack(lapply(seq_len(nrow(files)), function(xi) read_fun(files$fullname[xi], ext = xylim,  rot = rot, varname = varname, band = files$band[xi]))),
              ...)
  if (is.na(projection(r0))) projection(r0) <- "+proj=longlat +datum=WGS84"
  
  if (nfiles == 1) r0 <- r0[[1L]]
  r0 <- setZ(r0, files$date)
  
  r0
  
}


#' @name readchla
#' @export
#' @examples 
#' readCHL_month()
readCHL_month <- function(date, xylim = NULL, returnfiles = FALSE, ..., inputfiles = NULL, latest = TRUE) {
  if (is.null(inputfiles)) {
    ## memoize this call
    files <- ocfiles("monthly", product = "MODISA", varname = "CHL", type = "L3m")
  } else {
    files <- inputfiles
  }
  if (returnfiles) return(files)
  if (missing(date)) {
    if (latest)  date <- max(files$date) else date <- min(files$date)
  }
  date <- timedateFrom(date)
  files <- .processFiles(date, files, "monthly")
  rl <- lapply(files$fullname, raster::raster, varname = "chlor_a")
  if (!is.null(xylim)) rl <- lapply(rl, raster::crop, raster::extent(xylim))
  raster::setZ(raster::brick(rl), date)
}



#Default is to read from the Johnson Improved
# chlorophyll-a estimates using Southern Ocean-specific calibration
# algorithms, but the original MODIS and SeaWIFs products are also available via the argument \code{product}.
#
# Dates are matched to file names by finding the nearest match in
# time within a short duration. If \code{date} is greater than
# length 1 then the sorted set of unique matches is returned.
##
# The code that creates these derived files is at [raad-deriv](https://github.com/AustralianAntarcticDivision/raad-deriv).
# 
# 

# @references  Johnson, R, PG Strutton, SW Wright, A McMinn, and KM
# Meiners (2013) Three improved satellite chlorophyll algorithms for
# the Southern Ocean, J. Geophys. Res. Oceans, 118,
# doi:10.1002/jgrc.20270
# \url{http://onlinelibrary.wiley.com/doi/10.1002/jgrc.20270/full}
#

#' Read Chlorophyll-a, NASA algorithm
##'
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
#' Note that reaCHL_month reads the NASA algorithm L3m products. 
#' 
#' Note that this function cannot be used with 'extract()', for that use `read_chla_daily()`. 
#' 
#' 
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
      template <- crop(template, extent(xylim))
  }
  ex <- c(xmin(template), xmax(template), ymin(template), ymax(template))
  wdata <- vapour::gdal_raster_data(.vrt_ds0( files$fullname, "chlor_a"), target_ext = ex, target_res = res(template), resample = "average")
  
  out <- setValues(template, wdata[[1]])
  return(setZ(out, date[1]))
  # d <- readchla_mean(date, product = product, xylim = xylim, latest = latest)
  # if (nrow(d) < 1) {
  #   warning("no data available in Southern Ocean for these date/s")
  #  return(NULL)
  # }
  # algorithm <- match.arg(algorithm)
  # thename <- sprintf("chla_%s", algorithm)
  # d <- d[, c("bin_num", thename)]
  # names(d) <- c("bin_num", "value")
  # NROWS <- product2nrows(product)
  # gridmap <- raster::raster(raster::extent(-180, 180, -90, 90), ncol = NROWS * 2, nrow = NROWS, crs = "+proj=longlat +datum=WGS84 +no_defs")
  # ## this hack is to align with assumption from here
  # ## https://github.com/AustralianAntarcticDivision/ocean_colour/blob/master/seawifs_daily_bins_init.R#L37
  # gridmap <- raster::crop(gridmap, raster::extent(-180, 180, -90, -30), snap = "out")
  # if (!is.null(xylim)) gridmap <- crop(gridmap, xylim)
  # if (!is.null(grid)) gridmap <- grid
  # bin_chl(d$bin_num, d$value, NROWS, gridmap)

}

bin_chl <- function(bins, value, NROWS, gridmap) {
  bins <- tibble(bin_num = bins, value = value)
  ll <- coordinates(gridmap)
  ## removed dep on sosoc/croc 2018-0919
  bins <- tibble(bin_num = .lonlat2_bin(ll[,1], ll[, 2], NUMROWS = NROWS),
                 gridcell = seq_len(ncell(gridmap))) %>%
    dplyr::inner_join(bins, "bin_num")
  gridmap[bins[["gridcell"]]] <- bins[["value"]]
  gridmap
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



  rtemplate <- if (product == "oceancolor") raster(files$fullname[1L], band = files$band[1L]) else raster(files$fullname[1L])
  ##if (lon180) rtemplate <- .rotate(rtemplate)

  ## process xylim
  cropit <- FALSE
  if (!is.null(xylim)) {
    cropit <- TRUE
    cropext <- extent(xylim)
    ##rtemplate <- crop(rtemplate, cropext)
  }

  nfiles <- nrow(files)
  r <- vector("list", nfiles)

  for (ifile in seq_len(nfiles)) {
    r0 <- if (product == "oceancolor") raster(files$fullname[ifile], band = files$band[ifile]) else raster(files$fullname[ifile])
    ##if (lon180) r0 <- .rotate(r0)
    if(cropit) r0 <- crop(r0, cropext)
    ## r0[r0 < -2] <- NA
    r[[ifile]] <- r0
    ##if (verbose & ifile %% 10L == 0L) .progressreport(ifile, nfiles)
  }

  if (nfiles > 1)
    r <- brick(stack(r), ...)
  else r <- r[[1L]]
  names(r) <- basename(files$fullname)
  if (is.na(projection(r))) projection(r) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"

  r <- setZ(r, files$date)
  return(r)
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
