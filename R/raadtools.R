##' R tools for spatial data at the AAD
##'
##' Tools in R for reading, plotting and manipulating spatial data
##' commonly used at the Australian Antarctic Division (AAD).
##' @author Michael D. Sumner \email{michael.sumner@@aad.gov.au}
##'
##' Maintainer: Michael D. Sumner \email{michael.sumner@@aad.gov.au}
##'
##' @name raadtools
##' @docType package
##' @keywords package
NULL


.possiblepaths <- function() {
    list(default.datadir =  c("//aad.gov.au/files/AADC/Scientific_Data/Data/gridded/data",
                       "/Volumes/files/data"))
}
.trysetpath <- function() {
    possibles <- .possiblepaths()[["default.datadir"]]
    success <- FALSE
    for (i in seq_along(possibles)) {
        fi <- file.info(possibles[i])
        if (!is.na(fi$isdir) & fi$isdir) {
            options(default.datadir = possibles[i])
            success <- TRUE
        }
    }
    success
}
.onAttach <- function(libname, pkgname) {
    pathwasset <- .trysetpath()
    if (!pathwasset) {
        packageStartupMessage("Warning: could not find data repository at any of",
            paste(normalizePath(.possiblepaths()[["default.datadir"]], mustWork = FALSE), collapse = "\n"), sep = "\n\n")

        packageStartupMessage("Consider setting the option for your system\n")
        packageStartupMessage('For example: options(default.datadir = "', gsub("\\\\", "/", normalizePath("/myrepository/data", mustWork = FALSE)), '")', '\n', sep = "")

    }
}

##' @export
.currentsfiles <- function(data.dir = getOption("default.datadir"), data.source = file.path(data.dir, "current", "aviso", "upd", "7d")) {
     cfiles <- list.files(data.source, pattern = ".nc$", full.names = TRUE)
     datepart <- sapply(strsplit(basename(cfiles), "_"), function(x) x[length(x)-1])
     currentdates <- timedateFrom(as.Date(strptime(datepart, "%Y%m%d")))
     data.frame(file = cfiles, date = currentdates, stringsAsFactors = FALSE)

}


##' @importFrom raster t flip # imports should not be necessary here
##' @export
readcurr <- function(date = as.Date("1999-11-24"),
                     time.resolution = "weekly",
                     setNA = TRUE, rescale = TRUE,
                     ...) {

    data.dir = getOption("default.datadir")
    time.resolution <- match.arg(time.resolution)
    date <- timedateFrom(date)
    if (all(is.na(date))) stop("no input dates are valid")
    if (any(is.na(date))) {
      warning("not all input dates are valid")
      date <- date[!is.na(date)]
    }

    ## sort?
    ord <- order(date)
    if (any(diff(ord) < 0)) {
        warning("dates out of order and will be sorted")
        date <- date[ord]
    }

    files <- .currentsfiles(data.dir = data.dir)

     ## find indices into files that are requested
    windex <- integer(length(date))
    for (i in seq_along(date)) {
      windex[i] <- which.min(abs(date[i] - files$date))
    }
    ## check for duplicates
    dupes <- !duplicated(windex)
    if (sum(dupes) < length(windex)) warning("duplicated dates will be dropped")
    windex <- windex[dupes]
    date <- date[dupes]


    ## prevent reading more than one
    if (length(windex) > 1) {
        windex <- windex[1]
        date <- date[1]
        warning("only one time step can be read at once")
    }

    ## now check which of these have a valid file within the resolution

    dtime <- abs(difftime(date, files$date[windex], units = c("days")))

    dtimetest <- switch(time.resolution,
                        weekly = 4)
    if (all(dtime > dtimetest)) stop(sprintf("no data file within %.1f days of %s", dtimetest))
    if (any(dtime > dtimetest)) {
      warning(sprintf("%i input dates have no corresponding data file within %f days of available files", sum(dtime > dtimetest), dtimetest))
      windex <- windex[dtime <= dtimetest]
    }



    ## WGS84 Mercator X-distance of 1 hemisphere
    xtreme <- 20037508.34
    ytreme <- 16925421.91  ## Y-distance [-82,0]
    ## see http://soki.aad.gov.au/display/Data/Ocean+current+data+in+Mercator
    ## (if you compare the NbLongitudes/NbLatitudes from the files, they are out by half a pixel since the corner/centre is not explicit)


    r1 <- raster(files$file[windex], varname = "Grid_0001")
    r1 <- flip(flip(t(r1), direction = "y"), direction = "x")
    extent(r1) <- extent(0, xtreme * 2, -ytreme, ytreme)
    r2 <- raster(files$file[windex], varname = "Grid_0002")
    r2 <- flip(flip(t(r2), direction = "y"), direction = "x")
    extent(r2) <- extent(0, xtreme * 2, -ytreme, ytreme)
    r <- brick(r1, r2)
    names(r) <- c("U", "V")
    projection(r) <- "+proj=merc +ellps=WGS84 +over"
    r
}


##' Read NSIDC sea ice data from daily or monthly files
##'
##' Sea ice data is read from files managed by
##' \code{\link{icefiles}}. Dates are matched to file names by finding
##' the nearest match in time within a short duration. If \code{date}
##' is greater than length 1 then the sorted set of unique matches is
##' returned.
##' @param date date or dates of data to read, see Details
##' @param time.resolution time resoution data to read, daily or monthly
##' @param setNA mask zero and values greater than 100 as NA
##' @param rescale rescale values from integer range?
##' @param debug ignore data request and simply report on what would be returned after processing arguments
##' @param ... reserved for future use, currently ignored
##' @export
##' @return \code{\link[raster]{raster}} object
##' @seealso \code{\link{icefiles}} for details on the repository of
##' data files, \code{\link[raster]{raster}} for the return value
readice <- function(date = as.Date("1978-11-01"),
                    time.resolution = c("daily", "monthly"),
                    setNA = TRUE, rescale = TRUE,
                    debug = FALSE, ...) {
    datadir = getOption("default.datadir")
    time.resolution <- match.arg(time.resolution)
    date <- timedateFrom(date)
    if (all(is.na(date))) stop("no input dates are valid")
    if (any(is.na(date))) {
      warning("not all input dates are valid")
      date <- date[!is.na(date)]
    }

    ## sort?
    ord <- order(date)
    if (any(diff(ord) < 0)) {
        warning("dates out of order and will be sorted")
        date <- date[ord]
    }



    icyf <- icefiles(time.resolution = time.resolution)

    ## find indices into files that are requested
    windex <- integer(length(date))
    for (i in seq_along(date)) {
      windex[i] <- which.min(abs(date[i] - icyf$date)) ##findInterval(date, icyf$date)
    }

    ## check for duplicates
    dupes <- !duplicated(windex)
    if (sum(dupes) < length(windex)) warning("duplicated dates will be dropped")
    windex <- windex[dupes]
    date <- date[dupes]
    ## now check which of these have a valid file within the resolution

    dtime <- abs(difftime(date, icyf$date[windex], units = c("days")))

    dtimetest <- switch(time.resolution,
                        daily = 1.5, monthly = 15)
    if (all(dtime > dtimetest)) stop(sprintf("no ice data file within %.1f days of %s", dtimetest))
    if (any(dtime > dtimetest)) {
      warning(sprintf("%i input dates have no corresponding ice data file within %f days of available files", sum(dtime > dtimetest), dtimetest))
      windex <- windex[dtime <= dtimetest]
    }



    ## NSIDC projection and grid size for the Southern Hemisphere
    stersouth <-  "+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
    dims <- c(316L, 332L)
    rtemplate <- raster(GridTopology(c(-3937500, -3937500), c(25000, 25000), dims))
    if (length(windex) > 1L) {
      r <- brick(nrows = nrow(rtemplate), ncols = ncol(rtemplate),
                 xmn = xmin(rtemplate), xmx = xmax(rtemplate), ymn = ymin(rtemplate), ymx = ymax(rtemplate),
                 nl = length(windex))
    }
    ## loop over file indices
    for (ifile in seq_along(windex)) {
      con <- file(file.path(datadir, icyf$file[windex[ifile]]), open = "rb")
      trash <- readBin(con, "integer", size = 1, n = 300)
      dat <- readBin(con, "integer", size = 1, n = prod(dims), endian = "little", signed = FALSE)
      close(con)

      r100 <- dat > 250
      r0 <- dat < 1
      if (rescale) {
        dat <- dat/2.5  ## rescale back to 100
      }
      if (setNA) {
        dat[r100] <- NA
        dat[r0] <- NA
      }
      ##rtemp <- raster(t(matrix(dat, dims[1])), template = rtemplate)
      if (length(windex) > 1) {
          r <- setValues(r, matrix(dat, dims[1]), layer = ifile)
      } else {
          r <- raster(t(matrix(dat, dims[1])), template = rtemplate)
      }
    }


    projection(r) <- stersouth
    names(r) <- icyf$file[windex]
    r <- setZ(r, icyf$date[windex])
    r
}


##' Load \code{data.frame} of file path and dates of NSIDC sea ice concentration data.
##'
##' This function loads the latest cache of stored NSIDC files for
##' either daily or monthly data for the Southern Hemisphere,
##' processing by the SMMR/SSMI NASA Team.
##' @param time.resolution daily or monthly files?
##' @export
##' @examples
##' \dontrun{
##' icf <- icefiles(time.resolution = "monthly")
##' icf[which.min((as.Date("1995-01-01") + runif(1, -4000, 4000)) - as.Date(icf$date), ]
##' }
##' @return data.frame of \code{file} and \code{date}
icefiles <- function(time.resolution = c("daily", "monthly")) {
    time.resolution <- match.arg(time.resolution)
    files <- NULL
    load(file.path(getOption("default.datadir"), "cache", sprintf("%s_icefiles.Rdata", time.resolution)))
    files
}

##' Stable conversion to POSIXct from character and Date
##'
##' Conversion to POSIXct ensuring no local time zone applied. Currently supported is character, Date and
##' anything understood by \code{\link[base]{as.POSIXct}}.
##'
##' @param x input date-time stamp, character, Date or other supported type.
##' @param \dots ignored
##' @return the vector \code{x} converted (if necessary) to \code{POSIXct}
##' @export
timedateFrom <- function(x, ...) {
  as.POSIXct(x, tz = "GMT", ...)
}

##' This is a list of often used projections, in PROJ.4
##'
##' @details Each element can be looked up by name, see Examples
##' @name commonprojections
##' @docType data
##' @references \url{http://www.spatialreference.org}
##' @section Warning:
##' This should be use only for a convenient reference to look up the projection strings commonly in use. There's
##' no guarantee that this would be appropriate and you should seek cartographic expertise.
##' @seealso \code{\link[raster]{projection}}, \code{\link[sp]{CRS}}, \code{\link[sp]{proj4string}}
##' @keywords data
##' @examples
##' names(commonprojections)
##' commonprojections[["polar"]]
##' @export
NULL
commonprojections <- list(longlat = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0",
                          polar = "+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0",
                          laea = "+proj=laea +lat_0=-90 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0",
                          merc = "+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")





