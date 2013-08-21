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



##' Read NSIDC sea ice data from daily or monthly files
##'
##' Sea ice data is read from files managed by \code{\link{icefiles}}
##'
##' @param date date or date range of data to read, will find something within a short window
##' @param time.resolution time resoution data to read, daily or monthly
##' @param zeroNA mask zero values as NA
##' @param rescale rescale values from integer range?
##' @param ... reserved for future use, currently ignored
##' @export
##' @return \code{\link{raster}} object
##' @seealso \code{\link{icefiles}} for details on the repository of data files, \code{\link{raster}} for the return value
readice <- function(date = as.Date("1978-11-01"),
                    time.resolution = "daily",
                    zeroNA = TRUE, rescale = TRUE, ...) {
    datadir = getOption("default.datadir")
    date <- timedateFrom(date)
    if (all(is.na(date))) stop("no input dates are valid")
    if (length(date) > 1L) {
      date <- date[!is.na(date)][1]
      warning("date input is longer than 1, returning only first valid date")
    }
    stersouth <-  "+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
    dims <- c(316, 332)
    icyf <- icefiles(time.resolution = time.resolution)

    windex <- which.min(abs(date - icyf$date)) ##findInterval(date, icyf$date)

    dtime <- abs(difftime(date, icyf$date[windex], units = c("days")))
    if (time.resolution == "daily") {
        if (dtime > 1.5) stop(sprintf("no ice data file within 1.5 days of %s", format(date)))
    }
     if (time.resolution == "monthly") {
        if (dtime > 15) stop(sprintf("no ice data file within 15 days of %s", format(date)))
    }


    con <- file(file.path(datadir, icyf$file[windex]), open = "rb")
    trash <- readBin(con, "integer", size = 1, n = 300)
    dat <- readBin(con, "integer", size = 1, n = prod(dims), endian = "little", signed = FALSE)
    close(con)

    r100 <- dat > 250
    r0 <- dat < 1
    if (rescale) {
        dat <- dat/2.5  ## rescale back to 100
    }
     if (zeroNA) {
        dat[r100] <- NA
        dat[r0] <- NA
    }
    r <- raster(t(matrix(dat, dims[1])), template = raster(GridTopology(c(-3937500, -3937500), c(25000, 25000), dims)))
    projection(r) <- stersouth
    names(r) <- icyf$file[windex]
    r <- setZ(r, icyf$date[windex])
    r
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





