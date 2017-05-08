##' R tools for spatial data, extensions using raster to read and extract
##'
##' Tools in R for reading, plotting and manipulating spatial data, originally 
##' used at the Australian Antarctic Division (AAD).
##' @author Michael D. Sumner \email{michael.sumner@@aad.gov.au}
##'
##' Maintainer: Michael D. Sumner \email{michael.sumner@@aad.gov.au}
##'
##' @name raadtools
##' @docType package
##' @keywords package
##' @import methods
##' @importFrom raster overlay
##' @importFrom sp as.image.SpatialGridDataFrame bbox CRS GridTopology  proj4string<- SpatialPoints SpatialPointsDataFrame spChFIDs spTransform 
##' @importFrom maptools ContourLines2SLDF spRbind
##' @importFrom raster brick crop deratify extent<- extract getZ nlayers projection projection<- raster res resample rotate setZ stack writeRaster xmax xmin ymax ymin

NULL






## .readNC <- function(x, varnames) {
##         ncf <- .ncops()
##         nccon <- ncf$open(x)
##         lv <- vector("list", length(varnames))
##         names(lv) <- varnames
##         for (i in seq_along(varnames)) {
##             lv[[i]] <- ncf$getvar(nccon, varnames[i])
##         }
##         ncf$close(nccon)
##         lv
##     }

##. missingNC <- function(x, varname, att) {
##     ncf <- .ncops()
##     nccon <- ncf$open(x)
##     val <- ncf$getatt(nccon, varname, att)$value
##     ncf$close(nccon)
##     val
## }

## .readAVISO <- function(x, justone = TRUE) {
##         xtreme <- 20037508
##         ytreme <- 16925422
##         maxvalue <- 90000
##         if(justone) {
##             vs <- c("NbLongitudes", "NbLatitudes", "Grid_0001")
##             x <- .readNC(x, vs)

##             names(x) <- c("x", "y", "z")
##             x$x <- seq_along(x$x)
##             x$y <- seq_along(x$y)
##             x$z <- t(x$z)
##             x <- raster(x)
##         } else {
##             vs <- c("NbLongitudes", "NbLatitudes", "Grid_0001", "Grid_0002")
##             x <- .readNC(x, vs)

##             names(x) <- c("x", "y", "z", "z2")
##             x$x <- seq_along(x$x)
##             x$y <- seq_along(x$y)
##             x$z <- t(x$z)
##             x$z2 <- t(x$z2)
##             x <- brick(raster(x[1:3]), raster(list(x = x$x, y = x$y, z = x$z2)))
##         }

##         x[!x < maxvalue] <- NA
## ##        x <- flip(flip(t(raster(x, varname = varname)), direction = "y"), direction = "x")
##         extent(x) <- extent(0, xtreme * 2, -ytreme, ytreme)
##         projection(x) <- "+proj=merc +ellps=WGS84 +over"
##         x
##     }

## ## what NetCDF support do we have?
## .netcdfpackage <- function() {
##     warninglevel <- getOption("warn")
##     on.exit(options(warn = warninglevel))
##     options(warn = -1)
##     if (suppressPackageStartupMessages(require(ncdf4, quietly = TRUE))) return("ncdf4")
##     if (suppressPackageStartupMessages(require(ncdf, quietly = TRUE))) return("ncdf")
##     if (suppressPackageStartupMessages(require(RNetCDF, quietly = TRUE))) return("RNetCDF")
##     NA
## }

## .ncops <- function(package = .netcdfpackage()) {
##     switch(package,
##            ncdf4 = list(open = nc_open, getvar = ncvar_get, getatt = ncatt_get, close = nc_close),
##            ncdf = list(open = open.ncdf, getvar = get.var.ncdf, getatt = att.get.ncdf, close = close.ncdf),
##            RNetCDF = list(open = open.nc, getvar = var.get.nc, getatt = att.get.nc, close = close.nc))
## }



##' This is a list of often used projections, in PROJ.4
##'
##' Each element can be looked up by name, see Examples
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



#' GRIB format metadata from the Antarctic Mesoscale Prediction System (AMPS) files. 
#' @name amps_metadata
#' @docType data
#' @title AMPS GRIB file metadata
##' @format \code{amps_metadata} A data frame with 8 columns.  The columns
##' represent
##' \tabular{rl}{
#' \code{Band} \tab the band number \cr
#' \code{GRIB_COMMENT} \tab the data measured \cr
#' \code{GRIB_ELEMENT} \tab the data name \cr
#' \code{GRIB_FORECAST_SECONDS} \tab forecast seconds  \cr
#' \code{GRIB_REF_TIME} \tab text reference time \cr
#' \code{GRIB_SHORT_NAME} \tab short name  \cr
#' \code{GRIB_UNIT} \tab  text unit \cr
#' \code{GRIB_VALID_TIME} \tab text valid time \cr 
#' }
#' @keywords data
#' @examples 
#' print(amps_metadata)
#' ## u_wind_at_900 <- read  ## unfinished
NULL



##' Voyage track data from the Aurora Australis
##'
##' This is a sample of the "Aurora Australis Voyage 3 2012/13 Track and Underway Data".
##' @name aurora
##' @docType data
##' @title Aurora Australis voyage track
##' @format \code{aurora} A data frame with 3 columns.  The columns
##' represent
##' \tabular{rl}{
##' \code{LONGITUDE_DEGEAST} \tab Longitude values \cr
##' \code{LATITUDE_DEGNORTH} \tab Latitude values \cr
##' \code{DATE_TIME_UTC} \tab Date-time values (POSIXct) \cr
##' }
##' @references
##' \url{http://gcmd.nasa.gov/KeywordSearch/Metadata.do?Portal=amd_au&MetadataView=Full&MetadataType=0&KeywordPath=&OrigMetadataNode=AADC&EntryId=201213030}
##' @examples
##' \dontrun{
##' ## These data were obtained like this
##' base <- "http://gcmd.gsfc.nasa.gov/KeywordSearch/RedirectAction.do?target"
##' b1 <-   "=F4t70bSf87FLsT1TNxR9TSPS74xbHAdheLQcH5Z5PMmgzJ9t%2Bi%2FEs1e8Fl61"
##' b2 <-   "MPhKjo9qxb2f9wyA%0D%0AoE1kjJ8AMcpFlMMRH7Z6umgNLsGMnWPeQdU7mZHMp%2"
##' b3 <-   "FtqMpahIrde%2F%2B9%2FZWAkIFrh2bhIiNfl4I9J%0D%0A5KBX9g5Wf7I9JdOgqY"
##' b4 <-   "bDdpj0iM1K%2BA%3D%3D"
##' aurora2013 <- read.csv(paste(base, b1, b2, b3, b4, collapse = ""), stringsAsFactors = FALSE)
##' aurora2013$DATE_TIME_UTC <- as.POSIXct(aurora2013$DATE_TIME_UTC, tz = "GMT")
##' ## get a daily sample
##' aurora <- aurora2013[,c("LONGITUDE_DEGEAST", "LATITUDE_DEGNORTH", "DATE_TIME_UTC")]
##' aurora <- aurora[!duplicated(format( aurora$DATE_TIME_UTC, "%Y-%j")), ]
##' aurora <- aurora[order(aurora$DATE_TIME_UTC), ]
##' save(aurora, file = "aurora.rda")
##' }
##' @keywords data
NULL



