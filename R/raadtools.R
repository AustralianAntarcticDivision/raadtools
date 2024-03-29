#' R tools for spatial data, extensions using raster to read and extract
#'
#' Tools in R for reading, plotting and manipulating spatial data, originally 
#' used at the Australian Antarctic Division (AAD).
#' @author Michael D. Sumner \email{michael.sumner@@aad.gov.au}
#'
#' Maintainer: Michael D. Sumner \email{michael.sumner@@aad.gov.au}
#' @name raadtools-package
#' @docType package
#' @keywords package
#' @import methods
#' @importFrom raster overlay
#' @importFrom sp as.image.SpatialGridDataFrame bbox CRS GridTopology  proj4string<- SpatialPoints SpatialPointsDataFrame spChFIDs spTransform 
#' @importFrom raster brick crop deratify extent<- extract getZ nlayers projection projection<- raster res resample rotate setZ stack writeRaster xmax xmin ymax ymin
#' @details
#'    read functions like \code{\link{readsst}} will read a data set by date-time vector, with a 
#'  set of shared arguments that work the same and documented against this dummy function. 
NULL

#' raadtools
#' 
#' Dummy function
#' @param date date or dates of data to read, 
#' @param time.resolution time resoution data to read, daily or monthly
#' @param xylim spatial extents to crop from source data, can be anything accepted by \code{\link[raster]{extent}}, see Details
#' @param lon180 defaults to TRUE, to "rotate" Pacific view [0, 360] data to Atlantic view [-180, 180]
#' @param setNA mask out land values (only applies to monthly time.resolution)
#' @param latest if TRUE (and date not supplied) return the latest time available
#' @param returnfiles ignore options and just return the file names and dates
#' @param ... passed in to brick, primarily for \code{filename}
#' @param inputfiles input the files data base to speed up initialization
raadtools <- function(date, time.resolution = "daily", lon180 = TRUE, setNA = TRUE, latest = FALSE, returnfiles = FALSE, ...) {
  stop("I don't do anything")
}





#' This is a list of often used projections, in PROJ
#'
#' Each element can be looked up by name, see Examples
#' @name commonprojections
#' @docType data
#' @references \url{http://www.spatialreference.org}
#' @section Warning:
#' This should be use only for a convenient reference to look up the projection strings commonly in use. There's
#' no guarantee that this would be appropriate and you should seek cartographic expertise.
#' @seealso \code{\link[raster]{projection}}, \code{\link[sp]{CRS}}, \code{\link[sp]{proj4string}}
#' @keywords data
#' @examples
#' names(commonprojections)
#' commonprojections[["polar"]]
commonprojections <- list(longlat = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0",
                          polar = "+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0",
                          laea = "+proj=laea +lat_0=-90 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0",
                          merc = "+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")



#' GRIB format metadata from the Antarctic Mesoscale Prediction System (AMPS) files. 
#' @name amps_metadata
#' @docType data
#' @title AMPS GRIB file metadata
#' @format \code{amps_metadata} A data frame with 8 columns.  The columns
#' represent
#' \tabular{rl}{
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

#' Stations locations
#' 
#' In polar stereographic, with longitude and latitude and name as attributes. 
#' 
#' Obtained with antanym
#' @docType data
#' @name stations
#' @examples 
#' plot(readice())
#' plot(stations, add = TRUE)
NULL

#' Voyage track data from the Aurora Australis
#'
#' This is a sample of the "Aurora Australis Voyage 3 2012/13 Track and Underway Data".
#' @name aurora
#' @docType data
#' @title Aurora Australis voyage track
#' @format \code{aurora} A data frame with 3 columns.  The columns
#' represent
#' \tabular{rl}{
#' \code{LONGITUDE_DEGEAST} \tab Longitude values \cr
#' \code{LATITUDE_DEGNORTH} \tab Latitude values \cr
#' \code{DATE_TIME_UTC} \tab Date-time values (POSIXct) \cr
#' }
#' @references
#' \url{http://gcmd.nasa.gov/KeywordSearch/Metadata.do?Portal=amd_au&MetadataView=Full&MetadataType=0&KeywordPath=&OrigMetadataNode=AADC&EntryId=201213030}
#' @examples
#' \dontrun{
#' ## These data were obtained like this
#' base <- "http://gcmd.gsfc.nasa.gov/KeywordSearch/RedirectAction.do?target"
#' b1 <-   "=F4t70bSf87FLsT1TNxR9TSPS74xbHAdheLQcH5Z5PMmgzJ9t%2Bi%2FEs1e8Fl61"
#' b2 <-   "MPhKjo9qxb2f9wyA%0D%0AoE1kjJ8AMcpFlMMRH7Z6umgNLsGMnWPeQdU7mZHMp%2"
#' b3 <-   "FtqMpahIrde%2F%2B9%2FZWAkIFrh2bhIiNfl4I9J%0D%0A5KBX9g5Wf7I9JdOgqY"
#' b4 <-   "bDdpj0iM1K%2BA%3D%3D"
#' aurora2013 <- read.csv(paste(base, b1, b2, b3, b4, collapse = ""), stringsAsFactors = FALSE)
#' aurora2013$DATE_TIME_UTC <- as.POSIXct(aurora2013$DATE_TIME_UTC, tz = "GMT")
#' ## get a daily sample
#' aurora <- aurora2013[,c("LONGITUDE_DEGEAST", "LATITUDE_DEGNORTH", "DATE_TIME_UTC")]
#' aurora <- aurora[!duplicated(format( aurora$DATE_TIME_UTC, "%Y-%j")), ]
#' aurora <- aurora[order(aurora$DATE_TIME_UTC), ]
#' save(aurora, file = "aurora.rda")
#' }
#' @keywords data
NULL


#' Voyage track data from the Nuyina
#'
#' This is a sample of the Nuyina underway in 2023. 
#' 
#' @name nuyina
#' @docType data
#' @title Nuyina underway track
#' @format \code{aurora} A data frame with 9 columns.  The columns represent 
#' longitude latitude date_time_utc air_pressure port_longwave_irradiance port_air_temperature precipitation_rate sea_water_salinity sea_water_temperature
#' @references
#' see data-raw for extraction
#' @examples
#' range(nuyina$date_time_utc)
#' @keywords data
NULL

