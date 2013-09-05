##' R tools for spatial data at the AAD
##'
##' Tools in R for reading, plotting and manipulating spatial data at
##' the Australian Antarctic Division (AAD).
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

##' Extract cell values from a given data source by point coordinates and times.
##'
##' This function reads data values from a datasource, one of "oisst",
##' "aviso" and "nsidc". The \code{Query} must be a data.frame with
##' 3-columns of longitude, latitude and date/date-time.
##' @title extractxyt
##' @param datasource name of the data source to extract from
##' @param Query data.frame of 3-columns, longitude,latitude,date-time
##' @param ... arguments passed to the read functions
##' @seealso Read functions \code{\link{readsst}} ("oisst"),
##' \code{\link{readcurr}} ("aviso"), \code{\link{readice}} ("nsidc").
##' @return numeric vector, one for each row of \code{Query}
##' @export
extractxyt <- function(datasource, Query, ...) {
    ## Query MUST be a 3 column data.frame of long/lat points
    xy <- as.matrix(Query[,1:2])
    date <- timedateFrom(Query[,3])
    if (all(is.na(date))) stop("no datetimes are non-missing")
    Query <- SpatialPointsDataFrame(SpatialPoints(xy, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")), data.frame(time = date))

    ## readcurr won't work except for magonly
    ## otherwise we need to get the template and check first
    datafun <- switch(datasource,
                      oisst = readsst,
                      nsidc = readice,
                      aviso = readcurr)
    if (is.null(datafun)) stop(sprintf("%s not available", datasource))
    files <- datafun(returnfiles = TRUE)

     ## find indices into files that are requested
    windex <- integer(length(date))
    for (i in seq_along(date)) {
      windex[i] <- which.min(abs(date[i] - files$date))
    }
    dtime <- abs(difftime(date, files$date[windex], units = c("days")))

    ## THIS IS BROKEN, HOW TO DO IT?
    dtimetest <- 4
##    if (all(dtime > dtimetest)) stop(sprintf("no data file within %.1f days of %s", dtimetest))
    if (any(dtime > dtimetest)) {
      warning(sprintf("%i input dates have no corresponding data file within %f days of available files", sum(dtime > dtimetest), dtimetest))
  ##    windex <- windex[dtime <= dtimetest]
    }

      ## work through all the unique indexes

    uindex <- unique(windex)
    extracteddata <- numeric(nrow(Query))



    for (ij in seq_along(uindex)) {
        thisindex <- windex == uindex[ij]
        d0 <- datafun(files$date[uindex[ij]], ...)
         ## get the cellnumbers just once
        if (ij == 1L) {
            extraction <- suppressWarnings(extract(d0, Query, cellnumbers = TRUE))
            cn <- extraction[,1]
            extracteddata[thisindex] <- extraction[thisindex,2]
        } else {
            extracteddata[thisindex] <- extract(d0, cn[thisindex])
        }
    }

    extracteddata
}

##' Functions to provide topographic (bathymetry and/or topography) data.
##'
##' Use \code{readtopo} (or its alias \code{readbathy}) to read data
##' from the chosen data set. The function \code{topofile} is used to
##' find the full file name.
##' @title Topography data
##' @name topofile
##' @aliases readtopo readbathy topofile
##' @param topo Data source, see Details.
##' @param lon180 Flag for returning data in Atlantic [-180, 180] rather than Pacific [0, 360] view.
##' @param polar Flag for returning the polar version of the IBCSO data.
##' @param ... reserved for future use, ignored currently
##' @details
##' The following data sets are available using the argument \code{topo}.
##' \describe{
##' \item{gebco_08}{The GEBCO_08 Grid, a global 30 arc-second grid largely generated by combining quality-controlled ship depth soundings with interpolation between sounding points guided by satellite-derived gravity data. \url{http://www.gebco.net/data_and_products/gridded_bathymetry_data/}}
##' \item{ibcso}{IBCSO bathymetry data, resolution 1min, use argument \code{polar = TRUE} to return the projected version (polar stereographic with true scale at 71S, WGS84), 500m resolution. \url{http://www.ibcso.org/data.html}}
##' \item{etopo1}{ETOPO1 is a 1 arc-minute global relief model of Earth's surface that integrates land topography and ocean bathymetry. \url{http://www.ngdc.noaa.gov/mgg/global/global.html}}
##' \item{etopo2}{Historic and deprecated prior version of ETOPO1. \url{http://www.ngdc.noaa.gov/mgg/global/etopo2.html}}
##' \item{kerguelen}{Kerguelen Plateau Bathymetric Grid, GeoScience Australia}
##' \item{george_v_terre_adelie}{A bathymetric Digital Elevation Model (DEM) of the George V and Terre Adelie continental shelf and margin - 100, 250, and 500 metre resolution. \url{http://data.aad.gov.au/aadc/metadata/metadata_redirect.cfm?md=AMD/AU/GVdem_2008}}
##' \item{smith_sandwell}{Global seafloor topography from satellite altimetry and ship depth soundings. \url{http://topex.ucsd.edu/WWW_html/mar_topo.html}}
##' }
##' @return
##' \describe{
##' \item{}{\code{topofile} returns a character string of the full path to a file name}
##' \item{}{\code{readtopo} and \code{readbathy} return the requested data as a RasterLayer (these are aliases)}
##' }
##' @examples
##' fname <- topofile("ibcso", polar = TRUE)
##' ibcso <- raster(fname)
##' @export
topofile <- function(topo = c("gebco_08", "ibcso",
                              "etopo1", "etopo2",
                              "kerguelen", "george_v_terre_adelie",
                              "smith_sandwell"),
                     polar = FALSE,
                     lon180 = TRUE, ...) {

    data.dir = getOption("default.datadir")
    topo <- match.arg(topo)
    polarsubdir <- "latlon"
    if (polar) {
        if (topo %in% c("ibcso")) {
            polarsubdir <- "ps71"
           } else {
            warning("no polar version of ", topo, "consider projectRaster(x, crs = '+proj=stere +lat_0=-71')")
        }
    }
    if (!lon180 & !(topo %in% c("smith_sandwell"))) warning("no Pacific view version available of ", topo)
    topopath <- file.path(data.dir, "bathymetry", topo,
                          switch(topo,
                       gebco_08 = "gebco_08.tif",
                       ibcso = file.path(polarsubdir, "ibcso_v1_is.tif"),
                       etopo1 = "ETOPO1_Ice_g_gdal.grd",
                       etopo2 = "ETOPO2v2c_f4.nc",
                       kerguelen = "kerg_dem_100m.grd",
                       george_v_terre_adelie = "gvdem100m_v3.nc",
                       ## use the RAW file via GDAL VRT
                       smith_sandwell = if (lon180) "topo_15.1_Atlantic.vrt" else "topo_15.1.vrt")
                       )
    topopath
}

##' @rdname topofile
##' @export
readtopo <- function(topo = c("gebco_08", "ibcso",
                              "etopo1", "etopo2",
                              "kerguelen", "george_v_terre_adelie",
                              "smith_sandwell"),
                     polar = FALSE,
                     lon180 = TRUE, ...) {
    topo <- match.arg(topo)
    raster(topofile(topo = topo, polar = polar, lon180 = lon180, ...))
}
##' @rdname topofile
##' @export
readbathy <- readtopo

##' Load file names and dates of OISST sea surface temperature data
##'
##' A data frame of file names and datres
##' @title OISST sea surface temperature files
##' @param fromcache load from cache?
##' @param ... reserved for future use
##' @return data.frame of file names and dates
##' @export
sstfiles <- function(fromcache = TRUE) {
    data.dir <- getOption("default.datadir")
    if (fromcache) {
        load(file.path(data.dir, "cache", "sstfiles.Rdata"))
        return(sstf)
    }

    dirpath <- file.path(data.dir, "sst", "OI-daily-v2", "daily")
    fs <- list.files(dirpath, pattern = "\\.nc$", recursive = TRUE, full.names = TRUE)

    ## flakey!!!!
    fsstrings <- as.Date(substr(basename(fs), 15, 22), "%Y%m%d")

    dates <- timedateFrom(as.Date(fsstrings, "%Y%m%d"))

    sstf <- data.frame(files = fs, date = dates, stringsAsFactors = FALSE)[order(dates), ]
    save(sstf, file = file.path(getOption("cachepath"), "sstfiles.Rdata"))

    sstf

}


##' Read OISST sea surface temperature data from daily files
##'
##' SST data read from files managed by
##' \code{\link{sstfiles}}. Dates are matched to file names by finding
##' the nearest match in time within a short duration. If \code{date}
##' is greater than length 1 then the sorted set of unique matches is
##' returned.
##' @param date date or dates of data to read, see Details
##' @param time.resolution time resoution data to read, daily only
##' @param varname variable to return from the data files, default is
##' "sst" or "anom", "err", "ice"
## @param setNA mask zero and values greater than 100 as NA
## @param rescale rescale values from integer range?
## @param debug ignore data request and simply report on what would be returned after processing arguments
##' @param lon180 defaults to TRUE, to "rotate" Pacific view [0, 360] data to Atlantic view [-180, 180]
##' @param returnfiles ignore options and just return the file names and dates
##' @param ... reserved for future use, currently ignored
##' @export
##' @return \code{\link[raster]{raster}} object
##' @seealso \code{\link{icefiles}} for details on the repository of
##' data files, \code{\link[raster]{raster}} for the return value
readsst <- function(date = as.Date("1981-09-01"), time.resolution = "daily", varname = c("sst", "anom", "err", "ice") ,
                    lon180 = TRUE,
                    returnfiles = FALSE,
                    ...) {
    time.resolution <- match.arg(time.resolution)
    varname <- match.arg(varname)
    date <- timedateFrom(date)
    files <- sstfiles()
    if (returnfiles) return(files)
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
   rtemplate <- raster(files$file[windex[1]], varname = varname)
    if (lon180) rtemplate <- rotate(rtemplate)
    if (length(windex) > 1L) {
      r <- brick(nrows = nrow(rtemplate), ncols = ncol(rtemplate),
                 xmn = xmin(rtemplate), xmx = xmax(rtemplate), ymn = ymin(rtemplate), ymx = ymax(rtemplate),
                 crs = projection(rtemplate),
                 nl = length(windex))
    }

    for (i in seq_along(windex)) {
        r0 <- raster(files$file[windex[i]], varname = varname)
        if (lon180) r0 <- rotate(r0)
        r0[r0 < -2] <- NA
        if (length(windex) > 1) {
          r <- setValues(r, values(r0), layer = i)
      } else {

          return(setZ(r0, date))
      }
    }
    return(setZ(r, date))

}


##' Load file names and dates of AVISO current data
##'
##' A data.frame of file names and dates
##' @title AVISO ocean currents files
##' @seealso \code{\link{readcurr}}
##' @return data.frame of file names and dates
##' @export
currentsfiles <- function() {
    data.dir = getOption("default.datadir")
    data.source = file.path(data.dir, "current", "aviso", "upd", "7d")
    cfiles <- list.files(data.source, pattern = ".nc$", full.names = TRUE)
    datepart <- sapply(strsplit(basename(cfiles), "_"), function(x) x[length(x)-1])
    currentdates <- timedateFrom(as.Date(strptime(datepart, "%Y%m%d")))
    data.frame(file = cfiles, date = currentdates, stringsAsFactors = FALSE)
}

##' Read AVISO ocean current data from weekly files
##'
##' Current data is read from files managed by
##' \code{\link{currentsfiles}}. Dates are matched to file names by
##' finding the nearest match in time within a short duration. By
##' default only one time step is returned with both U and V
##' components. Multiple dates can be returned for magnitude or
##' direction only.
##' @param date date or dates of data to read, see Details
##' @param time.resolution time resolution to read
## @param setNA mask zero and values greater than 100 as NA
## @param rescale rescale values from integer range?
##' @param magonly return just the magnitude from the U and V
##' components
##' @param dironly return just the direction from the U and V
##' @param lon180 defaults to TRUE, to "rotate" Pacific view [0, 360] data to Atlantic view [-180, 180]
##' components, in degrees (0 north, 90 east, 180 south, 270 west)
##' @param returnfiles ignore options and just return the file names and dates
##' @param ... reserved for future use, currently ignored
##' @export
##' @note These data are stored in a Mercator projection on Pacific
##' view \[0, 360\], the default behaviour is to reset this to Atlantic
##' view \[-180, 180\] with \code{lon180}. The Mercator projection is
##' preserved, see \code{\link[raster]{projectRaster}} and
##' \code{\link[raster]{resample}} for transformation methods.
##' @return \code{\link[raster]{raster}} object
##' @seealso \code{\link{icefiles}} for details on the repository of
##' data files, \code{\link[raster]{raster}} for the return value
# imports should not be necessary here
##' @importFrom raster t flip atan2
##' @export
readcurr <- function(date = as.Date("1999-11-24"),
                     time.resolution = "weekly",
                     ##setNA = TRUE,
                     ##rescale = TRUE,
                     magonly = FALSE,
                     dironly = FALSE,
                     lon180 = TRUE,
                     returnfiles = FALSE,
                     ...) {

     ## function to read just one
    read0 <- function(x, varname) {
        xtreme <- 20037508
        ytreme <- 16925422
        x <- flip(flip(t(raster(x, varname = varname)), direction = "y"), direction = "x")
        extent(x) <- extent(0, xtreme * 2, -ytreme, ytreme)
        projection(x) <- "+proj=merc +ellps=WGS84 +over"
        x
    }
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

    files <- currentsfiles()
    if (returnfiles) return(files)
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

    if (magonly & dironly) warning("only one of magonly and dironly may be used, returning magonly")
    ## now check which of these have a valid file within the resolution
    dtime <- abs(difftime(date, files$date[windex], units = c("days")))

    dtimetest <- switch(time.resolution,
                        weekly = 4)
    if (all(dtime > dtimetest)) stop(sprintf("no data file within %.1f days of %s", dtimetest))
    if (any(dtime > dtimetest)) {
      warning(sprintf("%i input dates have no corresponding data file within %f days of available files", sum(dtime > dtimetest), dtimetest))
      windex <- windex[dtime <= dtimetest]
    }
    ## prevent reading more than one unless mag/dironly
    if (length(windex) > 1L & !magonly & !dironly) {
        windex <- windex[1L]
        date <- date[1]
        warning("only one time step can be read at once")
    }
    i <- 1

         r1 <- read0(files$file[windex[i]], varname = "Grid_0001")
         r2 <- read0(files$file[windex[i]], varname = "Grid_0002")
    if (!(magonly | dironly)) {
        r <- brick(r1, r2)
         names(r) <- c("U", "V")
         return(r)
    }
    if (magonly) rasterfun <- function(x1, x2) sqrt(x1 * x1 + x2 *x2)
    if (dironly) rasterfun <- function(x1, x2) (90 - atan2(x2, x1) * 180/pi) %% 360

    r <- brick(rasterfun(r1, r2), nl = length(windex))

    for (i in seq_along(windex)[-1]) {
        r1 <- read0(files$file[windex[i]], varname = "Grid_0001")
        r2 <- read0(files$file[windex[i]], varname = "Grid_0002")
        r <- setValues(r, values(rasterfun(r1, r2)), layer = i)

    }
     r <- setZ(r, date)
    if (lon180) r <- suppressWarnings(rotate(r))
    return(r)

}

.loadfiles <- function(name, time.resolution, ...) {
    switch(name,
           nsidc = icefiles(time.resolution = time.resolution)

           )
}

   .valiDates <- function(x, allOK = TRUE) {
        xs <- timedateFrom(x)
        bad <- is.na(xs)
        if (all(bad)) stop("no input dates are valid")
        if (any(bad)) {
            notOK <- "not all input dates are valid"
            if (allOK) stop(notOK) else warning(notOK)
        }
        xs[!bad]
    }


    .sortDates <- function(x, resortOK = FALSE) {
        ord <- order(x)
        if (any(diff(ord) < 0)) {
            sortOK <- "dates out of order and will be sorted"
            if (resortOK) warning(sortOK) else stop(sortOK)
            x <- x[ord]
        }
        x
    }



    .indexDates <- function(xdate, filedate) {
        windex <- integer(length(xdate))
        for (i in seq_along(xdate)) {
            windex[i] <- which.min(abs(xdate[i] - filedate))
        }


        windex
    }

  .dedupe <- function(index, date, removeDupes = TRUE) {
        nondupes <- !duplicated(index)
        if (sum(nondupes) < length(index)) {
            if (removeDupes) warning("duplicated dates will be dropped") else stop("duplicated dates not allowed")
            index <- index[nondupes]
            date <- date[nondupes]
        }
        list(index = index, date = date)
    }

    .matchFiles <- function(querydate, refdate, index, daytest = 7) {
        ##
        deltatime <- abs(difftime(querydate, refdate, units = "days"))
        deltatest <- deltatime > daytest
        if (all(deltatest)) stop(sprintf("no data file within %.1f days of %s", daytest))
        if (any(deltatest)) {
            warning(sprintf("%i input dates have no corresponding data file within %f days of available files", sum(deltatest), daytest))
            index <- index[deltatest]
        }
        index
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
##' @param returnfiles ignore options and just return the file names and dates
##' @param ... reserved for future use, currently ignored
##' @export
##' @return \code{\link[raster]{raster}} object
##' @seealso \code{\link{icefiles}} for details on the repository of
##' data files, \code{\link[raster]{raster}} for the return value
readice <- function(date = as.Date("1978-11-01"),
                    time.resolution = c("daily", "monthly"),
                    setNA = TRUE, rescale = TRUE,
                    debug = FALSE,
                    returnfiles = FALSE, ...) {
    datadir = getOption("default.datadir")
    time.resolution <- match.arg(time.resolution)

    ## get file names and dates and full path
    files <- .loadfiles("nsidc", time.resolution = time.resolution)
    files$fullname <- file.path(datadir, files$file)
    if (returnfiles) return(files)

    ## checks on dates, we drop any that are NA
    date <- .valiDates(date, allOK = FALSE)

    ## sort dates if need be
    date <- .sortDates(date, resortOK = TRUE)

    ## mapping of files/dates, so we can process time series
    findex <- .indexDates(date, files$date)

    ## check for duplicates
    dedupedates <- .dedupe(findex, date, removeDupes = TRUE)
    findex <- dedupedates$index
    date <- dedupedates$date


    ## from this point one, we don't care about the input "date" - this is our index into all files and that's what we use
    findex <- .matchFiles(date, files$date[findex], findex, daytest = switch(time.resolution,daily = 1.5, monthly = 15))


    ## NSIDC projection and grid size for the Southern Hemisphere
    stersouth <-  "+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
    dims <- c(316L, 332L)
    rtemplate <- raster(GridTopology(c(-3937500, -3937500), c(25000, 25000), dims))
    if (length(findex) > 1L) {
      r <- brick(nrows = nrow(rtemplate), ncols = ncol(rtemplate),
                 xmn = xmin(rtemplate), xmx = xmax(rtemplate), ymn = ymin(rtemplate), ymx = ymax(rtemplate),
                 nl = length(findex))
    }
    ## loop over file indices
    for (ifile in seq_along(findex)) {
      con <- file(files$fullname[findex[ifile]], open = "rb")
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
      if (length(findex) > 1) {
          r <- setValues(r, matrix(dat, dims[1]), layer = ifile)
      } else {
          r <- raster(t(matrix(dat, dims[1])), template = rtemplate)
      }
    }


    projection(r) <- stersouth
    names(r) <- files$file[findex]
    r <- setZ(r, files$date[findex])
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





