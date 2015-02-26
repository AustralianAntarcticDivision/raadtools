##' R tools for spatial data, extensions using raster to read and extract
##'
##' Tools in R for reading, plotting and manipulating spatial data, originally 
##' used at the Australian Antarctic Division (AAD).
##' @author Michael D. Sumner \email{mdsumner@@gmail.com}
##'
##' Maintainer: Michael D. Sumner \email{mdsumner@@gmail.com}
##'
##' @name raadtools
##' @docType package
##' @keywords package
##' @import methods
##' @importFrom sp as.image.SpatialGridDataFrame bbox CRS GridTopology overlay proj4string<- SpatialPoints SpatialPointsDataFrame spChFIDs spTransform 
##' @importFrom maptools ContourLines2SLDF spRbind
##' @importFrom raster brick crop deratify extent<- extract getZ nlayers projection projection<- raster res resample rotate setZ stack writeRaster xmax xmin ymax ymin

NULL


## internal rotate to match old behaviour
## https://r-forge.r-project.org/scm/viewvc.php/pkg/raster/R/rotate.R?root=raster&r1=2782&r2=2981
#' @importFrom raster merge
.rotate <-  function(x, ...) {
  e <- extent(x)
  xrange <- e@xmax - e@xmin
  inverse <- FALSE
  if (xrange < 350 | xrange > 370 | e@xmin < -10 | e@xmax > 370) {
    if (xrange < 350 | xrange > 370 | e@xmin < -190 | e@xmax > 190) {
      warning('this does not look like an appropriate object for this function')
    } else {
      inverse <- TRUE
    }
  }
  hx <- e@xmin + xrange / 2
  r1 <- crop(x, extent(e@xmin, hx, e@ymin, e@ymax))
  r2 <- crop(x, extent(hx, e@xmax, e@ymin, e@ymax))
  if (inverse) {
    r1@extent@xmin <- r2@extent@xmax
    r1@extent@xmax <- r1@extent@xmin + 0.5 * xrange
  } else {
    r2@extent@xmin <- r2@extent@xmin - xrange
    r2@extent@xmax <- r2@extent@xmax - xrange
  }
  ln <- names(x)
  out <- merge(r1, r2, overlap=FALSE, ...)
  names(out) <- names(x)
  out@z <- x@z
  
  # suggested by Mike Sumner:
  p <- projection(out)	
  if (length(grep("\\+over", p)) > 0) {
    projection(out) <- gsub("[[:space:]]\\+over", "", p)
  }
  
  return(out)
}




##' Read map images.
##'
##' The ancient lost art of cartography.
##'
##' ibcso_background: The IBCSO RGB-map rasterlayer in high resolution
##' @title Maps of places
##' @param map name of the map to load
##' @param fact resize factor, see \code{\link[raster]{aggregate}}
##' @return RasterBrick, with R G B bands
##' @references
##' \url{http://www.ibcso.org/data.html}
##' @export
imagemap <- function(map = c("ibcso_background_hq"),
                       fact = 1L) {
    datadir <- getOption("default.datadir")
    map <- match.arg(map)
    fpath <- switch(map,
                    ibcso_background_hq = file.path(datadir,  "bathymetry", "ibcso", "image", "ibcso_background_hq.tif")
                   ## ant_and_sthn_ocean_13989 = file.path(datadir,  "maps", "ant_and_sthn_ocean_13989.tif"),
                   ## ant_sthn_ocean_ed9_13939 = file.path(datadir, "maps", "ant_sthn_ocean_ed9_13939.tif"),
                   ## kerguelen_to_antarctica_bathy_14033 = file.path(datadir, "maps",  "kerguelen_to_antarctica_bathy_14033.tif")
        )

    if (file.exists(fpath) & interactive()) message("\n\nremember to plot with plotRGB(x)")
    x <- brick(fpath)
    if (fact > 1) {
        ##aggregate(x, fact, fun = function(x, na.rm = TRUE) sample(x, 1L))
        x <- resample(x, raster(extent(x), nrows = ceiling(nrow(x)/fact), ncol = ceiling(ncol(x)/fact), crs = projection(x)), method = "ngb")

    }
    names(x) <- c("R", "G", "B")
    x
}



##' Load spatial map of fronts data.
##'
##' Currently ORSI is the only supported layer.
##'
##' "orsi" - the ORSI fronts derived from the files provided by the
##' WOCE Atlas, see References
##' @title Fronts map data for the Southern Ocean
##' @param map name of map to load
##' @references \url{http://woceatlas.tamu.edu/Sites/html/atlas/SOA_DATABASE_DOWNLOAD.html}
##' @return SpatialLinesDataFrame
##' @export
frontsmap <- function(map = c("orsi")) {
    .orsi()
}

.orsi <- function(layer = "orsi") {
    datapath <- getOption("default.datadir")
    cachepath <- file.path(datapath, "cache")
    f <- file.path(cachepath, grep(layer, list.files(cachepath), value = TRUE))
    load(f)
    return(get(layer))
}
##' Coastline data set as SpatialPolygons*
##'
##' This function reads and returns a coastline polygon data set, either from the
##' source or serialized cache (.Rdata). Data source locations are controlled by options.
##'
##' The following named data sets are available.
##'
##' "world", "world2" - Atlantic and Pacific versions of \code{\link[maptools]{wrld_simpl}} ("world1", "world360" are aliases)
##' "ant_coast", "ant_coast01", "ant_coast10" - AAD Antartic coast in full, "1 mill", and "10 mill" resolution ("cst00_polygon", "cst01_polygon", and "cst10_polygon" are aliases)
##' "Countries_hires" - the "CIA" world map exported from the Manifold GIS data set
##'
##' @title Coast map
##' @param map A named map source
##' @param \dots arguments passed to worker functions
##' @return  SpatialPolygonsDataFrame or SpatialPolygons (world1/2)
##' @examples
##' ## load the maptools::wrld_simpl data set in [0,360]
##' \dontrun{
##' w360 <- coastmap("world360")
##' }
##'
##' ## load the AAD coast layer in "1 mill" resolution
##' \dontrun{
##' cst01 <- coastmap("ant_coast10")
##' }
##' @export
coastmap <- function(map = c(
                     "world", "world2",
                     "ant_coast", "ant_coast01", "ant_coast10",
                     "Countries_hires",
                     "world1", "world360",
                     "cst00_polygon", "cst01_polygon", "cst10_polygon"), ...) {
##                     "GA_shelf", "GA_shelf_longlat", "GA_shelf_line", "GA_shelf_tosouth"), ...) {

  map <- match.arg(map)

  res <- switch(map,
                world = .world(),
                world1 = .world(),   ## synonym of world
                world2 = .world(FALSE),
                world360 = .world(FALSE),  ## synonym of world2
                ant_coast = .aadcoast(layer = "cst00_polygon", ...),
                ant_coast01 = .aadcoast(layer = "cst01_polygon", ...),
                ant_coast10 = .aadcoast(layer = "cst10_polygon", ...),
                cst00_polygon = .aadcoast(layer = "cst00_polygon", ...),  ## synonym of ant_coast
                cst01_polygon = .aadcoast(layer = "cst01_polygon", ...),  ## synonym of ant_coast01
                cst10_polygon = .aadcoast(layer = "cst10_polygon", ...),   ## synonym of ant_coast10
                Countries_hires = .manifoldcoast(layer = "Countries_hires")
                ##GA_shelf = .geoscience(layer = "Geomorph_shelf_laea"),
                ##GA_shelf_longlat = .geoscience(layer = "Geomorph_shelf_longlat"),
                ##GA_shelf_line = .geoscience(layer = "Geomorph_shelf_line"),
                ##GA_shelf_tosouth = .geoscience(layer = "Geomorph_shelf_tosouth")
  )
  res
}

.manifoldcoast <-  function(layer = c("Countries_hires"), fromCache = TRUE, debug = FALSE) {
  datapath <- getOption("default.datadir")
  cachepath <- file.path(datapath, "cache", "vector_cache")

  layer = match.arg(layer)

  if (fromCache & !is.null(cachepath) & file.exists(cachepath)) {
    f <- file.path(cachepath, grep(layer, list.files(cachepath), value = TRUE))
    if (debug) print("loading from cache")
    if (debug) print(layer)
    if (debug) print(f)

    load(f)
    return(get(layer))
  }
  ##layerpath <- switch(layer,
  ##                    Countries_hires= file.path(datapath, "coastline")
  ## )
  ## if(debug) print(layerpath)
  ## if(debug) print(layer)
  ## if(require(rgdal)) {
  ## readOGR(layerpath, layer)
  ## } else {
  ##   stop(sprintf("cannot read layer %s from %s", layer, layerpath))
  ## }

}


##.geoscience <- function(layer) {
##    require(rgdal)
##     readOGR("D:/Toolbox/data_candidates/GeoScience", layer)
##}

##' @importFrom rgeos gIntersection gUnion
##' @importFrom maptools elide
##' @importFrom raster extent
##' @importMethodsFrom raster extent
 .world <-
function(world1 = TRUE) {

    on.exit(detach(pos = pos))
    attach(system.file("data", "wrld_simpl.rda", package = "maptools"))
    pos <- rev(grep("wrld_simpl.rda", search()))[1L]
    wrld <- get("wrld_simpl", pos = pos)
    ##detach(pos = pos)
    if (world1) return(as(wrld, "SpatialPolygons"))
    bb <- bbox(wrld)
    opt <- options(warn = -1)
    on.exit(options(opt))
    w1 <- gIntersection(wrld, as(extent(-180, 0, bb[2,1], bb[2,2]), "SpatialPolygons"), byid = TRUE)
    w2 <- gIntersection(wrld, as(extent(0,180, bb[2,1], bb[2,2]), "SpatialPolygons"), byid = TRUE)
    wrld <- gUnion(elide(w1, shift = c(360, 0)), w2, byid = TRUE)
    proj4string(wrld) <- CRS(" +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0 +over")
    return(wrld)
}

.aadcoast <-
function(layer = c("cst10_polygon", "cst01_polygon", "cst00_polygon"), fromCache = TRUE, debug = FALSE) {
     ##require(rgdal)
      datapath <- getOption("default.datadir")
  cachepath <- file.path(datapath, "cache", "vector_cache")

##     gispath <- getOption("gispath")
##     cachepath <- getOption("cachepath")

     layer = match.arg(layer)

     if (fromCache & !is.null(cachepath) & file.exists(cachepath)) {
         f <- file.path(cachepath, grep(layer, list.files(cachepath), value = TRUE))
         if (debug) print("loading from cache")
         if (debug) print(layer)
         if (debug) print(f)

         load(f)
         return(get(layer))
     }
  ##   layerpath <- switch(layer,
  ##                    cst10_polygon = file.path(gispath, "Antarctic coast", "add_ver6", "10mill"),
  ##                    cst01_polygon = file.path(gispath, "Antarctic coast", "add_ver6", "1mill"),
  ##                    cst00_polygon = file.path(gispath, "Antarctic coast", "add_ver6", "best_resolution")
  ##                    )
  ##   readOGR(layerpath, layer)


    }


##' Read the \enc{Sall?e}{Sallee} Mixed layer depth climatology.
##'
##' http://soki.aad.gov.au/display/Data/Mixed+layer+depth
##' @title Mixed Layer Depth
##' @param date date to extract, can only be a climatology month
##' @param xylim extent specification
##' @param returnfiles return the file details only
##' @param ... ignored
##' @return RasterBrick
##' @importFrom raster subset
##' @encoding ISO-8859-2
##' @export
readmld <- function(date, xylim = NULL, returnfiles = FALSE, ...) {
    if (missing(date)) date <- seq(as.Date("2013-01-01"), by = "1 month", length = 12L)

    date <- timedateFrom(date)

    date <- format(date, "%b")
    x <- .loadMLD()
    if (!is.null(xylim)) x  <- crop(x, extent(xylim))
    if (returnfiles) return(data.frame(file = file.path(getOption("default.datadir"), "cache", "sallee_mld2013.Rdata"), date = timedateFrom(seq(as.Date("2013-01-01"), by = "1 month", length = 12L))))
    warning("MLD data is only a climatology, returning matching month only")
    subset(x, date)
}


.loadMLD <- function() {
    p <- file.path(getOption("default.datadir"), "cache", "sallee_mld2013.Rdata")
    if (!file.exists(p)) return(NULL)
    mld <- NULL
    load(p)
    mld
}










## shared stuff
## datadir
## normalize input dates - need index and value

## private, but common
## dims, projection, bbox
## files
 .processFiles <- function(dt, f, tr) {
        findex <- .processDates(dt, f$date, tr)
        f[findex, ]
    }


.expandFileDateList <- function(x) {
    vl <- vector("list", length(x))
    for (i in seq_along(x)) {
        b <- brick(x[i])
        dates <- timedateFrom(getZ(b))
        vl[[i]] <- data.frame(file = rep(x[i], length(dates)), date = dates, band = seq_along(dates))
    }
    do.call("rbind", vl)
}


.loadfiles <- function(name, time.resolution, ...) {
    switch(name,
           nsidc = icefiles(time.resolution = time.resolution),
           ssmi = icefiles(product = "ssmi"),
           johnson = chlafiles(time.resolution = time.resolution, product = "johnson"),
           oceancolor = chlafiles(time.resolution = time.resolution, product = "oceancolor")

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
        if (all(deltatest)) stop(sprintf("no data file within %.1f days of %s", daytest, format(querydate)))
        if (any(deltatest)) {
            warning(sprintf("%i input dates have no corresponding data file within %f days of available files", sum(deltatest), daytest))
            index <- index[!deltatest]
        }
        index
    }

   .processDates <- function(qdate, fdate, timeres) {
        ## checks on dates, we drop any that are NA
        qdate <- .valiDates(qdate, allOK = FALSE)

        ## sort dates if need be
        qdate <- .sortDates(qdate, resortOK = TRUE)

        ## mapping of files/dates, so we can process time series
        findex <- .indexDates(qdate, fdate)

        ## check for duplicates
        dedupedates <- .dedupe(findex, qdate, removeDupes = TRUE)
        findex <- dedupedates$index
        date <- dedupedates$date

        .matchFiles(date, fdate[findex], findex, daytest = switch(timeres,daily = 1.5, weekly = 4, monthly = 15, weekly3 = 26))
    }



##' Data frame of all available fast ice files.
##'
##' A data frame with file, date, fullname
##' @title fast ice files
##' @param datadir data repository path
##' @param ... reserved for future use, currently ignored
##' @return data frame
##' @export
fasticefiles <- function(datadir = getOption("default.datadir"), ...) {
    pref <- file.path("fastice", "fraser_fastice", "binary_fast_ice")
    fs <- list.files(file.path(datadir, pref), pattern = "img$")
    dates <- as.POSIXct(strptime(fs, "binary_%Y_%j"), tz = "GMT")
    data.frame(file = file.path(pref, fs), date = dates, fullname = file.path(datadir, pref, fs), stringsAsFactors = FALSE)
}

##' Read fast ice data, optionally with a mask
##'
##' Fast ice data on original Equal Area Cylindrical grid
##' @title Fast ice data
##' @param date date or dates to read (can be character, POSIXt, or Date)
##' @param time.resolution fixed at roughly "3 weekly"
##' @param xylim extent in native space of the grid
##' @param returnfiles return the file details only
##' @param ... reserved for future use, currently ignored
##' @return RasterBrick with 1 for fast ice pixels, 0 for other, NA for land mask
##' @references \url{http://data.aad.gov.au/aadc/metadata/metadata.cfm?entry_id=modis_20day_fast_ice}
##' @export
##' @examples 
##'
##' r <- readfastice(c("2002-02-10", "2002-03-03"))
##' 
##'
readfastice <-
function(date, time.resolution = "weekly3",
         xylim = NULL, returnfiles = FALSE, ...) {

    dims <- c(4300, 425)
    datadir = getOption("default.datadir")
    gridmask <- t(matrix(readBin(file.path(datadir, "fastice", "fraser_fastice", "geoloc", "coastmask.img"), "integer", size = 2, n = prod(dims), endian = "little"), dims[1]))
     read0 <- function(x) {
        projstring <- "+proj=cea +lon_0=91 +lat_0=-90 +lat_ts=-65 +datum=WGS84"
        ## bbox in cea
        bb <- structure(c(-4751610.61938822, 3822717.4673464, -13464081.4706772,
                          -14314422.8015431), .Dim = c(2L, 2L))
        topleft <- bb[1,]
        botright <- bb[2,]


        d <- readBin(x, "integer", size = 1, n = prod(dims), endian = "little")
        d <- t(matrix(d, dims[1]))
        d[gridmask == 1] <- NA
        raster(d, crs = projstring, xmn = topleft[1], xmx = botright[1], ymn = botright[2], ymx = topleft[2])
    }

    files <- fasticefiles()

    if (missing(date)) date <- min(files$date)
    date <- timedateFrom(date)

    ## it would be nice here to trim down if there were input dates
    if (returnfiles) return(files)
    files <- .processFiles(date, files, time.resolution)

    cropit <- FALSE
    if (!is.null(xylim)) {
        cropit <- TRUE
        cropext <- extent(xylim)
    }

    nfiles <- nrow(files)
    r <- vector("list", nfiles)

    for (ifile in seq_len(nfiles)) {
        r0 <- read0(files$fullname[ifile])
        if (cropit) {
            r0 <- crop(r0, cropext)
        }
        r[[ifile]] <- r0
    }
    r <- if (nfiles > 1) brick(stack(r), ...) else r[[1L]]
    names(r) <- sprintf("fastice_%s", format(files$date, "%Y%m%d"))

    setZ(r, files$date)

}

##' Load file names and dates of Arrigo production data.
##'
##' These are 8 day estimates from MODIS and SeaWiFS satellite data,
##' original NASA algorithm.
##' @title Arrigo production files
##' @param fromCache load file catalog from cache, or rebuild it
##' @param ... reserved for future use, currently ignored
##' @return  data.frame of file names and dates
##' @export
prodfiles <- function(fromCache = TRUE, ...) {
    datadir <- getOption("default.datadir")
    dirpath <- file.path(datadir, "prod", "Arrigo", "8d")
    if (fromCache) {
        load(file.path(datadir, "cache", "prodfiles.Rdata"))
        pfiles$fullname <- file.path(datadir, pfiles$file)
        return(pfiles)
    }
    fs <- list.files(pattern = "prod\\.bin$", dirpath, full.names = TRUE)
    dates <- timedateFrom(as.Date(basename(fs), "%Y%j"))


    pfiles <- data.frame(file = gsub("^/", "", gsub(datadir, "", fs)), date = dates, stringsAsFactors = FALSE)
    ##save(pfiles, file = file.path(datadir, "cache", "prodfiles.Rdata"))
    pfiles

}

##' Read Arrigo production data.
##'
##' Arrigo production on Stereographic grid.
##' @title Arrigo production data
##' @param date date or dates of data to read
##' @param returnfiles if TRUE return just the files from \code{prodfiles}
##' @param time.resolution choice of temporal resolution, weekly only
##' @param xylim crop or not
##' @param ... reserved for future use, currently ignored
##' @return RasterLayer or RasterBrick
##' @export
readprod <- function(date,  time.resolution = "weekly", xylim = NULL, returnfiles = FALSE, ...) {

    read0 <- function(x) {
        proj <- "+proj=stere +lat_0=-90 +lon_0=180 +ellps=sphere"
        offset <- c(5946335, 5946335)
        dims <- c(1280L, 1280L)
        proddata <- readBin(x, numeric(), prod(dims), size = 4, endian = "little")
        proddata[proddata < 0] <- NA
        x <- list(x = seq(-offset[1L], offset[1L], length = dims[1L]),
                   y =  seq(-offset[2L], offset[2L], length = dims[2L]),
                   z = matrix(proddata, dims[1L])[rev(seq_len(dims[2L])),])

        raster(x, crs = proj)

    }


    time.resolution <- match.arg(time.resolution)

    files <- prodfiles()
    if (returnfiles) return(files)

    if (missing(date)) date <- min(files$date)
    date <- timedateFrom(date)
##    findex <- .processDates(date, files$date, time.resolution)

    files <- .processFiles(date, files, time.resolution)
    cropit <- FALSE
    if (!is.null(xylim)) {
        cropit <- TRUE
        cropext <- extent(xylim)
    }


    nfiles <- nrow(files)

    r <- vector("list", nfiles)

    for (ifile in seq_len(nfiles)) {
        r0 <- read0(files$fullname[ifile])
        if (cropit)
            r0 <- crop(r0, cropext)
        r[[ifile]] <- r0
    }

    r <- brick(stack(r), ...)
    names(r) <- sprintf("prod_%s", format(files$date, "%Y%m%d"))

    setZ(r, files$date)

}


##' Read Chlorophyll-a for the Southern Ocean
##'
##' Ocean colour Chlorophyll-a data. Default is to read from the Johnson Improved
##' chlorophyll-a estimates using Southern Ocean-specific calibration
##' algorithms, but the original MODIS and SeaWIFs products are also available via the argument \code{product}.
##'
##' Dates are matched to file names by finding the nearest match in
##' time within a short duration. If \code{date} is greater than
##' length 1 then the sorted set of unique matches is returned.
##'
##' @param date date or dates of data to read, see Details
##' @param time.resolution time resolution data to read, weekly or monthly
##' @param product choice of product, see Details
##' @param xylim spatial extents to crop from source data, can be anything accepted by \code{\link[raster]{extent}}
##' @param returnfiles ignore options and just return the file names and dates
##' @param verbose print messages on progress etc.
##' @param ... passed to brick, for \code{filename}
##' @references  Johnson, R, PG Strutton, SW Wright, A McMinn, and KM
##' Meiners (2013) Three improved satellite chlorophyll algorithms for
##' the Southern Ocean, J. Geophys. Res. Oceans, 118,
##' doi:10.1002/jgrc.20270
##' \url{http://onlinelibrary.wiley.com/doi/10.1002/jgrc.20270/full}
##'
##' @export
##' @return \code{\link[raster]{raster}} object
##' @seealso \code{\link{chlafiles}} for details on the repository of
##' data files, \code{\link[raster]{raster}} for the return value
##' @examples
##' \dontrun{
##' d <- readchla(c("2003-01-01", c("2003-06-01")),
##'          xylim = extent(100, 150, -70, -30))
##' }
##' @export
readchla <- function(date, time.resolution = c("weekly", "monthly"),
                     product = c("johnson", "oceancolor"),
                     xylim = NULL,

                    ##lon180 = TRUE,
                    returnfiles = FALSE,
                    verbose = TRUE,
                    ...) {

  time.resolution <- match.arg(time.resolution)
  product <- match.arg(product)
  files <- chlafiles(time.resolution = time.resolution, product = product)
  if (returnfiles) return(files)

  if (missing(date)) date <- min(files$date)
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
  names(r) <- basename(files$file)
  r <- setZ(r, files$date)
  return(r)
}

##' Chlorophyll-a for the Southern Ocean
##'
##' This function generates a list of available chlorophyll-a files, including SeaWiFS and MODIS.
##' @title Chlorophyll-a
##' @param time.resolution weekly (8day) or monthly
##' @param product choice of sea ice product, see \code{readchla}
##' @param ... reserved for future use, currently ignored
##' @return data.frame
##' @export
chlafiles <- function(time.resolution = c("weekly", "monthly"),
                      product = c("johnson", "oceancolor"), ...) {
  datadir <- getOption("default.datadir")
  product <- match.arg(product)
  time.resolution <- match.arg(time.resolution)
  fromCache <- TRUE
  if (fromCache) {
     ## print(file.path(datadir, "cache", sprintf("%s_%s_chlafiles.Rdata", product, time.resolution)))
    load(file.path(datadir, "cache", sprintf("%s_%s_chlafiles.Rdata", product, time.resolution)))
    chlf$fullname <- file.path(datadir,  chlf$file)
    return(chlf)
  }

}

.updatechlafiles <- function(datadir = getOption("default.datadir"), preferModis = TRUE) {
  tr <- c(monthly = "monthly", weekly = "8d")


  ## first johnson
  for (i in seq_along(tr)) {
      dirpath <- file.path("chl", "johnson", c("modis", "seawifs"), tr[i])

      fs <- gsub(datadir, "", list.files(file.path(datadir, dirpath), full.names = TRUE))
      fs <- gsub("^/", "", fs)

      if (!length(fs) > 0) {
          warning(sprintf("no files fould for %s at %s", tr[i], dirpath))
          next;
      }
      dates <- timedateFrom(strptime(substr(basename(fs), 2, 8), "%Y%j"))


      chlf <- data.frame(file = fs, date = dates,  stringsAsFactors = FALSE)[order(dates), ]
      ## implementing preferModis
      dupes <- which(duplicated(chlf$date)) - !preferModis
      if (length(dupes) > 0) chlf <- chlf[-dupes, ]
      save(chlf, file = file.path(datadir, "cache", sprintf("johnson_%s_chlafiles.Rdata", names(tr[i]))))
  }
  ## now oceancolor

 for (i in seq_along(tr)) {
      dirpath <- file.path("chl", "oceancolor", c("modis", "seawifs"), tr[i], "netcdf")

      fs <- list.files(file.path(datadir, dirpath), full.names = TRUE)

        if (!length(fs) > 0) {
          warning(sprintf("no files fould for %s at %s", tr[i], dirpath))
          next;
      }
      xfs <- .expandFileDateList(fs)
      fs <- gsub(datadir, "", xfs$file)
      fs <- gsub("^/", "", fs)

      dates <- xfs$date  ##timedateFrom(strptime(substr(basename(fs), 2, 8), "%Y%j"))
      chlf <- data.frame(file = fs, date = dates,  band = xfs$band, stringsAsFactors = FALSE)[order(dates), ]
      ## implementing preferModis
      dupes <- which(duplicated(chlf$date)) - !preferModis
      if (length(dupes) > 0) chlf <- chlf[-dupes, ]
      save(chlf, file = file.path(datadir, "cache", sprintf("oceancolor_%s_chlafiles.Rdata", names(tr[i]))))
  }

}


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




##internal for now
## .. content for \description{} (no empty lines) ..
##
## .. content for \details{} ..
## title
## param dates
## param level
## param simplify
## return SpatialLinesDataFrame with columns
## \item{level} The raw integer index from the Sokolov/Rintoul data set
## \item{front} The matching name of the front region (top or bottom?)
## \item{date} The date of the week
contourfronts <-
function(date, level = NULL, simplify = TRUE) {
  ##conts <- c(sBdy =  1, SACCF_S = 2, SACCF_N = 3,PF_S = 4,PF_M = 5, PF_N =  6, SAF_S = 7, SAF_M = 8, SAF_N =  9, SAZ_S = 10, SAZ_M = 11, SAZ_N = 12)

  ##if (!is.null(level)) conts <- conts[level]
  files <- readfronts(returnfiles = TRUE)
  if (missing(date)) date <- files$date[1L]
  files <- .processFiles(date, files, "weekly")
  nfiles <- nrow(files)
  for (ifile in seq_len(nfiles)) {
    f <- readfronts(files$date[ifile], RAT = TRUE)
    if (ifile == 1L) levs <- levels(f)[[1L]]
    ## better explore this, drop the south of sBdy?
    cl0 <-  ContourLines2SLDF(contourLines(as.image.SpatialGridDataFrame(as(deratify(f, complete = TRUE), "SpatialGridDataFrame")), levels = levs$ID[-1L]))
    if (simplify) {
      cl0 <- .dropAllButCoordiest(cl0)
    }
    proj4string(cl0) <- CRS(projection(f))
    cl0$front <- levs$name[-1L]
    cl0$date <- files$date[ifile]

    if (ifile > 1) {
      cl0 <- .incrementIDs(cl0, ifile)
      ## this obviously is slow, better fix
      cl <- spRbind(cl, cl0)
    } else {
      cl <- cl0
    }
    if (interactive()) invisible("harass user")
  }
  spTransform(cl, CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
}


.incrementIDs <- function(x, incr) {
  ids <- paste(rownames(as.data.frame(x)), incr, sep = "_")
  spChFIDs(x, ids)
}

.dropAllButCoordiest <-
function(x) {
    for (iObj in seq_len(nrow(x))) {
        if (inherits(x, "SpatialLinesDataFrame")) {
                        wmax <- which.max(sapply(x[iObj, ]@lines[[1]]@Lines, function(x) nrow(x@coords)))
                        x@lines[[iObj]]@Lines <- x@lines[[iObj]]@Lines[wmax]
            }
        if (inherits(x, "SpatialPolygonsDataFrame")) {
             wmax <- which.max(sapply(x[iObj, ]@lines[[1]]@Lines, function(x) nrow(x@coords)))
             x@lines[[iObj]]@Lines <- x@lines[[iObj]]@Lines[wmax]
        }
    }
        x
}






##' Read data from the Sokolov/Rintoul Southern Ocean fronts analysis.
##'
##'
##' Sokolov Rintoul
##' @param date date or dates of data to read, see Details
##' @param time.resolution time resoution data to read, daily or monthly
##' @param product choice of product, see Details
##' @param xylim spatial extents to crop from source data, can be anything accepted by \code{\link[raster]{extent}}, see Details
##' @param lon180 if TRUE, data originally in Pacific view will be returned in Atlantic view (-180:180)
##' @param returnfiles ignore options and just return the file names and dates
##' @param RAT if \code{TRUE} data is returned with region names as a raster attribute table on the gridded data, see \code{\link[raster]{ratify}}
##' @param ... reserved for future use, currently ignored
##' @export
##' @return \code{\link[raster]{raster}} object
##' @examples
##' \dontrun{
##' b <- readfronts(c("1993-01-01", "2005-01-02"), lon180 = FALSE)
##' extract(readfronts, data.frame(aurora[,1:2], aurora[,3] - 10 * 365.25 * 24 * 3600)
##' }
readfronts <- function(date,
                    time.resolution = c("weekly"),
                    product = c("sokolov"),
                    xylim = NULL,
                       lon180 = TRUE,
                       returnfiles = FALSE, RAT = TRUE, ...) {
       datadir = getOption("default.datadir")
    time.resolution <- match.arg(time.resolution)

    product <- match.arg(product)
       file <- file.path(datadir, "fronts", "ACCfronts.nc")
      wks <- seq(timedateFrom("1992-10-14"), by = "7 days", length = 854)
    ## get file names and dates and full path
    files <- data.frame(file = file.path("fronts", "ACCfronts.nc"), fullname = file.path(datadir, "fronts", "ACCfronts.nc"),
                        date = wks, band = seq_along(wks), stringsAsFactors = FALSE)

       ##frontname <- c("sBdy", "SACCF_S", "SACCF_N", "PF_S", "PF_M", "PF_N", "SAF_S",
       ##          "SAF_M", "SAF_N", "SAZ_S", "SAZ_M", "SAZ_N")

       if (returnfiles) return(files)

       if (missing(date)) date <- min(files$date)

       ##findex <- .processDates(date, files$date, time.resolution)
       ##date <- files$date[findex]
       files <- .processFiles(date, files, time.resolution)
       nfiles <- nrow(files)

       proj <- "+proj=merc +ellps=WGS84"
       if (!lon180) proj <- paste(proj, "+over")
       ##extreme.points <- as.matrix(expand.grid(c(-180, 180), c(-82, -30.24627)))
       ##epoints.merc <- project(extreme.points, proj)
##       epoints.merc <- structure(c(-20037508, 20037508, -20037508,
  ##                                 20037508, -16925422, -16925422, -3513725, -3513725), .Dim = c(4L, 2L))
       epoints.merc <- structure(c(0, 2 * 20037508, 0,
                                   2 * 20037508, -16925422, -16925422, -3513725, -3513725), .Dim = c(4L, 2L))

l <- vector("list", nfiles)
       for (i in seq_along(l)) {
           r0 <- raster(file, band = files$band[i], stopIfNotEqualSpaced=FALSE)
           extent(r0) <- extent(bbox(epoints.merc))
           projection(r0) <- proj
           e <- new("Extent", xmin = 0, xmax = 2 * 20037508, ymin = -11087823.8567493 , ymax = -3513725)
           if (!is.null(xylim)) r0<- crop(r0, extent(xylim)) else r0 <- crop(r0, e)

           if (lon180)  r0 <- suppressWarnings(.rotate(r0))
    r0[is.nan(r0)] <- NA
             if (RAT) {
           rat <- data.frame(ID = 0:12, name = c("south of sBdy", "between SACCF-S & sBdy", "SACCF-N & SACCF-S",
"PF-S & SACCF-N", "PF-M & PF-S", "PF-N & PF-M", "SAF-S & PF-N",
"SAF-M & SAF-S", "SAF-N & SAF-M", "SAZ-S & SAF-N", "SAZ-M & SAZ-S",
"SAZ-N & SAZ-M", "north of SAZ-N"))
           levels(r0) <- rat
       }
           l[[i]] <- r0
       }



       r <- if (length(l) > 1) setZ(brick(stack(l)), files$date) else setZ(l[[1L]], files$date)
        ## lots of cells are wasted with nodata

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



.possiblepaths <- function() {
  a <- list(default.datadir =  c(
    "/Volumes/files/data",
    "/mnt/raad"))
  a
}
.trysetpath <- function() {
  possibles <- .possiblepaths()[["default.datadir"]]
  success <- FALSE
  existing <- getOption("default.datadir")
  if (!is.null(existing)) {
    possibles <- c(existing, possibles)
  } else {
    
  }
  for (i in seq_along(possibles)) {
    fi <- file.info(file.path(possibles[i], "data"))
    if (!is.na(fi$isdir) & fi$isdir) {
      options(default.datadir = possibles[i])
      success <- TRUE
      break;
    }
  }
  ## try RAAD_DIR, which may only be available to R CMD check from ~/.R/check.Renviron
  r <- getOption("repos")
  dd <- getOption("default.datadir")
  if (is.null(dd["default.datadir"])) {
    dd["default.datadir"] <- Sys.getenv("RAAD_DIR");
    options(repos = r, default.datadir = dd); 
  }
  
  
  success
}
.onAttach <- function(libname, pkgname) {
  pathwasset <- .trysetpath()
  if (!pathwasset) {
    packageStartupMessage("\nWarning: could not find data repository at any of\n\n",
                          paste(.possiblepaths()[["default.datadir"]], collapse = "\n"), sep = "\n\n")
    
    packageStartupMessage("Consider setting the option for your system\n")
    packageStartupMessage('For example: options(default.datadir = "', gsub("\\\\", "/", normalizePath("/myrepository/data", mustWork = FALSE)), '")', '\n', sep = "")
    
  }
}
