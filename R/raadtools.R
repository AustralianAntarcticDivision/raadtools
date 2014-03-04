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
    a <- list(default.datadir =  c(
                       "//aad.gov.au/files/AADC/Scientific_Data/Data/gridded/data",
                       "/Volumes/files/data",
              "/mnt/raadtools",
              "H:/environmental_data/new/data",
              "//147.66.8.89/gridded/data"))

    ##if (Sys.info()["nodename"] == "ICT-42618") a$default.datadir <- c("E:/repo/data", a$default.datadir)
    a
}
.trysetpath <- function() {
    possibles <- .possiblepaths()[["default.datadir"]]
    success <- FALSE
    existing <- getOption("default.datadir")
    if (!is.null(existing)) {
        possibles <- c(existing, possibles)
    }
    for (i in seq_along(possibles)) {
        fi <- file.info(possibles[i])
        if (!is.na(fi$isdir) & fi$isdir) {
            options(default.datadir = possibles[i])
            success <- TRUE
            break;
        }
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



##' Extract cell values from a given data source by point coordinates and times.
##'
##' This function reads data values from a datasource, one of "oisst",
##' "aviso" and "nsidc". The \code{Query} must be a data.frame with
##' 3-columns of longitude, latitude and date/date-time.
##' @title extractxyt
##' @rdname raadtools-deprecated
##' @param datasource name of the data source to extract from
##' @param Query data.frame of 3-columns, longitude,latitude,date-time
##' @param ... arguments passed to the read functions
##' @seealso Read functions \code{\link{readsst}} ("oisst"),
##' \code{\link{readcurr}} ("aviso"), \code{\link{readice}} ("nsidc").
##' @return numeric vector, one for each row of \code{Query}
##' @export
extractxyt <- function(datasource, Query, ...) {
    .Deprecated("extract")
    ## Query MUST be a 3 column data.frame of long/lat points
    xy <- as.matrix(Query[,1:2])
    date <- timedateFrom(Query[,3])
    if (all(is.na(date))) stop("no datetimes are non-missing")
    Query <- SpatialPointsDataFrame(SpatialPoints(xy, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")), data.frame(time = date), match.ID = FALSE)

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
    ##dtimetest <- 4
##    if (all(dtime > dtimetest)) stop(sprintf("no data file within %.1f days of %s", dtimetest))
    ##if (any(dtime > dtimetest)) {
    ##  warning(sprintf("%i input dates have no corresponding data file within %f days of available files", sum(dtime > dtimetest), dtimetest))
  ##    windex <- windex[dtime <= dtimetest]
    ##}

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
            ##cat("cn:\n")
            ##print(cn)
        } else {
            extracteddata[thisindex] <- extract(d0, cn[thisindex])
        }
        ## this part is fine
        ##print(getZ(d0))
        ##cat("ij:\n")
        ##print(ij)
        ##cat("thisindex:\n")
        ##print(thisindex)
        ##cat("extracteddata:\n")
        ##print(extracteddata)
    }

    extracteddata
}



##' SST colours
##'
##' @title SST colours
##' @param x a vector of data values or a single number
##' @param palette logical, if \code{TRUE} return a list with matching colours and values
##' @param alpha value in 0,1 to specify opacity
##' @references Derived from \url{"http://oceancolor.gsfc.nasa.gov/DOCS/palette_sst.txt}.
##' @return colours, palette, or function, see Details
##' @export
sst.pal <- function(x, palette = FALSE, alpha = 1) {

    ##pal <- read.table("http://oceancolor.gsfc.nasa.gov/DOCS/palette_sst.txt", header = TRUE, colClasses = "integer", comment.char = "")
    ##cols <- rgb(pal[,2], pal[,3], pal[,4], maxColorValue = 255)
    ##dput(cols)
    breaks <- seq(-2, 46, length = 256)
    cols <- c("#5B0A76", "#63098B", "#7007AB", "#7C07CA", "#8207DF", "#8007EA",
"#7807EE", "#6E07EE", "#6407EF", "#5807EF", "#4907EF", "#3607EF",
"#2208EE", "#1208EC", "#0B08E9", "#0808E4", "#0808E0", "#0808DD",
"#0808D9", "#0808D4", "#0808CF", "#0808C9", "#0808C4", "#0808BF",
"#0808B9", "#0808B3", "#0808AD", "#0808A6", "#08089F", "#080899",
"#080B93", "#08108B", "#081782", "#081E79", "#082672", "#082E6D",
"#08366A", "#083E68", "#084668", "#084E68", "#08566A", "#085D6B",
"#08626D", "#086470", "#086372", "#086275", "#08617A", "#086282",
"#08658B", "#086893", "#086C98", "#08719B", "#08769D", "#087B9E",
"#08809F", "#08859F", "#0888A0", "#088CA2", "#0891A3", "#0896A5",
"#089CA6", "#08A1A7", "#08A6A9", "#08ACAC", "#08B1B1", "#08B6B6",
"#08BBBB", "#08C0C0", "#08C5C5", "#08C8C8", "#08CCCC", "#08D1D1",
"#08D5D5", "#08D8D8", "#08DBDB", "#08DEDE", "#08E1E1", "#08E3E3",
"#08E6E6", "#08E9E9", "#08EBEB", "#08EDEC", "#08EEEA", "#08EEE5",
"#08EEDF", "#08EDD8", "#08EBD1", "#08EACA", "#08E8C0", "#08E7B2",
"#08E6A2", "#08E593", "#08E385", "#08E27B", "#08E074", "#08DD72",
"#08DB72", "#08D873", "#08D575", "#08D176", "#08CC76", "#08C775",
"#08C173", "#08BC70", "#08B86B", "#08B563", "#08B25B", "#08AF54",
"#08AC51", "#08A950", "#08A450", "#089F50", "#089950", "#08944F",
"#088F4F", "#088A4F", "#08864E", "#08844B", "#088646", "#08893F",
"#088C38", "#088C30", "#088B25", "#098A18", "#0E8B0E", "#188D09",
"#259208", "#2F9708", "#349C08", "#37A108", "#39A608", "#3CAC08",
"#41B108", "#46B608", "#4BBB08", "#50C008", "#55C408", "#59C708",
"#5FC908", "#67CC08", "#6ECE08", "#76D108", "#7ED308", "#86D608",
"#8ED908", "#97DB08", "#A0DE08", "#A9E108", "#B3E308", "#BBE508",
"#C3E608", "#CDE608", "#D7E608", "#DFE508", "#E2E308", "#E1E008",
"#E0DD08", "#E0D908", "#E0D308", "#E0CD08", "#E0C608", "#E0BF08",
"#E0B908", "#E0B408", "#E0AE08", "#E0A608", "#E09F08", "#E09708",
"#E08F08", "#E08708", "#E07F08", "#E07708", "#E06F08", "#E06708",
"#E05F08", "#E05908", "#E05408", "#E04E08", "#DF4608", "#DF3F08",
"#DF3708", "#DE2E08", "#DD2508", "#DB1C08", "#D81308", "#D50C08",
"#D10908", "#CC0808", "#C80808", "#C40808", "#BE0808", "#B60808",
"#AF0808", "#A70808", "#9F0808", "#970808", "#8F0808", "#870808",
"#7F0808", "#780808", "#730808", "#6E0808", "#680808", "#6C0D0D",
"#701313", "#721616", "#731A1A", "#761E1E", "#772221", "#792626",
"#7B2929", "#7D2D2D", "#7F3131", "#803435", "#823838", "#843C3C",
"#86403F", "#884343", "#8A4746", "#8C4B4A", "#8E4F4E", "#8F5252",
"#915555", "#93595A", "#955D5D", "#966161", "#986464", "#9A6868",
"#9C6B6C", "#9E706F", "#A07373", "#A27777", "#A47B7B", "#A57E7E",
"#A88282", "#A98686", "#AA8A89", "#AD8D8D", "#AF9191", "#B09595",
"#B29999", "#B59C9C", "#B69F9F", "#B7A3A3", "#B9A7A7", "#BCAAAB",
"#BEAEAE", "#BFB2B2", "#C1B6B6", "#C3B9B9", "#C5BDBD", "#C7C0C1",
"#C8C5C5", "#CAC8C9", "#CCCCCC", "#000000")

    hexalpha <- as.hexmode(round(255 * alpha))
    if (nchar(hexalpha) == 1L) hexalpha <- paste(rep(hexalpha, 2L), collapse = "")
    cols <- paste0(cols, hexalpha)

     if (palette) return(list(breaks = breaks, cols = cols))

    if (missing(x)) return(colorRampPalette(cols))

    if (length(x) == 1L) {
        return(paste0(colorRampPalette(cols)(x), hexalpha))
    } else {
        return(cols[findInterval(x, breaks)])
    }

}



##' Ocean colour palette for chlorophyll-a.
##'
##' Flexible control of the chlorophyll-a palette. If \code{x} is a
##' single number, the function returns that many colours evenly
##' spaced from the palette. If \code{x} is a vector of multiple
##' values the palette is queried for colours matching those values,
##' and these are returned. If \code{x} is missing and \code{palette}
##' is \code{FALSE} then a function is returned that will generate n
##' evenly spaced colours from the palette, as per
##' \code{\link{colorRampPalette}}.
##' @title Ocean colour colours for chlorophyll-a.
##' @param x a vector of data values or a single number
##' @param palette logical, if \code{TRUE} return a list with matching colours and values
##' @param alpha value in 0,1 to specify opacity
##' @references Derived from \url{http://oceancolor.gsfc.nasa.gov/DOCS/palette_chl_etc.txt}.
##' @return colours, palette, or function, see Details
##' @export
##' @examples
##' \dontrun{
##' chl <- readchla(xylim = c(100, 110, -50, -40))
##' ## just get a small number of evenly space colours
##' plot(chl, col = chl.pal(10))
##' ## store the full palette and work with values and colours
##' pal <- chl.pal()
##' ## the standard full palette
##' plot(chl, breaks = pal$breaks, col = pal$cols)
##' ## a custom set of values with matching colours
##' plot(chl, col = chl.pal(pal$breaks[seq(1, length(pal$breaks), length = 10)]))
##' ## any number of colours stored as a function
##' myfun <- chl.pal()
##' plot(chl, col = myfun(18))
##' ## just n colours
##' plot(chl, col = chl.pal(18))
##' }
chl.pal <- function(x, palette = FALSE, alpha = 1) {

    ##pal <- read.table("http://oceancolor.gsfc.nasa.gov/DOCS/palette_chl_etc.txt", header = TRUE, colClasses = "integer", comment.char = "")
    ##cols <- rgb(pal[,2], pal[,3], pal[,4], maxColorValue = 255)
    ##dput(cols)
    breaks <-  c(0, exp(round(seq(-4.6, 4.1, length = 255), digits = 2)))
    cols <- c("#000000", "#90006F", "#8D0072", "#8A0075", "#870078", "#84007B",
"#81007E", "#7E0081", "#7B0084", "#780087", "#75008A", "#72008D",
"#6F0090", "#6C0093", "#690096", "#660099", "#63009C", "#60009F",
"#5D00A2", "#5A00A5", "#5700A8", "#5400AB", "#5100AE", "#4E00B1",
"#4B00B4", "#4800B7", "#4500BA", "#4200BD", "#3F00C0", "#3C00C3",
"#3900C6", "#3600C9", "#3300CC", "#3000CF", "#2D00D2", "#2A00D5",
"#2700D8", "#2400DB", "#2100DE", "#1E00E1", "#1B00E4", "#1800E7",
"#1500EA", "#1200ED", "#0F00F0", "#0C00F3", "#0900F6", "#0600F9",
"#0000FC", "#0000FF", "#0005FF", "#000AFF", "#0010FF", "#0015FF",
"#001AFF", "#0020FF", "#0025FF", "#002AFF", "#0030FF", "#0035FF",
"#003AFF", "#0040FF", "#0045FF", "#004AFF", "#0050FF", "#0055FF",
"#005AFF", "#0060FF", "#0065FF", "#006AFF", "#0070FF", "#0075FF",
"#007AFF", "#0080FF", "#0085FF", "#008AFF", "#0090FF", "#0095FF",
"#009AFF", "#00A0FF", "#00A5FF", "#00AAFF", "#00B0FF", "#00B5FF",
"#00BAFF", "#00C0FF", "#00C5FF", "#00CAFF", "#00D0FF", "#00D5FF",
"#00DAFF", "#00E0FF", "#00E5FF", "#00EAFF", "#00F0FF", "#00F5FF",
"#00FAFF", "#00FFFF", "#00FFF7", "#00FFEF", "#00FFE7", "#00FFDF",
"#00FFD7", "#00FFCF", "#00FFC7", "#00FFBF", "#00FFB7", "#00FFAF",
"#00FFA7", "#00FF9F", "#00FF97", "#00FF8F", "#00FF87", "#00FF7F",
"#00FF77", "#00FF6F", "#00FF67", "#00FF5F", "#00FF57", "#00FF4F",
"#00FF47", "#00FF3F", "#00FF37", "#00FF2F", "#00FF27", "#00FF1F",
"#00FF17", "#00FF0F", "#00FF00", "#08FF00", "#10FF00", "#18FF00",
"#20FF00", "#28FF00", "#30FF00", "#38FF00", "#40FF00", "#48FF00",
"#50FF00", "#58FF00", "#60FF00", "#68FF00", "#70FF00", "#78FF00",
"#80FF00", "#88FF00", "#90FF00", "#98FF00", "#A0FF00", "#A8FF00",
"#B0FF00", "#B8FF00", "#C0FF00", "#C8FF00", "#D0FF00", "#D8FF00",
"#E0FF00", "#E8FF00", "#F0FF00", "#F8FF00", "#FFFF00", "#FFFB00",
"#FFF700", "#FFF300", "#FFEF00", "#FFEB00", "#FFE700", "#FFE300",
"#FFDF00", "#FFDB00", "#FFD700", "#FFD300", "#FFCF00", "#FFCB00",
"#FFC700", "#FFC300", "#FFBF00", "#FFBB00", "#FFB700", "#FFB300",
"#FFAF00", "#FFAB00", "#FFA700", "#FFA300", "#FF9F00", "#FF9B00",
"#FF9700", "#FF9300", "#FF8F00", "#FF8B00", "#FF8700", "#FF8300",
"#FF7F00", "#FF7B00", "#FF7700", "#FF7300", "#FF6F00", "#FF6B00",
"#FF6700", "#FF6300", "#FF5F00", "#FF5B00", "#FF5700", "#FF5300",
"#FF4F00", "#FF4B00", "#FF4700", "#FF4300", "#FF3F00", "#FF3B00",
"#FF3700", "#FF3300", "#FF2F00", "#FF2B00", "#FF2700", "#FF2300",
"#FF1F00", "#FF1B00", "#FF1700", "#FF1300", "#FF0F00", "#FF0B00",
"#FF0700", "#FF0300", "#FF0000", "#FA0000", "#F50000", "#F00000",
"#EB0000", "#E60000", "#E10000", "#DC0000", "#D70000", "#D20000",
"#CD0000", "#C80000", "#C30000", "#BE0000", "#B90000", "#B40000",
"#AF0000", "#AA0000", "#A50000", "#A00000", "#9B0000", "#960000",
"#910000", "#8C0000", "#870000", "#820000", "#7D0000", "#780000",
"#730000", "#6E0000", "#690000", "#000000")

        hexalpha <- as.hexmode(round(255 * alpha))
    if (nchar(hexalpha) == 1L) hexalpha <- paste(rep(hexalpha, 2L), collapse = "")
    cols <- paste0(cols, hexalpha)

    if (palette) return(list(breaks = breaks, cols = cols))
    if (missing(x)) return(colorRampPalette(cols))

    if (length(x) == 1L) {
        return(paste0(colorRampPalette(cols)(x), hexalpha))
    } else {
        return(cols[findInterval(x, breaks)])
    }

}













##' Read the \enc{Sallée}{Sallee} Mixed layer depth climatology.
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



##' Load definitions of administrative regions.
##'
##' The following maps are available, with modifier "LL" for
##' unprojected versions (modified for the dateline wrap).
##'
##' \describe{
##' \item{sectors}{The basic Southern Ocean sectors, each other layer derives from these}
##' \item{ssrus}{The CCAMLR SSRU boundaries}
##' \item{habitatAssessFull}{The sectors plus physical regions}
##' \item{habitatAsses}{The sectors plus physical regions, with non-ocean removed}}
##' @title administrative polygons
##' @param map name of layer to load, see Details
##' @param ... ignored
##' @return \code{SpatialPolygonsDataFrame}
##' @export
adminmap <- function(map = c("sectors", "ssrus",
                     "habitatAssess", "habitatAssessLL",
                     "habitatAssessFull", "habitatAssessFullLL"
                     ), ...) {
    map <- match.arg(map)
    res <- switch(map,
                  sectors = .admin("SectorAreas_PS"),
                  ssrus = .admin("ssru"),
                  habitatAssess = .admin("BioregionCLIP"),
                  habitatAssessLL = .admin("BioregionCLIP_Longlat"),
                  habitatAssessFull = .admin("BioregionORIG"),
                  habitatAssessFullLL = .admin("BioregionORIG_Longlat"),
                  )
    res
}

.admin <- function(layer = c("SectorAreas_PS", "ssru", "BioregionORIG_Longlat", "BioregionCLIP", "BioregionCLIP_Longlat", "BioregionORIG"), fromCache = TRUE) {
    datapath <- "\\\\aad.gov.au/files/ERM/Projects/HabitatAssessment/output_layers/Sectors"

    cachepath <- file.path(getOption("default.datadir"), "cache", "vector_cache")

  layer = match.arg(layer)

  if (fromCache & !is.null(cachepath) & file.exists(cachepath)) {
    f <- file.path(cachepath, grep(layer, list.files(cachepath), value = TRUE)[1])
       load(f)
    return(get(layer))
  }

  layerpath <- datapath


  if(require(rgdal)) {
      readOGR(layerpath, layer)
  } else {
    stop(sprintf("cannot read layer %s from %s", layer, layerpath))
  }

}

.loadcache_admin <- function(layers =  c("SectorAreas_PS", "BioregionORIG_Longlat", "BioregionCLIP", "BioregionCLIP_Longlat", "BioregionORIG")) {

    cachepath <- file.path(getOption("default.datadir"), "cache", "vector_cache")
    if (!file.exists(cachepath)) stop(sprintf("cachepath %s not available"))
    for (lay in layers) {
        print(lay)
        objectname <- lay
        assign(objectname, .admin(lay, fromCache = FALSE))
        fname <- file.path(cachepath, sprintf("%s.Rdata", lay))
        save(list = objectname, file = fname)
        rm(list = objectname)
    }

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



##' Data frame all of all available fast ice files.
##'
##' A data frame with file, date, fullname
##' @title fast ice files
##' @param datadir data repository path
##' @return data frame
##' @export
fasticefiles <- function(datadir = getOption("default.datadir")) {
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
##' @param ... other arguments, ignored
##' @return RasterBrick with 1 for fast ice pixels, 0 for other, NA for land mask
##' @references \url{http://data.aad.gov.au/aadc/metadata/metadata.cfm?entry_id=modis_20day_fast_ice}
##' @export
##' @examples r <- readfastice(c("2002-02-10", "2002-03-03"))
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
    r <- if (nfiles > 1) brick(stack(r)) else r[[1L]]
    names(r) <- sprintf("fastice_%s", format(files$date, "%Y%m%d"))

    setZ(r, files$date)

}

##' Load file names and dates of Arrigo production data.
##'
##' These are 8 day estimates from MODIS and SeaWiFS satellite data,
##' original NASA algorithm.
##' @title Arrigo production files
##' @return  data.frame of file names and dates
##' @export
prodfiles <- function() {
    datadir <- getOption("default.datadir")
    dirpath <- file.path(datadir, "prod", "Arrigo", "8d")
    fromCache <- TRUE
    if (fromCache) {
        load(file.path(datadir, "cache", "prodfiles.Rdata"))
        pfiles$fullname <- file.path(datadir, pfiles$file)
        return(pfiles)
    }
    fs <- list.files(pattern = "prod\\.bin$", dirpath, full.names = TRUE)
    dates <- timedateFrom(as.Date(basename(fs), "%Y%j"))


    pfiles <- data.frame(file = gsub("^/", "", gsub(datadir, "", fs)), date = dates, stringsAsFactors = FALSE)
    save(pfiles, file = file.path(datadir, "cache", "prodfiles.Rdata"))
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
##' @param ... ignored
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
    r <- brick(stack(r))
    names(r) <- sprintf("prod_%s", format(files$date, "%Y%m%d"))

    setZ(r, files$date)
}



##' Load file names and dates of AVISO SSH/SSHA data
##'
##' A data.frame of file names and dates
##' @title AVISO sea surface height / anomaly files
##' @param ssha logical value, return absolute (SSH) or relative (SSHA anomaly) values
##' @seealso \code{\link{readssh}}
##' @return data.frame of file names and dates
##' @export
sshfiles <- function(ssha = FALSE) {
    datadir = getOption("default.datadir")
    product <- if(ssha) "ssha" else "ssh"
    data.source = file.path(datadir, product, "aviso", "upd", "7d")
    cfiles <- list.files(data.source, pattern = ".nc$", full.names = TRUE)
    datepart <- sapply(strsplit(basename(cfiles), "_"), function(x) x[length(x)-1])
    currentdates <- timedateFrom(as.Date(strptime(datepart, "%Y%m%d")))
   cfs <- data.frame(file = gsub(paste(datadir, "/", sep = ""), "", cfiles), date = currentdates,
                     fullname = cfiles, stringsAsFactors = FALSE)
       ## look at these bad boys

    ##696 //aad.gov.au/files/AADC/Scientific_Data/Data/gridded/data/ssh/aviso/upd/7d/nrt_global_merged_madt_h_20130320_20130320_20130326.nc 2013-03-20 11:00:00
    ##697 //aad.gov.au/files/AADC/Scientific_Data/Data/gridded/data/ssh/aviso/upd/7d/nrt_global_merged_madt_h_20130327_20130327_20130402.nc 2013-03-27 11:00:00
    ##698 //aad.gov.au/files/AADC/Scientific_Data/Data/gridded/data/ssh/aviso/upd/7d/nrt_global_merged_madt_h_20130403_20130403_20130406.nc 2013-04-03 11:00:00
    ##699 //aad.gov.au/files/AADC/Scientific_Data/Data/gridded/data/ssh/aviso/upd/7d/nrt_global_merged_madt_h_20130403_20130403_20130409.nc 2013-04-03 11:00:00
    ##700 //aad.gov.au/files/AADC/Scientific_Data/Data/gridded/data/ssh/aviso/upd/7d/nrt_global_merged_madt_h_20130410_20130410_20130416.nc 2013-04-10 10:00:00

        cfs[diff(cfs$date) > 0, ]

}


##' Sea surface height/anomaly
##'
##' Details
##' @title read SSH/A
##' @param date date or dates of data to read, see Details
##' @param time.resolution time resolution to read
##' @param xylim spatial extents to crop from source data, can be anything accepted by \code{\link[raster]{extent}}, see Details
##' @param lon180 defaults to TRUE, to "rotate" Pacific view [0, 360] data to Atlantic view [-180, 180]
##' components, in degrees (0 north, 90 east, 180 south, 270 west)
##' @param ssha logical, to optionally return anomaly or height
##' @param returnfiles ignore options and just return the file names and dates
##' @param verbose print messages on progress etc.
##' @param ... reserved for future use, currently ignored
##' @export
##' @return data.frame
readssh <- function (date, time.resolution = "weekly",
    xylim = NULL, lon180 = TRUE, ssha = FALSE,
    returnfiles = FALSE, verbose = TRUE, ...)
{
     read0 <- function(x, varname) {
         xtreme <- 20037508
         ytreme <- 16925422
         x <- flip(flip(t(raster(x, varname = varname, stopIfNotEqualSpaced=FALSE)), direction = "y"),
             direction = "x")
         x[x > 9999] <- NA
         extent(x) <- extent(0, xtreme * 2, -ytreme, ytreme)
         projection(x) <- "+proj=merc +ellps=WGS84 +over"
         x
     }
    datadir = getOption("default.datadir")
    time.resolution <- match.arg(time.resolution)

    files <- sshfiles(ssha = ssha)
    if (returnfiles)
        return(files)
    if (missing(date)) date <- min(files$date)
     date <- timedateFrom(date)
    ##findex <- .processDates(date, files$date, time.resolution)
     files <- .processFiles(date, files, time.resolution)

    cropit <- FALSE
    if (!is.null(xylim)) {
        cropit <- TRUE
        cropext <- extent(xylim)
    }
    nfiles <- nrow(findex)
    r <- vector("list", nfiles)
    for (ifile in seq_len(nfiles)) {
        r0 <- read0(files$fullname[ifile], varname = "Grid_0001")
        if (lon180)
            r0 <- suppressWarnings(rotate(r0))
        if (cropit)
            r0 <- crop(r0, cropext)
        r[[ifile]] <- r0
    }
    r <- if (nfiles > 1) brick(stack(r)) else r[[1L]]
    setZ(r, files$date)

}



##' NCEP2 wind files
##'
##' Files containing NCEP2 wind vector data
##' @title Files containing NCEP2 wind vector data
##' @param data.source ignored, reserved for future use
##' @param time.resolution  time resolution data to read, daily only for now
##' @return \code{data.frame} of file names and dates
##' @export
windfiles <-
function(data.source = "", time.resolution = c("daily")) {
      datadir <- getOption("default.datadir")
      time.resolution <- match.arg(time.resolution)
##      fromCache <- TRUE
     fromCache <- TRUE
      if (fromCache) {
          load(file.path(datadir, "cache", sprintf("%s_windfiles.Rdata", time.resolution)))
          wf$ufullname <- file.path(datadir,  wf$ufile)
          wf$vfullname <- file.path(datadir,  wf$vfile)
          return(wf)
      }


     cfiles <- list.files(file.path(datadir, "wind", "ncep2", time.resolution), pattern = ".nc$", full.names = TRUE)
     cfiles <- file.path("wind", "ncep2", time.resolution, basename(cfiles))
     ufiles <- grep("uwnd", cfiles, value = TRUE)
     vfiles <- grep("vwnd", cfiles, value = TRUE)

     ##datepart <- sapply(strsplit(basename(cfiles), "_"), function(x) x[length(x)-1])
     dates <- as.POSIXlt(as.Date(basename(ufiles), "uwnd.10m.gauss.%Y"))
     dates$mday <- 1
     dates$mon <- 0
     dates <- as.POSIXct(dates)
     wf <- data.frame(ufile = ufiles, vfile = vfiles, date = dates, stringsAsFactors = FALSE)
      wfU <- .expandFileDateList(file.path(datadir, wf$ufile))
      wfV <- .expandFileDateList(file.path(datadir, wf$vfile))
      wf <- data.frame(ufile = gsub("^/", "", gsub(datadir, "", wfU$file)), vfile = gsub("^/", "", gsub(datadir, "", wfV$file)), date = wfU$date, band = wfU$band, stringsAsFactors = FALSE)
     save(wf, file = file.path(datadir, "cache", sprintf("%s_windfiles.Rdata", time.resolution)))
     wf

}

##' Read wind
##'
##' Read wind data
##' @title title1
##' @param date date or dates of data to read, see Details
##' @param time.resolution time resolution to read
##' @param magonly return just the magnitude from the U and V
##' components
##' @param dironly return just the direction from the U and V, in degrees N=0, E=90, S=180, W=270
##' @param returnfiles ignore options and just return the file names and dates
##' @param xylim crop
##' @param lon180 Pacific or Atlantic
##' @param ... ignored
##' @return raster object
##' @examples
##' \dontrun{
##'  dts <- seq(as.Date("2000-01-01"), by = "1 days", length = 350)
##'  library(animation)
##'  ani.start(ani.width = 800, ani.height = 800)
##'  for (i in seq_along(dts)) {
##'     x <- readwind(dts[i]);
##'     if (i == 1L) crds <- coordinates(x[[1]])
##'     plot(sqrt(x[[1]]^2 + x[[2]]^2), xlim = c(40, 180), ylim = c(-90, -20));
##' x1 <- crds[,1]
##' y1 <- crds[,2]
##' x2 <- crds[,1] + values(x[[1]])/4
##' y2 <- crds[,2] + values(x[[2]])/4
##'     arrows(x1, y1, x2, y2, length = 0.06);
##'     plot(m, add = TRUE)
##' }
##' ani.stop()
##'
##'
##'
##' }
##' @export
readwind <- function(date, time.resolution = c("daily"), xylim = NULL, lon180 = TRUE,
                     magonly = FALSE, dironly = FALSE, returnfiles = FALSE, ...) {

     time.resolution <- match.arg(time.resolution)
    files <- windfiles()
    if (returnfiles) return(files)



     if (missing(date)) date <- min(files$date)
     date <- timedateFrom(date)
   ## findex <- .processDates(date, files$date, time.resolution)
     files <- .processFiles(date, files, time.resolution)


     nfiles <- nrow(files)
     if (!(magonly | dironly) & nfiles > 1L) {
         warning("only one time index can be read at once unless magonly or dironly is TRUE")
         files <- files[1,]
     }


      if (!(magonly | dironly))
        rasterfun <- function(x1, x2) {
            x <- brick(x1, x2)
            names(x) <- c("U", "V")
            x
        }
    if (magonly)
        rasterfun <- function(x1, x2) sqrt(x1 * x1 + x2 * x2)
    if (dironly)
        rasterfun <- function(x1, x2) (90 - atan2(x2, x1) * 180/pi)%%360
    cropit <- FALSE
    if (!is.null(xylim)) {
        cropit <- TRUE
        cropext <- extent(xylim)
    }

    r <- vector("list", nfiles)
    for (ifile in seq_len(nfiles)) {
        r1 <- raster(files$ufullname[ifile], band = files$band[ifile])
        r2 <- raster(files$vfullname[ifile], band = files$band[ifile])
        r0 <- rasterfun(r1, r2)
        if (lon180)     r0 <- suppressWarnings(rotate(r0))
        if (cropit) r0 <- crop(r0, cropext)
        r[[ifile]] <- r0

    }
    r <- brick(stack(r))
    if (magonly | dironly)  {
        r <- setZ(r, files$date)
        names(r) <- sprintf("wind_%s", format(files$date, "%Y%m%d"))
    } else {

        r <- setZ(r, rep(files$date, 2L))
        names(r) <- sprintf("%swind_%s", c("U", "V"), format(files$date, "%Y%m%d"))
    }


     ## get alignment right (put this in raster?)
     extent(r) <- extent(c(xmin(r) + res(r)[1]/2, xmax(r) + res(r)[1]/2,
                           ymin(r), ymax(r)))

     r

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
##' @param ... reserved for future use, currently ignored
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
  ##if (lon180) rtemplate <- rotate(rtemplate)

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
    ##if (lon180) r0 <- rotate(r0)
    if(cropit) r0 <- crop(r0, cropext)
    ## r0[r0 < -2] <- NA
    r[[ifile]] <- r0
    ##if (verbose & ifile %% 10L == 0L) .progressreport(ifile, nfiles)
  }

  if (nfiles > 1)
    r <- brick(stack(r))
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
##' @return data.frame
##' @export
chlafiles <- function(time.resolution = c("weekly", "monthly"),
                      product = c("johnson", "oceancolor")) {
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

##' @rdname readtopo
##' @export
topofile <- function(topo = c("gebco_08", "ibcso",
                              "etopo1", "etopo2",
                              "kerguelen", "george_v_terre_adelie",
                              "smith_sandwell"),
                     polar = FALSE,
                     lon180 = TRUE, ...) {

    datadir = getOption("default.datadir")
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
    topopath <- file.path(datadir, "bathymetry", topo,
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



##' Functions to provide topographic (bathymetry and/or topography) data.
##'
##' Use \code{readtopo} (or its alias \code{readbathy}) to read data
##' from the chosen data set. The function \code{topofile} is used to
##' find the full file name.
##' \code{xylim} is expected to be consistent with \code{lon180}
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
##' @title Topography data
##' @name readtopo
##' @aliases topofile readbathy
##' @param topo Data source, see Details.
##' @param lon180 Flag for returning data in Atlantic [-180, 180] rather than Pacific [0, 360] view.
##' @param xylim spatial extents to crop from source data, can be anything accepted by \code{\link[raster]{extent}}, see Details
##' @param polar Flag for returning the polar version of the IBCSO data.
##' @param ... reserved for future use, ignored currently
##' @return
##' \describe{
##' \item{}{\code{topofile} returns a character string of the full path to a file name}
##' \item{}{\code{readtopo} and \code{readbathy} return the requested data as a RasterLayer (these are aliases)}
##' }
##' @examples
##' ibcso <- readtopo("ibcso", polar = TRUE)
##' @export
readtopo <- function(topo = c("gebco_08", "ibcso",
                              "etopo1", "etopo2",
                              "kerguelen", "george_v_terre_adelie",
                              "smith_sandwell"),
                     polar = FALSE,
                     lon180 = TRUE,
                     xylim = NULL,
                     ...) {
    topo <- match.arg(topo)

    if (!lon180 & topo %in% c("geboc_08", "ibcso", "etopo1", "etopo2")) {
        tfile <- topofile(topo = topo, polar = FALSE, ...)
        if (is.null(xylim)) res <- rotate(raster(tfile))
    } else {
        tfile <- topofile(topo = topo, polar = polar, lon180 = lon180, ...)
        res <- raster(tfile)
    }

    if (topo == "etopo2") projection(res) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
    if (!is.null(xylim)) res <- crop(res, xylim)
    res
}
##' @rdname readtopo
##' @export
readbathy <- readtopo

##' Load file names and dates of OISST sea surface temperature data
##'
##' A data frame of file names and datres
##' @title OISST sea surface temperature files
##' @param fromcache load from cache?
##' @param time.resolution time resolution read
##' @param ... reserved for future use
##' @return data.frame of file names and dates
##' @export
sstfiles <- function(fromcache = TRUE, time.resolution = c("daily", "monthly")) {
    datadir <- getOption("default.datadir")
    time.resolution <- match.arg(time.resolution)
    if (fromcache) {
        load(file.path(datadir, "cache", sprintf("sstfiles_%s.Rdata", time.resolution)))
        sstf$fullname <- file.path(datadir, sstf$file)

        return(sstf)
    }

    if (time.resolution == "daily") {
        dirpath <- file.path(datadir, "sst", "OI-daily-v2", "daily")
        fs <- list.files(dirpath, pattern = "\\.nc$", recursive = TRUE, full.names = TRUE)

        ## flakey!!!!
        fsstrings <- as.Date(substr(basename(fs), 15, 22), "%Y%m%d")

        dates <- timedateFrom(as.Date(fsstrings, "%Y%m%d"))

    }
    if (time.resolution == "monthly") {
        dirpath <- file.path(datadir, "sst", "oimonth_v2", "monthly")
        fs <- list.files(dirpath, full.names = TRUE)
        bname <- gsub(".gz", "", basename(fs))
        dates <- timedateFrom(as.POSIXct(strptime(paste0(bname, "01"), "oiv2mon.%Y%m%d"), tz = "GMT"))
    }
    sstf <- data.frame(files = gsub("^/", "", gsub(datadir, "", fs)), date = dates, stringsAsFactors = FALSE)[order(dates), ]
    save(sstf, file = file.path(datadir, "cache", sprintf("sstfiles_%s.Rdata", time.resolution)))

    sstf

}

.progressreport <- function(current, finish) {
        cat(paste(rep("\b", 16), collapse = ""))
        cat(sprintf("%6d of %6d", current, finish));
        flush.console()
        invisible(NULL)
}
##' Read OISST sea surface temperature data from daily files
##'
##' SST data read from files managed by
##' \code{\link{sstfiles}}. Dates are matched to file names by finding
##' the nearest match in time within a short duration. If \code{date}
##' is greater than length 1 then the sorted set of unique matches is
##' returned.
##' @param date date or dates of data to read, see Details
##' @param time.resolution time resoution data to read, daily or monthly
##' @param varname variable to return from the data files, default is
##' "sst" or "anom", "err", "ice"
##' @param xylim spatial extents to crop from source data, can be anything accepted by \code{\link[raster]{extent}}, see Details
##' @param lon180 defaults to TRUE, to "rotate" Pacific view [0, 360] data to Atlantic view [-180, 180]
##' @param returnfiles ignore options and just return the file names and dates
##' @param verbose print messages on progress etc.
##' @param ... reserved for future use, currently ignored
##' @export
##' @return \code{\link[raster]{raster}} object
##' @seealso \code{\link{icefiles}} for details on the repository of
##' data files, \code{\link[raster]{raster}} for the return value
##' @examples
##' \dontrun{
##' ## read one time slice and plot it up in preparation for reading a time series
##' d <- readsst()
##' plot(d)
##' ## this step is interactive, draw a boundary on the plot
##' ext <- drawExtent()
##' ## these can be created manually with xmin,xmax,ymin,ymax
##' ## ext <- extent(-100, 150, -75, -30)
##' ## now read a big chunk of data for this small region
##' dts <- seq(as.Date("2001-01-03"), by = "1 week", length = 100)
##' sst <- readsst(dts, xylim = ext)
##' }
readsst <- function(date, time.resolution = c("daily", "monthly"), varname = c("sst", "anom", "err", "ice"),
                    xylim = NULL,
                    lon180 = TRUE,
                    returnfiles = FALSE,
                    verbose = TRUE,
                    ...) {

    datadir <- getOption("default.datadir")
    time.resolution <- match.arg(time.resolution)
    varname <- match.arg(varname)

    if (! varname == "sst" & time.resolution == "monthly") stop("only sst available for monthly data")
    files <- sstfiles(time.resolution = time.resolution)
    if (returnfiles) return(files)

    if (missing(date)) date <- min(files$date)
    date <- timedateFrom(date)
     ## from this point one, we don't care about the input "date" - this is our index into all files and that's what we use
    ##findex <- .processDates(date, files$date, time.resolution)
    ##date <- files$date[findex]
    files <- .processFiles(date, files, time.resolution)

    ## process xylim
    cropit <- FALSE
    if (!is.null(xylim)) {
        cropit <- TRUE
        cropext <- extent(xylim)
        ##rtemplate <- crop(rtemplate, cropext)
    }

    nfiles <- nrow(files)
    r <- vector("list", nfiles)
    if (time.resolution == "daily") {
        rtemplate <- raster(files$fullname[1L], varname = varname)
        if (lon180) rtemplate <- rotate(rtemplate)
        for (ifile in seq_len(nfiles)) {
            r0 <- raster(files$fullname[ifile], varname = varname)
            if (lon180) r0 <- rotate(r0)
            if(cropit) r0 <- crop(r0, cropext)
            r0[r0 < -2] <- NA
            r[[ifile]] <- r0
        }
    ## monthly
    } else {
        landmask <- readBin(file.path(datadir, "sst", "oimonth_v2", "lstags.onedeg.dat"), "numeric", size = 4, n = 360 * 180, endian = "big")
        for (ifile in seq_len(nfiles)) {
             fname <- files$fullname[ifile]
             con <- gzfile(fname, open = "rb")
             version <- readBin(con, "integer", size = 4, n = 1, endian = "big")
             date <-  readBin(con, "integer", size = 4, n = 7, endian = "big")
             d <- readBin(con, "numeric", size = 4, n = 360 * 180, endian = "big")
             close(con)
              d[d > 500] <- NA
             d[landmask < 1] <- NA
             sst <- flip(raster(t(matrix(d,   360, 180)),
                                xmn = 0, xmx = 360, ymn = -90, ymx = 90,
                                crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"), "y")
             if (lon180) sst <- rotate(sst)
             if (cropit) sst <- crop(sst,cropext)
             r[[ifile]] <- sst
        }



    }

         if (nfiles > 1) r <- brick(stack(r)) else r <- r[[1L]]
          names(r) <- paste("sst", time.resolution, format(files$date, "%Y%m%d"), sep = "_")
             r <- setZ(r, files$date)

             return(r)


}


##' Load file names and dates of AVISO current data
##'
##' A data.frame of file names and dates
##' @title AVISO ocean currents files
##' @seealso \code{\link{readcurr}}
##' @return data.frame of file names and dates
##' @export
currentsfiles <- function() {
    datadir = getOption("default.datadir")
    cachefile <- file.path(datadir, "cache", sprintf("currentsfiles_weekly.Rdata"))

    fromCache <- TRUE
    if (fromCache) {
       load(cachefile)
       cfs$fullname <- file.path(datadir, cfs$file)
       return(cfs)
    }
    data.source = file.path(datadir, "current", "aviso", "upd", "7d")
    cfiles <- list.files(data.source, pattern = ".nc$", full.names = TRUE)
    datepart <- sapply(strsplit(basename(cfiles), "_"), function(x) x[length(x)-1])
    currentdates <- timedateFrom(as.Date(strptime(datepart, "%Y%m%d")))

    cfs <- data.frame(file = gsub("^/", "", gsub(datadir, "", cfiles)), date = currentdates, stringsAsFactors = FALSE)
    cfs <- cfs[diff(cfs$date) > 0, ]
    save(cfs, file = cachefile)
    cfs

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





##' Read AVISO ocean current data from weekly files
##'
##' Current data is read from files managed by
##' \code{\link{currentsfiles}}. Dates are matched to file names by
##' finding the nearest match in time within a short duration. By
##' default only one time step is returned with both U and V
##' components. Multiple dates can be returned for magnitude or
##' direction, U or V only.
##'
##' This is the "SSALTO/DUACS - DT Geostrophic Velocities - Up-to-date Global Processing". See References.
##'
##' @param date date or dates of data to read, see Details
##' @param time.resolution time resolution to read
## @param setNA mask zero and values greater than 100 as NA
## @param rescale rescale values from integer range?
##' @param magonly return just the magnitude from the U and V
##' components
##' @param dironly return just the direction from the U and V, in degrees N=0, E=90, S=180, W=270
##' @param uonly return just the U component of velocity
##' @param vonly return just the V component of velocity
##' @param lon180 defaults to TRUE, to "rotate" Pacific view [0, 360] data to Atlantic view [-180, 180]
##' components, in degrees (0 north, 90 east, 180 south, 270 west)
##' @param xylim spatial extents to crop from source data, can be anything accepted by \code{\link[raster]{extent}}, see Details
##' @param returnfiles ignore options and just return the file names and dates
##' @param verbose print messages on progress etc.
##' @param ... reserved for future use, currently ignored
##' @export
##' @note These data are stored in a Mercator projection on Pacific
##' view \[0, 360\], the default behaviour is to reset this to Atlantic
##' view \[-180, 180\] with \code{lon180}. The Mercator projection is
##' preserved, see \code{\link[raster]{projectRaster}} and
##' \code{\link[raster]{resample}} for transformation methods.
##'
##' \code{xylim} is expected to be consistent with the source
##' data itself (which is not necessarily in longitude/latitude) and
##' with \code{lon180}, if in doubt first read a single time slice,
##' plot it and draw an \code{\link[raster]{extent}}.
##' @return \code{\link[raster]{raster}} object with the "U"
##' (horizontal/X) and "V" (vertical/Y) components of velocity in
##' cm/s. Setting either of the (mutually exclusive) \code{magonly}
##' and \code{dironly} arguments returns the magnitude (in cm) or
##' direction (in degrees relative to North) of the velocity vectors.
##' @seealso \code{\link{icefiles}} for details on the repository of
##' data files, \code{\link[raster]{raster}} for the return value
# imports should not be necessary here
##' @importFrom raster t flip atan2
##' @export
##' @references \url{http://www.aviso.oceanobs.com/en/data/products/sea-surface-height-products/global/index.html}
##' @examples
##' \dontrun{
##' ## read a single time slice, and plot the directions [0,360) as an image with arrows
##' x <- readcurr(dironly = TRUE)
##' ## get a local extent for a zoom plot
##' e <- extent(projectExtent(raster(extent(130, 150, -50, -30), crs = "+proj=longlat"), projection(x)))
##' x1 <- crop(readcurr(), e)
##' crds <- coordinates(x1)
##' scale <- 2000
##' plot(crop(x, e))
##' x1 <- crds[,1]
##' y1 <- crds[,2]
##' x2 <- crds[,1] + values(x1[["U"]]) * scale
##' y2 <- crds[,2] + values(x1[["V"]]) * scale
##' arrows(x1, y1, x2, y2, length = 0.03)
##' }
readcurr <- function(date,
                     time.resolution = "weekly",
                     xylim = NULL,
                     ##setNA = TRUE,
                     ##rescale = TRUE,
                     magonly = FALSE,
                     dironly = FALSE,

                     uonly = FALSE,
                     vonly = FALSE,

                     lon180 = TRUE,
                     returnfiles = FALSE,
                     verbose = TRUE,
                     ...) {

read0 <- function(x, varname) {
        xtreme <- 20037508
        ytreme <- 16925422
        x <- flip(flip(t(raster(x, varname = varname, stopIfNotEqualSpaced=FALSE)), direction = "y"),
            direction = "x")
        extent(x) <- extent(0, xtreme * 2, -ytreme, ytreme)
        projection(x) <- "+proj=merc +ellps=WGS84 +over"
        x
    }

    datadir = getOption("default.datadir")
    time.resolution <- match.arg(time.resolution)
    if ((magonly + dironly + uonly + vonly) > 1) stop("only one of magonly, dironly, uonly or vonly may be used, exiting")

    files <- currentsfiles()
    if (returnfiles) return(files)


    if (missing(date)) date <- min(files$date)

date <- timedateFrom(date)
    ##findex <- .processDates(date, files$date, time.resolution)
files <- .processFiles(date, files, time.resolution)

nfiles <- nrow(files)
    ## prevent reading more than one unless mag/dironly
    if (nfiles > 1L & !magonly & !dironly & !uonly & !vonly) {
        files <- files[1,]
        warning("only one time step can be read at once unless magonly, dironly uonly or vonly is TRUE")
    }

    if (!(magonly | dironly)) rasterfun <- function(x1, x2) {x <- brick(x1, x2); names(x) <- c("U", "V");x}
    if (magonly) rasterfun <- function(x1, x2) sqrt(x1 * x1 + x2 *x2)
    if (dironly) rasterfun <- function(x1, x2) (90 - atan2(x2, x1) * 180/pi) %% 360

    if (!(magonly | dironly)) {
        if (uonly) rasterfun <- function(x1, x2) x1
        if (vonly) rasterfun <- function(x1, x2) x2
    }

       ## process xylim
    cropit <- FALSE
    if (!is.null(xylim)) {
        cropit <- TRUE
        cropext <- extent(xylim)

    }



    r <- vector("list", nfiles) ##brick(rasterfun(r1, r2), nl = length(findex))
    for (ifile in seq_len(nfiles)) {
       r1 <- read0(files$fullname[ifile], varname = "Grid_0001")
        r2 <- read0(files$fullname[ifile], varname = "Grid_0002")
        ##r1 <- .readAVISO(files$fullname[findex[ifile]], justone = FALSE)
        ##r <- setValues(r, values(rasterfun(r1, r2)), layer = i)
        r0 <- rasterfun(r1, r2)
        if (lon180) r0 <- suppressWarnings(rotate(r0))
        if(cropit) r0 <- crop(r0, cropext)
        r[[ifile]] <- r0
        ##if (verbose & ifile %% 10L == 0L) .progressreport(ifile, nfiles)

    }

    r <- brick(stack(r))
     if (magonly | dironly | uonly | vonly) r <- setZ(r, files$date) else r <- setZ(r, rep(files$date, 2L))
##    if (lon180) r <- suppressWarnings(rotate(r))
    return(r)

}

##' Read data from sea ice data products.
##'
##'
##' Sea ice data is read from files managed by \code{\link{icefiles}}.
##'
##' Currently available products are
##'
##' \describe{
##' \item{'nsidc'}{daily or monthly NSIDC concentration data for the Southern Hemisphere, processed by the SMMR/SSMI NASA Team}
##' \item{'ssmi'}{daily SSMI concentration data for the Southern Hemisphere}
##' }
##'
##' Dates are matched to file names by finding the nearest match in
##' time within a short duration. If \code{date} is greater than
##' length 1 then the sorted set of unique matches is returned.
##' @param date date or dates of data to read, see Details
##' @param time.resolution time resoution data to read, daily or monthly
##' @param product choice of sea ice product, see Details
##' @param xylim spatial extents to crop from source data, can be anything accepted by \code{\link[raster]{extent}}, see Details
##' @param setNA mask zero and values greater than 100 as NA
##' @param rescale rescale values from integer range?
##' @param debug ignore data request and simply report on what would be returned after processing arguments
##' @param returnfiles ignore options and just return the file names and dates
##' @param verbose print messages on progress etc.
##' @param ... reserved for future use, currently ignored
##' @export
##' @return \code{\link[raster]{raster}} object
##' @seealso \code{\link{icefiles}} for details on the repository of
##' data files, \code{\link[raster]{raster}} for the return value
readice <- function(date,
                    time.resolution = c("daily", "monthly"),
                    product = c("nsidc", "ssmi"),
                    xylim = NULL,
                    setNA = TRUE, rescale = TRUE,

                    debug = FALSE,
                    verbose = TRUE,
                    returnfiles = FALSE, ...) {
    datadir = getOption("default.datadir")
    time.resolution <- match.arg(time.resolution)

    product <- match.arg(product)
    ## get file names and dates and full path
    files <- .loadfiles(product, time.resolution = time.resolution)
    ##files$fullname <- file.path(datadir, files$file)
    if (returnfiles) return(files)

    if (missing(date)) date <- min(files$date)
    date <- timedateFrom(date)
    ## from this point one, we don't care about the input "date" - this is our index into all files and that's what we use
    files <- .processDates(date, files, time.resolution)


    ## NSIDC projection and grid size for the Southern Hemisphere
    stersouth <-  "+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"

    ## modify based on dataproduct
    dims <- switch(product,
                   nsidc = c(316L, 332L),
                   ssmi = c(632L, 664L))
    res <-  switch(product,
                   nsidc = c(25000, 25000),
                   ssmi = c(12500, 12500))

    rtemplate <- raster(GridTopology(c(-3937500, -3937500), res, dims))


    ## process xylim
    cropit <- FALSE
    if (!is.null(xylim)) {
        cropit <- TRUE
        cropext <- extent(xylim)
        ##rtemplate <- crop(rtemplate, cropext)
    }

    nfiles <- nrow(files)

    r <- vector("list", nfiles)

    ## note that this can be replaced by a direct raster(file) once the package
    ## gets updated (post raster_2.1-49, October 2013)
    .readNSIDC <- function(fname) {

        con <- file(fname, open = "rb")
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
      raster(t(matrix(dat, dims[1])), template = rtemplate)

    }

    .readSSMI <- function(fname) {
        x <- raster(fname, varname = "concentration")
        x <- flip(x, "y")
        if (!setNA) {
            x[is.na(x)] <- -127
        } else {
            x[x > 100 | x < 1] <- NA
        }

        extent(x) <- extent(rtemplate)
        x

    }

    ## loop over file indices
    for (ifile in seq_len(nfiles)) {


    r0 <- switch(product,
                nsidc = .readNSIDC(files$fullname[ifile]),
                ssmi = .readSSMI(files$fullname[ifile]))

      if (cropit) r0 <- crop(r0, cropext)
      r[[ifile]] <- r0
      ##if (verbose & ifile %% 10L == 0L) .progressreport(ifile, nfiles)
  }
    if (nfiles > 1) r <- brick(stack(r)) else r <- r[[1L]]
    projection(r) <- stersouth
    names(r) <- basename(files$file)
    r <- setZ(r, files$date
    r
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

       findex <- .processDates(date, files$date, time.resolution)
       date <- files$date[findex]
       proj <- "+proj=merc +ellps=WGS84"
       if (!lon180) proj <- paste(proj, "+over")
       ##extreme.points <- as.matrix(expand.grid(c(-180, 180), c(-82, -30.24627)))
       ##epoints.merc <- project(extreme.points, proj)
##       epoints.merc <- structure(c(-20037508, 20037508, -20037508,
  ##                                 20037508, -16925422, -16925422, -3513725, -3513725), .Dim = c(4L, 2L))
       epoints.merc <- structure(c(0, 2 * 20037508, 0,
                                   2 * 20037508, -16925422, -16925422, -3513725, -3513725), .Dim = c(4L, 2L))

l <- vector("list", length(findex))
       for (i in seq_along(l)) {
           r0 <- raster(file, band = findex[i], stopIfNotEqualSpaced=FALSE)
           extent(r0) <- extent(bbox(epoints.merc))
           projection(r0) <- proj
           e <- new("Extent", xmin = 0, xmax = 2 * 20037508, ymin = -11087823.8567493 , ymax = -3513725)
           if (!is.null(xylim)) r0<- crop(r0, extent(xylim)) else r0 <- crop(r0, e)

           if (lon180)  r0 <- suppressWarnings(rotate(r0))
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



       r <- if (length(l) > 1) setZ(brick(stack(l)), date) else setZ(l[[1L]], date)
        ## lots of cells are wasted with nodata

       r
}





#r <- readice("1995-01-01", dataproduct = "ssmi")
#r1 <- readice("1995-01-01")




##' Load metadata and location of files of sea ice data products.
##'
##' This function loads the latest cache of stored files for
##' ice products.
##' @param time.resolution daily or monthly files?
##' @param product choice of sea ice product, see \code{\link{readice}}
##' @export
##' @examples
##' \dontrun{
##' icf <- icefiles(time.resolution = "monthly")
##' icf[which.min((as.Date("1995-01-01") + runif(1, -4000, 4000)) - as.Date(icf$date), ]
##' }
##' @return data.frame of \code{file} and \code{date}
icefiles <- function(time.resolution = c("daily", "monthly"), product = c("nsidc", "ssmi")) {
    time.resolution <- match.arg(time.resolution)
    product <- match.arg(product)
    datadir <- getOption("default.datadir")
    ## TODO, need a system of tokens . . .
    if (product == "nsidc") id_token <- time.resolution else id_token <- product
    files <- NULL
    load(file.path(datadir, "cache", sprintf("%s_icefiles.Rdata", id_token)))
    files$fullname <- file.path(datadir, files$file)
    files
}


.updateicefiles <- function(datadir = getOption("default.datadir")) {


    for (time.resolution in c("daily", "monthly")) {
        subpath <- file.path("seaice", "smmr_ssmi_nasateam", time.resolution)
        fpath <- file.path(datadir, subpath)
        fs <- list.files(fpath , recursive = TRUE, pattern = "s.bin$", full.names = FALSE)

        if (!length(fs) > 0) {
            warning(sprintf("no files found for %s at %s", time.resolution, fpath))
            next;
        }
        datepart <- sapply(strsplit(basename(fs), "_"), "[", 2)
        if(time.resolution == "monthly") datepart <- paste0(datepart, "01")

        icdates <- as.POSIXct(strptime(datepart, "%Y%m%d"), tz = "GMT")

        files <- data.frame(file = file.path(subpath, fs), date = icdates, stringsAsFactors = FALSE)

        ## take the "last" duplicated   (should be lexicographically f0n > f0m)
        bad <- rev(duplicated(rev(icdates)))

        files <- files[!bad, ]
        files <- files[order(files$date), ]

        fpath <- file.path(getOption("default.datadir"),"cache", sprintf("%s_icefiles.Rdata", time.resolution))
        save(files, file = fpath)
        print(sprintf("saved %s", fpath))
    }

    ## ssmi
    dataproduct <- "ssmi"
    subpath <- file.path("seaice", "ssmi", "ifremer", "antarctic", "daily")
    fs <- list.files(file.path(datadir, subpath), recursive = TRUE, pattern = ".nc$", full.names = FALSE)
    datepart <- gsub(".nc$", "", basename(fs))
    icdates <- as.POSIXct(strptime(datepart, "%Y%m%d"), tz = "GMT")
    files <- data.frame(file = file.path(subpath, fs), date = icdates, stringsAsFactors = FALSE)
    files <- files[order(icdates), ]
    fpath <- file.path(datadir, "cache", sprintf("%s_icefiles.Rdata", dataproduct))
    save(files, file = fpath)
    print(sprintf("saved %s", fpath))
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
