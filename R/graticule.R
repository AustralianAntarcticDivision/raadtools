##' Create a graticule from an object
##'
##' Using an object as input, this function builds a graticule in the coordinate system provided.
##' @title graticule
##' @param x object to dervived context from, Spatial* or Raster*
##' @param labels ignored
##' @param nsegs number of vertices to include on each line segment
##' @return SpatialLinesDataFrame
##' @export
graticule <- function(x,
                      labels = FALSE,
                      nsegs = 50) {

    if (missing(x)) x <- raster()
    llproj <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"

    projected <- !isLonLat(x)
    if (projected) {
        ext <- extent(projectExtent(x, llproj))
    } else {
        ext <- extent(x)
    }



##
    easts <- pretty(c(xmin(ext), xmax(ext)), n = 8)
    norths <- pretty(c(ymin(ext), ymax(ext)), n = 8)
    easts <- easts[easts >= xmin(ext) & easts <= xmax(ext)]
    norths <- norths[norths >= ymin(ext) & norths <= ymax(ext)]
    eastlist <- vector(mode = "list", length = length(easts))
    for (i in 1:length(easts)) {
        eastlist[[i]] <- Line(cbind(rep(easts[i], nsegs), seq(ymin(ext), ymax(ext), length.out = nsegs)))
    }

    northlist <- vector(mode = "list", length = length(norths))
    for (i in 1:length(norths)) {
        northlist[[i]] <- Line(cbind(seq(xmin(ext), xmax(ext), length.out = nsegs), rep(norths[i], nsegs)))
    }

    grat <- SpatialLinesDataFrame(
        SpatialLines(list(Lines(northlist, "NS"), Lines(eastlist, "EW")), CRS(llproj)),
        data.frame(name = c("northing", "easting"), row.names = c("NS", "EW")))
    if (projected) {
        if (.check_pkg("rgdal")) grat <- spTransform(grat, CRS(projection(x))) else stop("rgdal required for reprojection")
        ##grat <- gIntersection(grat, as(extent(x), "SpatialPolygons"))
    }
    grat
}


.Meridian <- function(n = 50, meridian = 180) {
    llproj <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
    SpatialLinesDataFrame(SpatialLines(list(Lines(list(Line(cbind(rep(meridian, n), seq(-90, 90, length = n)))), "dateline")), CRS(llproj)), data.frame(name = "dateline", row.names = "dateline"))
}

.intersectsMeridians <- function(obj) {

    .check_pkg("rgeos")
    m1 <- .Meridian(meridian = 0)
    m2 <- .Meridian(meridian = 180)
    if (!isLonLat(obj)) {
        .check_pkg("rgdal")
        m1 <- spTransform(m1, CRS(projection(obj)))
        m2 <- spTransform(m2, CRS(projection(obj)))
    }

    c(gIntersects(obj, m1), gIntersects(obj, m2))
}


.check_pkg <- function(pkgname) {
            if (!(pkgname %in% loadedNamespaces())) {
                ns <- try(loadNamespace(pkgname))
                if (isNamespace(ns)) {
                    message(sprintf("[loaded the %s namespace]", pkgname))
                } else {
                    msg <- sprintf(paste("This method requires the %s package",
                                 "but is unable to load the %s namespace",
                                 sep=","), pkgname, pkgname)
                    stop(msg)
                }
            }
            invisible(NULL)
        }
