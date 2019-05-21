## function to find intersections with lines at a given longitude
#' @importFrom rgeos gIntersection
#' @importFrom sp SpatialLines Lines Line coordinates
longint <- function(lines, longitudes, latrange = c(-90, 90), fun = median) {
  latitudes <- rep(NA_real_, length(longitudes))
  for (i in seq_along(longitudes)) {
    line <- SpatialLines(list(Lines(list(Line(cbind(rep(longitudes[i], 2), latrange))), "1")), proj4string = CRS(projection(lines)))
    pts <- try(gIntersection(lines, line))
    if (!inherits(pts, "try-error") & !is.null(pts)) latitudes[i] <- median(coordinates(pts)[,2])
  }
  cbind(longitudes, latitudes)
}

keepOnlyMostComplexLine <- function(x) {
  for (iObj in seq_len(nrow(x))) {
    if (inherits(x, "SpatialLinesDataFrame")) {
      wmax <- which.max(sapply(x[iObj, ]@lines[[1]]@Lines, function(x)
        nrow(x@coords)))
      x@lines[[iObj]]@Lines <- x@lines[[iObj]]@Lines[wmax]
    }
    
  }
  x
}

#' @importFrom  raster projectExtent raster calc rasterToContour isLonLat
#' @importFrom sp  spChFIDs
#' @importFrom maptools spRbind
monthlyIceContours <- function(month, years = NULL, fun = max, 
                               llim = NULL, product = "nsidc", lev = 15, 
                               longlat = TRUE) {
  icf <- icefiles(product = product)
  if (is.null(years)) years <- unique(format(icf$date, "%Y"))
  cl <- vector("list", length(years))
  dummy <- readice(product = product)
  if (!is.null(llim)) {
    ex <- projectExtent(raster(llim, crs = "+proj=longlat +ellps=WGS84"), projection(dummy))
  } else {
    ex <- NULL
  }
  for (iyear in seq_along(years)) {
    thisf <- subset(icf, format(date, "%m") == month & format(date, "%Y") == years[iyear])
    ice <- readice(thisf$date, xylim = ex)
    ice <- calc(ice, max, na.rm = TRUE)
    
    thiscl <-  keepOnlyMostComplexLine(rasterToContour(ice, lev = lev))
    thiscl$year <- years[iyear]
    if (iyear == 1) {
      icelines <- thiscl
    } else {
      icelines <- spRbind(icelines, spChFIDs(thiscl, as.character(iyear)))
    }
  }
  if (longlat & !isLonLat(dummy)) icelines <- spTransform(icelines, "+proj=longlat +ellps=WGS84")
  icelines
}
