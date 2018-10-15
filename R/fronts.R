

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
##' @param setNA is \code{TRUE} NaN values are replaced with NA
##' @param trim if \code{TRUE} the map is cropped to no contiguous margins of missing data
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
                       setNA = FALSE,
                       trim = TRUE,
                       returnfiles = FALSE, RAT = TRUE, ...) {
  time.resolution <- match.arg(time.resolution)
  ftx <- .allfilelist()
 cfiles <- grep("fronts", ftx, value = TRUE)
  cfiles1 <- grep("ACCfronts.nc", cfiles, value = TRUE)
  if (length(cfiles1) < 1) stop("file ACCfronts.nc not found!")
  cfiles1 <- cfiles1[1]
  product <- match.arg(product)
  wks <- seq(timedateFrom("1992-10-14"), by = "7 days", length = 854)
  ## get file names and dates and full path
  files <- tibble::tibble(fullname = cfiles1,
                      date = wks, band = seq_along(wks))
    if (missing(date)) date <- min(files$date)
  
  date <- timedateFrom(date)
 
  ##frontname <- c("sBdy", "SACCF_S", "SACCF_N", "PF_S", "PF_M", "PF_N", "SAF_S",
  ##          "SAF_M", "SAF_N", "SAZ_S", "SAZ_M", "SAZ_N")
  
  if (returnfiles) return(files)
  
  
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
    r0 <- raster(files$fullname[i], band = files$band[i], stopIfNotEqualSpaced=FALSE)
    extent(r0) <- extent(bbox(epoints.merc))
    projection(r0) <- proj
    e <- new("Extent", xmin = 0, xmax = 2 * 20037508, ymin = -11087823.8567493 , ymax = -3513725)
    if (!is.null(xylim)) r0<- crop(r0, extent(xylim)) else r0 <- crop(r0, e)
    
    if (lon180)  r0 <- suppressWarnings(.rotate(r0))
   
    
    if (setNA) r0[is.nan(r0)] <- NA
    if (RAT) {
      r0 <- ratify(r0)
      rat <- levels(r0)[[1]]
      
      rat <- data.frame(ID = 0:12, name = c("south of sBdy", "between SACCF-S & sBdy", "SACCF-N & SACCF-S",
                                            "PF-S & SACCF-N", "PF-M & PF-S", "PF-N & PF-M", "SAF-S & PF-N",
                                            "SAF-M & SAF-S", "SAF-N & SAF-M", "SAZ-S & SAF-N", "SAZ-M & SAZ-S",
                                            "SAZ-N & SAZ-M", "north of SAZ-N"), stringsAsFactors = FALSE)
      levels(r0) <- rat
    }
    l[[i]] <- r0
  }
  
  
  
  r <- if (length(l) > 1) setZ(brick(stack(l)), files$date) else setZ(l[[1L]], files$date)
  ## lots of cells are wasted with nodata and with float32 becoming 64
  v <- values(r)
  mode(v) <- "integer"
  print(str(v))
  r <- setValues(r, v)
  dataType(r) <- "INT4S"
  if (trim) r <- raster::trim(r)
  r
}

