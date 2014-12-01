

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
##' @param ... passed to brick, primarily for \code{filename}
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
  
  time.resolution <- match.arg(time.resolution)
  product <- match.arg(product)
  ## get file names and dates and full path
  files <- .loadfiles(product, time.resolution = time.resolution)
  ##files$fullname <- file.path(datadir, files$file)
  if (returnfiles) return(files)
  if (missing(date)) date <- min(files$date)
  date <- timedateFrom(date)
  files <- .processFiles(date, files, time.resolution)
  
  ## check that files are available
  
  
  ## NSIDC projection and grid size for the Southern Hemisphere
  stersouth <-  "+proj=stere +lat_0=-90 +lat_ts=-70 +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs "
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
  }
  ## TODO need filename for the singleton case
  if (nfiles > 1) r <- brick(stack(r), ...) else r <- r[[1L]]
  
  projection(r) <- stersouth
  names(r) <- basename(files$file)
  setZ(r, files$date)
}




.readice1 <- function(date,
                      time.resolution = c("daily", "monthly"),
                      product = c("nsidc", "ssmi"),
                      xylim = NULL,
                      setNA = TRUE, rescale = TRUE,
                      
                      debug = FALSE,
                      verbose = TRUE,
                      returnfiles = FALSE, ...) {
  
  time.resolution <- match.arg(time.resolution)
  product <- match.arg(product)
  ## get file names and dates and full path
  files <- .loadfiles(product, time.resolution = time.resolution)
  ##files$fullname <- file.path(datadir, files$file)
  if (returnfiles) return(files)
  if (missing(date)) date <- min(files$date)
  date <- timedateFrom(date)
  files <- .processFiles(date, files, time.resolution)
  
  ## check that files are available
  
  
  ## NSIDC projection and grid size for the Southern Hemisphere
  stersouth <-  "+proj=stere +lat_0=-90 +lat_ts=-70 +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs "
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
  }
  ## TODO need filename for the singleton case
  if (nfiles > 1) r <- brick(stack(r), ...) else r <- r[[1L]]
  
  projection(r) <- stersouth
  names(r) <- basename(files$file)
  setZ(r, files$date)
}
