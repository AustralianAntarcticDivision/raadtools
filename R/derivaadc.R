
##' Read data from polar climatological and other summary environmental data
##'
##'
##' Derived data are read from files managed by \code{\link{derivaadcfiles}}.
##'
##' @param products choice of products, see \code{\link{derivaadcproducts}} for available products
##' @param xylim spatial extents to crop from source data, can be anything accepted by \code{\link[terra]{ext}}
##' @param returnfiles ignore options and just return the file names
##' @param ... passed on, primarily for \code{filename}
##' @export
##' @return \code{SpatRaster}
##' @seealso \code{\link{derivaadcfiles}} for details on the repository of
##' data files
##' @references \url{http://data.aad.gov.au/aadc/metadata/metadata.cfm?entry_id=Polar_Environmental_Data}
##' @examples
##' \dontrun{
##' prods <- c("bathymetry","chl_summer_climatology")
##' x <- readderivaadc(prods)
##' }
readderivaadc <- function(products,
                    xylim = NULL,
                    returnfiles = FALSE, ...) {

  ## get file names and dates and full path
  files <- derivaadcfiles(products = products, ...)
  if (returnfiles) return(files)

  ## check that files are available
  if (!all(file.exists(files$fullname))) {
      stop("files missing: ", paste(files$fullname[!file.exists(files$fullname)],collapse=", "))
  }

  ## projection and grid size for the Southern Hemisphere
  prj  <- "+proj=longlat +datum=WGS84"

  nfiles <- nrow(files)
  r <- vector("list", nfiles)
  ## loop over file indices
  for (ifile in seq_len(nfiles)) {
    r[[ifile]] <- crop_if_needed(.rast_nc(files$fullname[ifile]), xylim)
  }
  r <- if (nfiles > 1) terra::rast(r) else r[[1L]]

  ## these files carry no CRS, so this is set rather than filled in
  terra::crs(r) <- prj
  ## no terra::ext(r) <- terra::ext(-180, 180, -80, -30)
  names(r) <- gsub("\\.nc$","",basename(files$file)) ## drop .nc extension from names
  .write_if_filename(r, ...)
}



#' List all derived data products available through \code{\link{readderivaadc}}
#'
#' @return character vector of product names
#' @seealso \code{\link{readderivaadc}} and \code{\link{derivaadcfiles}}
#' @export derivaadcproducts
derivaadcproducts <- function() {
    ftx <- .allfilelist()
    cfiles <- grep("webdav.data.aad.gov.au/data/environmental/derived/antarctic/netcdf/.*?nc$", ftx, value = TRUE)
    tolower(gsub("\\.nc$","",basename(cfiles)))
}


##' Load metadata and location of files of polar climatological and other summary environmental data
##'
##' This function loads the latest cache of stored files for
##' these products, which are available from \url{http://webdav.data.aad.gov.au/data/environmental/derived/antarctic/}
##' @param products which derived product
##' @param ... reserved for future use, currently ignored
##' @export
##' @return data.frame of \code{file} and \code{date}
#' @seealso \code{\link{readderivaadc}} and \code{\link{derivaadcproducts}}
#' @examples 
#' (prods <- derivaadcproducts())
#' readderivaadc(sample(prods, 1L))
derivaadcfiles <- function(products, ...) {

    if (missing(products)) stop("must specify products")
    datadir <- getOption("default.datadir")
    ## list all files
    ftx <- .allfilelist()
    cfiles <- grep("webdav.data.aad.gov.au/data/environmental/derived/antarctic/netcdf/.*?nc$", ftx, value = TRUE)
    if (length(cfiles) < 1) stop("no files found")
    allproducts <- tolower(gsub("\\.nc$","",basename(cfiles)))
    products <- sapply(tolower(products),function(z)match.arg(z,allproducts))

    idx <- sapply(products,function(z)which(allproducts==z))
    cfiles <- cfiles[idx]

    data.frame(file = gsub(paste0(datadir, "/"), "", cfiles), fullname = cfiles, stringsAsFactors = FALSE)
}
