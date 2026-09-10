


#' Find topography data files
#'
#' Return the full path, or GDAL data source name, for a topography or
#' bathymetry data set. Use \code{\link{read_topo}} to read the data itself.
#'
#' The available data sets, and what each one is, are listed in the Details
#' section of \code{\link{read_topo}}.
#' @param topo Data source, see \code{\link{read_topo}}.
#' @param polar Flag for returning the polar version of the IBCSO data.
#' @param lon180 Flag for returning data in Atlantic \[-180, 180\] rather than Pacific \[0, 360\] view.
#' @param ... reserved for future use, ignored currently
#' @return character string of the full path to a file name
#' @seealso \code{\link{read_topo}} to read the data
#' @export
#' @importFrom vapour vapour_vrt
topofile <- function(topo = c("gebco_23", "gebco_08",  "ibcso",
                              "etopo1", "etopo2",
                              "kerguelen",
                              "smith_sandwell", "gebco_14", "macrie1100m", "macrie2100m", "cryosat2",
                              "lake_superior",
                              "ramp", "ibcso_is", "ibcso_bed",

                              "rema_1km",
                              "rema_200m",
                              "rema_100m",
                              "rema_8m",
                              "gebco_19", "gebco_21"),
                     polar = FALSE,
                     lon180 = TRUE,
                     ...) {
  topo <- match.arg(topo)
  if (topo == "ibcso") topo <- "ibcso_is" ## ??

  if (topo == "rema_8m") {
    r8m_files <- raadfiles::rema_8m_files()
    ##warning(sprintf("rema_8m is a very large **virtual** raster consisting of many (%i) files on disk,\n beware of making subsets that will pull a lot of data into memory", nrow(r8m_files)))
  }
  allf <- allfiles()

  topopath <-  switch(topo,
                      gebco_23 = raadfiles::gebco23_files()$fullname,
                      smith_sandwell = if (lon180) raadfiles::smith_sandwell_lon180_files()$fullname else raadfiles::smith_sandwell_files()$fullname,
                      gebco_08 = raadfiles::gebco08_files()$fullname,
                      gebco_14 = raadfiles::gebco14_files()$fullname,
                      gebco_19 = raadfiles::gebco19_files()$fullname,
                      gebco_21 = raadfiles::gebco21_files()$fullname,
                      ibcso_is = dplyr::filter(raadfiles::ibcso_files(all = FALSE))$fullname,
                      ibcso_bed = dplyr::filter(raadfiles::ibcso_bed_files(all = FALSE))$fullname,
                      etopo1 = raadfiles::etopo1_files()$fullname,
                      etopo2 = raadfiles::etopo2_files()$fullname,
                      kerguelen = raadfiles::kerguelen_files()$fullname,

                      macrie1100m = raadfiles::macquarie100m_57S_files()$fullname,
                      macrie2100m = raadfiles::macquarie100m_58S_files()$fullname,
                      cryosat2 = raadfiles::cryosat2_files()$fullname,
                      lake_superior = raadfiles::lakesuperior_files()$fullname,
                      ramp = raadfiles::ramp_files()$fullname,
                      rema_100m = raadfiles::rema_100m_files()$fullname[1L],
                      rema_200m = raadfiles::rema_200m_files()$fullname[1L],
                      rema_1km = raadfiles::rema_1km_files()$fullname[1L],
                      rema_8m =   file.path(dirname(dirname(r8m_files$fullname[1])), "rema_mosaic_8m_dem.vrt"))


  if (topo == "lake_superior") {
    topopath <- vapour::vapour_vrt(topopath, extent = c(-92.20042, -83.99958, 45.99958, 49.50042), projection = "OGC:CRS84")

  }
  ## no longer needed we're using the tif from v2
  # if (topo == "ibcso_bed") {
  #   topopath <- vapour::vapour_vrt(topopath, extent = c(  -3333500, 3334000, -3337000, 3333500),
  #                                  projection = "+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs ")
  # 
  # }
  if (topo %in% c("gebco_08", "etopo1", "etopo2")) {
    topopath <- vapour::vapour_vrt(topopath, extent = c(-180, 180, -90, 90), projection = "OGC:CRS84")
  }
  if (topo == "smith_sandwell") {
##    topopath <- vapour::vapour_vrt(topopath, projection = "EPSG:3857")
  }
  if (topo == "cryosat2") {
    topopath <- vapour::vapour_vrt(topopath,  extent = c(-2821000, 2819000, rev(c(-2421000, 2419000)) ),
                                   projection = "+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
  }


  if (length(topopath) < 1 || is.na(topopath) || is.null(topopath)) stop(sprintf("cannot find %s", topo))
  topopath[1L]
}


#' REMA tile index
#'
#' Read the index of Reference Elevation Model of Antarctica (REMA) tiles, as
#' polygon outlines. Use \code{\link{read_topo}} with one of the 'rema_'
#' data sets to read the elevation data itself.
#' @param ... reserved for future use, ignored currently
#' @return SpatVector of REMA tile outlines
#' @seealso \code{\link{read_topo}}
#' @export
read_rema_tiles <- function(...) {
  terra::vect(raadfiles::rema_tile_files(all = FALSE)$fullname[1])
}

