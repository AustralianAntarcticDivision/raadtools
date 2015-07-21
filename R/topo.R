.ssbintext <- function() {
  '<VRTDataset rasterXSize="21600" rasterYSize="17280">
  <GeoTransform>0.0, 1855.325, 0.0000000000000000e+000, 15987768, 0.0000000000000000e+000,-1850.436</GeoTransform>
  <SRS>PROJCS["unnamed",
  GEOGCS["Normal Sphere (r=6370997)",
  DATUM["unknown",
  SPHEROID["sphere",6370997,0]],
  PRIMEM["Greenwich",0],
  UNIT["degree",0.0174532925199433]],
  PROJECTION["Mercator_1SP"],
  PARAMETER["central_meridian",0],
  PARAMETER["scale_factor",1],
  PARAMETER["false_easting",0],
  PARAMETER["false_northing",0]]</SRS>
  <Metadata />
  <VRTRasterBand dataType="Int16" band="1" subClass="VRTRawRasterBand">
  <Metadata />
  <SourceFilename relativeToVRT="1">ssbinfile</SourceFilename>
  <SourceBand>1</SourceBand>
  <ImageOffset>0</ImageOffset>
  <PixelOffset>2</PixelOffset>
  <LineOffset>43200</LineOffset>
  <ByteOrder>MSB</ByteOrder>
  </VRTRasterBand>
  </VRTDataset>'
}

.ssatlanticbintext <- function() {
  '<VRTDataset rasterXSize="21600" rasterYSize="17280">
  <SRS>PROJCS["unnamed",
    GEOGCS["Normal Sphere (r=6370997)",
  DATUM["unknown",
  SPHEROID["sphere",6370997,0]],
  PRIMEM["Greenwich",0],
  UNIT["degree",0.0174532925199433]],
  PROJECTION["Mercator_1SP"],
  PARAMETER["central_meridian",0],
  PARAMETER["scale_factor",1],
  PARAMETER["false_easting",0],
  PARAMETER["false_northing",0]]</SRS>
  <GeoTransform>-20037510.000, 1.8553250000000000e+003, 0.0000000000000000e+000, 1.5987768000000000e+007, 0.0000000000000000e+000,-1.8504359999999999e+003</GeoTransform>
  <Metadata />
  <VRTRasterBand dataType="Int16" band="1">
  <Metadata />
  <SimpleSource>
  <SourceFilename relativeToVRT="1">ssvrtfile</SourceFilename>
  <SourceBand>1</SourceBand>
  <SourceProperties RasterXSize="21600" RasterYSize="17280" DataType="Int16" BlockXSize="21600" BlockYSize="1" />
  <SrcRect xOff="10800" yOff="0" xSize="10800" ySize="17280" />
  <DstRect xOff="0" yOff="0" xSize="10800" ySize="17280" />
  </SimpleSource>
  <SimpleSource>
  <SourceFilename relativeToVRT="1">ssvrtfile</SourceFilename>
  <SourceBand>1</SourceBand>
  <SourceProperties RasterXSize="21600" RasterYSize="17280" DataType="Int16" BlockXSize="21600" BlockYSize="1" />
  <SrcRect xOff="0" yOff="0" xSize="10800" ySize="17280" />
  <DstRect xOff="10800" yOff="0" xSize="10800" ySize="17280" />
  </SimpleSource>
  </VRTRasterBand>
  </VRTDataset>'
}


.smithsandwellraw <- function(candidatefiles) {
  tail(sort( grep("/topo_.*\\.img$", candidatefiles, value = TRUE)), 1)
}

.smithsandwellvrt <- function(lon180 = FALSE) {
  binfile <- .smithsandwellraw(allfiles()$fullname)
  vrtfile1 <- file.path(dirname(binfile), ".vrt", gsub(".img$", ".vrt", basename(binfile)))
  vrtfile2 <- file.path(dirname(binfile), ".vrt",gsub(".img$", "atlantic.vrt", basename(binfile)))
                        
  vrtext1 <- gsub("ssbinfile", file.path("..", basename(binfile)), .ssbintext())
  vrtext2 <- gsub("ssvrtfile", basename(vrtfile1), .ssatlanticbintext())
  
  if (!file.exists(vrtfile1)) writeLines(vrtext1, vrtfile1)
  if (!file.exists(vrtfile2)) writeLines(vrtext2, vrtfile2)
  if (lon180) vrtfile2 else vrtfile1
}


##' @rdname readtopo
##' @export
topofile <- function(topo = c("gebco_08", "ibcso",
                              "etopo1", "etopo2",
                              "kerguelen", "george_v_terre_adelie",
                              "smith_sandwell"),
                     polar = FALSE,
                     lon180 = TRUE,
                     ...) {
  
  datadir = getOption("default.datadir")
  topo <- match.arg(topo)
  polarsubdir <- "latlon"
  if (polar) {
    if (topo %in% c("ibcso")) {
      polarsubdir <- "ps71"
    } else {
      warning("no polar version of ", topo, "consider projectRaster(x, crs = '+proj=stere +lat_0=-71', filename = 'mycopy.grd')")
    }
  }
  if (!lon180 & !(topo %in% c("smith_sandwell"))) warning("no Pacific view version available of ", topo)
  
  if (topo == "smith_sandwell") {
    topopath <- .smithsandwellvrt(lon180 = lon180)
  } else {
    
  topopath <- file.path(datadir, "bathymetry", topo,
                        switch(topo,
                               gebco_08 = "gebco_08.tif",
                               ibcso = file.path(polarsubdir, "ibcso_v1_is.tif"),
                               etopo1 = "ETOPO1_Ice_g_gdal.grd",
                               etopo2 = "ETOPO2v2c_f4.nc",
                               kerguelen = "kerg_dem_100m.grd",
                               george_v_terre_adelie = "gvdem100m_v3.nc"
                               ## use the RAW file via GDAL VRT
                               
  ))
  }
  if (!file.exists(topopath)) stop(sprint("expected to find file, but it's not there: ", topopath))
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
##' @aliases readtopo topofile readbathy
##' @param topo Data source, see Details.
##' @param lon180 Flag for returning data in Atlantic [-180, 180] rather than Pacific [0, 360] view.
##' @param xylim spatial extents to crop from source data, can be anything accepted by \code{\link[raster]{extent}}, see Details
##' @param polar Flag for returning the polar version of the IBCSO data.
##' @param returnfiles Return just the relevant file name
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
                     returnfiles = FALSE,
                     ...) {
  topo <- match.arg(topo)
  
  if (!lon180 & topo %in% c("geboc_08", "ibcso", "etopo1", "etopo2")) {
    tfile <- topofile(topo = topo, polar = FALSE, ...)
    if (returnfiles) return(tfile)
    if (is.null(xylim)) res <- .rotate(raster(tfile))
  } else {
    tfile <- topofile(topo = topo, polar = polar, lon180 = lon180, ...)
    if (returnfiles) return(tfile)
    res <- raster(tfile)
  }
  
  if (topo == "etopo2") projection(res) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
  if (topo == "kerguelen") projection(res) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0" 
  if (topo == "george_v_terre_adelie") projection(res) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0" 
  
  
  if (!is.null(xylim)) res <- crop(res, xylim)
  res
}
##' @rdname readtopo
##' @export
readbathy <- readtopo
