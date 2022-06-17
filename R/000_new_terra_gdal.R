.do_terra <- function() {
  getOption("raadtools.do_terra")
}

.be_terra <- function() {
  getOption("raadtools.be_terra")
}
.set_terra <- function() {
  options(raadtools.be_terra = TRUE)
  options(raadtools.do_terra = TRUE)
  invisible(NULL)
}

## take a terra object and crop it or use y instead
## can pass in a projection string for when it's wrong or missing
.terra <- function(x, y, projection = NULL) {
  if (is.numeric(y) || inherits(y, "Extent")) {
    x <-  terra::crop(x, terra::intersect(terra::ext(x), terra::ext(y)))
  }
  if (inherits(y, "SpatExtent")) {
    x <-  terra::crop(x, y)
  }
  if (inherits(y, "BasicRaster")) {
    x <- terra::rast(y[[1L]])
  }
  
  if (inherits(y, "SpatRaster")) {
    x <- y
  }
  if (!inherits(x, "SpatRaster")) stop()
  if (!is.null(projection) && is.na(terra::crs(x))) x <- terra::set.crs(x, projection)
  if (is.na(terra::crs(x))) message("no CRS set for template")
  x
}
.read_oisst_daily <- function(x, y  = NULL, ..., subds = c("sst", "anom", "err", "ice"), lon180 = TRUE) {
  subds <- match.arg(subds)
  files <- raadtools:::.processFiles(x, raadfiles::oisst_daily_files(), "daily")
  if (is.null(y)) {
    ## no template we just go native
   out <- terra::rast(files$fullname, subds = subds) 

   if (lon180) out <- terra::rotate(out)

   return(out)
  } else {
    tmp0 <- terra::rast(terra::rast(files$fullname[1L], subds = subds))
    if (lon180) tmp0 <- terra::rotate(tmp0)
    
    template <- .terra(tmp0, y)
  
    
    return(terra::rast(lapply(files$fullname, function(filename) terra::project(terra::rast(filename, subds = subds), template))))
  }
}

.read_oisst_monthly <- function(x, y = NULL, ..., lon180 = TRUE) {
  files <- raadtools:::.processFiles(x, raadfiles::oisst_monthly_files(), "monthly")
  if (is.null(y)) {
    ## no template we just go native
    out <- terra::rast(files$fullname[1L], lyrs = files$band) 
    
    if (lon180) out <- terra::rotate(out)
    
    return(out)
  } else {
    tmp0 <- terra::rast(terra::rast(files$fullname[1L], lyrs = files$band))
    if (lon180) tmp0 <- terra::rotate(tmp0)
    
    template <- .terra(tmp0, y)
    
    mask <- 
    return(terra::rast(lapply(split(files, files$band), function(filedf) 
      terra::project(terra::rast(filedf$fullname, lyrs = filedf$band), template))))
  }
}



.read_nsidc_daily <- function(x, y = NULL) {
  files <- raadtools:::.processFiles(x, raadfiles::nsidc_south_daily_files(), "daily") 
  files$south <- files$fullname
  files$fullname <- files$root <- NULL
  files <- dplyr::inner_join(files, dplyr::transmute(raadfiles::nsidc_north_daily_files(), north = fullname, date), "date")
  
  stemplate <- '<VRTDataset rasterXSize="316" rasterYSize="332"> 
    <VRTRasterBand dataType="Byte" band="1" subClass="VRTRawRasterBand"> 
    <SourceFilename relativetoVRT="1">%s</SourceFilename> 
    <ImageOffset>300</ImageOffset> 
    <PixelOffset>1</PixelOffset> 
    <LineOffset>316</LineOffset> 
    </VRTRasterBand> 
    <SRS>PROJCRS[\"WGS 84 / NSIDC Sea Ice Polar Stereographic South\",\n    BASEGEOGCRS[\"WGS 84\",\n        ENSEMBLE[\"World Geodetic System 1984 ensemble\",\n            MEMBER[\"World Geodetic System 1984 (Transit)\"],\n            MEMBER[\"World Geodetic System 1984 (G730)\"],\n            MEMBER[\"World Geodetic System 1984 (G873)\"],\n            MEMBER[\"World Geodetic System 1984 (G1150)\"],\n            MEMBER[\"World Geodetic System 1984 (G1674)\"],\n            MEMBER[\"World Geodetic System 1984 (G1762)\"],\n            MEMBER[\"World Geodetic System 1984 (G2139)\"],\n            ELLIPSOID[\"WGS 84\",6378137,298.257223563,\n                LENGTHUNIT[\"metre\",1]],\n            ENSEMBLEACCURACY[2.0]],\n        PRIMEM[\"Greenwich\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433]],\n        ID[\"EPSG\",4326]],\n    CONVERSION[\"US NSIDC Sea Ice polar stereographic south\",\n        METHOD[\"Polar Stereographic (variant B)\",\n            ID[\"EPSG\",9829]],\n        PARAMETER[\"Latitude of standard parallel\",-70,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8832]],\n        PARAMETER[\"Longitude of origin\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8833]],\n        PARAMETER[\"False easting\",0,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8806]],\n        PARAMETER[\"False northing\",0,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8807]]],\n    CS[Cartesian,2],\n        AXIS[\"easting (X)\",north,\n            MERIDIAN[90,\n                ANGLEUNIT[\"degree\",0.0174532925199433]],\n            ORDER[1],\n            LENGTHUNIT[\"metre\",1]],\n        AXIS[\"northing (Y)\",north,\n            MERIDIAN[0,\n                ANGLEUNIT[\"degree\",0.0174532925199433]],\n            ORDER[2],\n            LENGTHUNIT[\"metre\",1]],\n    USAGE[\n        SCOPE[\"Polar research.\"],\n        AREA[\"Southern hemisphere - south of 60°S onshore and offshore - Antarctica.\"],\n        BBOX[-90,-180,-60,180]],\n    ID[\"EPSG\",3976]]</SRS>
    <GeoTransform> -3.9500000000000000e+06,  2.5000000000000000e+04,  0.0000000000000000e+00,  4.3500000000000000e+06,  0.0000000000000000e+00, -2.5000000000000000e+04</GeoTransform>
    </VRTDataset>'
  
  ntemplate <- '<VRTDataset rasterXSize="304" rasterYSize="448"> 
  <VRTRasterBand dataType="Byte" band="1" subClass="VRTRawRasterBand"> 
    <SourceFilename relativetoVRT="1">%s</SourceFilename> 
    <ImageOffset>300</ImageOffset> 
    <PixelOffset>1</PixelOffset> 
    <LineOffset>304</LineOffset> 
  </VRTRasterBand> 
  <SRS>PROJCRS[\"WGS 84 / NSIDC Sea Ice Polar Stereographic North\",\n    BASEGEOGCRS[\"WGS 84\",\n        ENSEMBLE[\"World Geodetic System 1984 ensemble\",\n            MEMBER[\"World Geodetic System 1984 (Transit)\"],\n            MEMBER[\"World Geodetic System 1984 (G730)\"],\n            MEMBER[\"World Geodetic System 1984 (G873)\"],\n            MEMBER[\"World Geodetic System 1984 (G1150)\"],\n            MEMBER[\"World Geodetic System 1984 (G1674)\"],\n            MEMBER[\"World Geodetic System 1984 (G1762)\"],\n            MEMBER[\"World Geodetic System 1984 (G2139)\"],\n            ELLIPSOID[\"WGS 84\",6378137,298.257223563,\n                LENGTHUNIT[\"metre\",1]],\n            ENSEMBLEACCURACY[2.0]],\n        PRIMEM[\"Greenwich\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433]],\n        ID[\"EPSG\",4326]],\n    CONVERSION[\"US NSIDC Sea Ice polar stereographic north\",\n        METHOD[\"Polar Stereographic (variant B)\",\n            ID[\"EPSG\",9829]],\n        PARAMETER[\"Latitude of standard parallel\",70,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8832]],\n        PARAMETER[\"Longitude of origin\",-45,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8833]],\n        PARAMETER[\"False easting\",0,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8806]],\n        PARAMETER[\"False northing\",0,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8807]]],\n    CS[Cartesian,2],\n        AXIS[\"easting (X)\",south,\n            MERIDIAN[45,\n                ANGLEUNIT[\"degree\",0.0174532925199433]],\n            ORDER[1],\n            LENGTHUNIT[\"metre\",1]],\n        AXIS[\"northing (Y)\",south,\n            MERIDIAN[135,\n                ANGLEUNIT[\"degree\",0.0174532925199433]],\n            ORDER[2],\n            LENGTHUNIT[\"metre\",1]],\n    USAGE[\n        SCOPE[\"Polar research.\"],\n        AREA[\"Northern hemisphere - north of 60°N onshore and offshore, including Arctic.\"],\n        BBOX[60,-180,90,180]],\n    ID[\"EPSG\",3413]]</SRS>
    <GeoTransform> -3.8375000000000000e+06,  2.5000000000000000e+04,  0.0000000000000000e+00,  5.8375000000000000e+06,  0.0000000000000000e+00, -2.5000000000000000e+04</GeoTransform>
 
</VRTDataset>
  '
  files$vrtsouth <- sprintf(stemplate, files$south)
  files$vrtnorth <- sprintf(ntemplate, files$north)
  projection <- "+proj=cass +R=6378137"
  extent <- c(-.5, 0.5, -1, 1) * 20037508
  if (is.null(y)) y <- terra::rast(terra::ext(extent), res = 46276, crs = projection)
  dimension <- dim(y)[2:1]
  mask_nsidc <- function(x) {
    x[x > 250] <- NA
    x/2.5
  }

  #abcd <- 1:1e5  
  # (abcd)[which((20037508/(abcd) ) == as.integer((20037508/(abcd) )))]
  # [1]     1     2     4    23    46    92   433   503   866  1006  1732  2012  9959 11569 19918 23138
  # [17] 39836 46276
  # 
  terra::rast(
    lapply(split(files, 1:nrow(files)), function(x) 
    terra::setValues(y, mask_nsidc(vapour::vapour_warp_raster_dbl(c(x$vrtsouth, x$vrtnorth), 
                                                       extent = extent, dimension = dimension, projection = projection, resample = "bilinear")))))
  
  
}


.read_topo <- function(xylim = NULL) {
  topos <- c("/vsicurl/https://public.services.aad.gov.au/datasets/science/GEBCO_2019_GEOTIFF/GEBCO_2019.tif", 
             "/vsicurl/https://opentopography.s3.sdsc.edu/raster/NASADEM/NASADEM_be.vrt")
  extent <- c(-180, 180, -90, 90)
  dimension <- c(360, 180)
  projection <- "OGC:CRS84"
  if (is.numeric(xylim)) extent <- rep(xylim, length.out = 4L)
  if (inherits(xylim, "Extent")) extent <- c(xylim@xmin, xylim@xmax, xylim@ymin, xylim@ymax)
  if (is.null(xylim)) {
    template <- terra::rast(terra::ext(extent), nrows = dimension[2L], ncols = dimension[1L], crs= projection)
  } else {
    template <- xylim
  }
  fact <- 1
  if (terra::is.lonlat(template)) {
    fact <- 111111.12
  }
  if (min(terra::res(template)) * fact > 30) {
    message("dropping NASADEM")
      topos <- topos[1L]
  }
  extent <- c(terra::xmin(template), terra::xmax(template), 
              terra::ymin(template), terra::ymax(template))
  dimension <- dim(template)[2:1]
  projection <- terra::crs(template)
  #return(template)
  terra::setValues(template, vapour::vapour_warp_raster_dbl(topos, resample = "bilinear", 
                                                            extent = extent, dimension = dimension, 
                                                            projection = projection))
}
