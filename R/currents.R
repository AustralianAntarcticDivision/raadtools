##' Load file names and dates of AVISO current data
##'
##' A data.frame of file names and dates
##' @title AVISO ocean currents files
##' @param time.resolution time resolution to load
##' @param ... reserved for future use, currently ignored
##' @seealso \code{\link{readcurr}}
##' @return data.frame of file names and dates
##' @export
currentsfiles <- function(time.resolution = c("daily", "weekly"), ...) {
  time.resolution <- match.arg(time.resolution)
  if (time.resolution != "daily") warning("only daily available, no weekly - ignoring 'time.resolution'")
  raadfiles::altimetry_daily_files()
}


# dimensions:
#   time = 1 ;
# lat = 720 ;
# lon = 1440 ;
# nv = 2 ;
# variables:
#   float time(time) ;
# time:long_name = "Time" ;
# time:standard_name = "time" ;
# time:units = "days since 1950-01-01 00:00:00 UTC" ;
# time:calendar = "julian" ;
# time:axis = "T" ;
# float lat(lat) ;
# lat:long_name = "Latitude" ;
# lat:standard_name = "latitude" ;
# lat:units = "degrees_north" ;
# lat:bounds = "lat_bnds" ;
# lat:axis = "Y" ;
# lat:valid_min = -90 ;
# lat:valid_max = 90 ;
# float lat_bnds(nv, lat) ;
# float lon(lon) ;
# lon:long_name = "Longitude" ;
# lon:standard_name = "longitude" ;
# lon:units = "degrees_east" ;
# lon:bounds = "lon_bnds" ;
# lon:axis = "X" ;
# lon:valid_min = 0 ;
# lon:valid_max = 360 ;
# float lon_bnds(nv, lon) ;
# int crs ;
# crs:grid_mapping_name = "latitude_longitude" ;
# crs:semi_major_axis = 6371000 ;
# crs:inverse_flattening = 0 ;
# int nv(nv) ;
# int u(lon, lat, time) ;
# u:_FillValue = -2147483647 ;
# u:long_name = "Absolute geostrophic velocity: zonal component" ;
# u:standard_name = "surface_eastward_geostrophic_sea_water_velocity" ;
# u:units = "m/s" ;
# u:scale_factor = 1e-04 ;
# int v(lon, lat, time) ;
# v:_FillValue = -2147483647 ;
# v:long_name = "Absolute geostrophic velocity: meridian component" ;
# v:standard_name = "surface_northward_geostrophic_sea_water_velocity" ;
# v:units = "m/s" ;
# v:scale_factor = 1e-04 ;
# 
# // global attributes:
#   :cdm_data_type = "Grid" ;
# :title = "DT merged Global Ocean Gridded Absolute Geostrophic Velocities SSALTO/Duacs L4 product" ;
# :summary = "This dataset contains Delayed Time Level-4 absolute geostrophic velocities products from multi-satellite observations over Global Ocean." ;
# :comment = "Surface product; Absolute Geostrophic Velocities" ;
# :time_coverage_resolution = "P1D" ;
# :product_version = "5.0" ;
# :institution = "CNES, CLS" ;
# :project = "SSALTO/DUACS" ;
# :references = "www.aviso.altimetry.fr" ;
# :contact = "aviso@altimetry.fr" ;
# :license = "http://www.aviso.altimetry.fr/fileadmin/documents/data/License_Aviso.pdf" ;
# :platform = "ERS-1, Topex/Poseidon" ;
# :date_created = "2014-02-28 13:07:00" ;
# :history = "2014-02-28 13:07:00:creation" ;
# :Conventions = "CF-1.6" ;
# :standard_name_vocabulary = "http://cf-pcmdi.llnl.gov/documents/cf-standard-names/standard-name-table/12/cf-standard-name-table.html" ;
# :geospatial_lat_min = -90 ;
# :geospatial_lat_max = 90 ;
# :geospatial_lon_min = 0 ;
# :geospatial_lon_max = 360 ;
# :geospatial_vertical_min = "0.0" ;
# :geospatial_vertical_max = "0.0" ;
# :geospatial_lat_units = "degrees_north" ;
# :geospatial_lon_units = "degrees_east" ;
# :geospatial_lat_resolution = 0.25 ;
# :geospatial_lon_resolution = 0.25 ;
