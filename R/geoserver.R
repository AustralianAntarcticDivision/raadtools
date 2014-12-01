# x1 <- "http://services.aad.gov.au/geoserver/esri/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=esri:countries&maxFeatures=50&outputFormat=json"
# 
# ##system(sprintf("ogrinfo %s", x))
# 
# library(rgdal)
# ogrInfo(x1, "OGRGeoJSON")
# 
# d1 <- readOGR(x1, "OGRGeoJSON")
# 
# 
# x2 <- "http://services.aad.gov.au/geoserver/aadc/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=aadc:PLACE_NAMES&maxFeatures=50&outputFormat=json"
# ogrInfo(x2, "OGRGeoJSON")
# d2 <- readOGR(x2, "OGRGeoJSON")
