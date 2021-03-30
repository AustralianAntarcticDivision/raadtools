#remotes::install_github("ropensci/antanym")

library(antanym)
g <- an_read(cache = "session")

s <- rbind(an_filter(g, query = "^Mawson Station"), 
      an_filter(g, query = "^Casey Station"), 
      an_filter(g, query = "^Davis Station"))[, c("place_name", "longitude", "latitude")]
library(raadtools)
xy <- rgdal::project(cbind(s$longitude, s$latitude), projection(readice()))
stations <- sp::SpatialPointsDataFrame(sp::SpatialPoints(xy, proj4string = sp::CRS(projection(readice()))), s)

usethis::use_data(stations)
