pmap <- rnaturalearth::ne_coastline(scale = 10)
pmap <- pmap[coordinates(rgeos::gCentroid(pmap, byid = TRUE))[,2] < 0, ]
usethis::use_data(pmap, internal = TRUE)
