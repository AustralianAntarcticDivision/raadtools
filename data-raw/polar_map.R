pmap <- rnaturalearth::ne_coastline(scale = 10)
pmap <- pmap[coordinates(rgeos::gCentroid(pmap, byid = TRUE))[,2] < 0, ]
files <- icefiles()
files$size <- unlist(future_map(files$fullname, fs::file_size))

bad_nsidc <- as.integer(as.Date(files$date[files$size < 40000]))

pmap <- raadtools:::pmap
usethis::use_data( pmap, bad_nsidc, internal = TRUE)
