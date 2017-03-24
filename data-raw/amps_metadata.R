af <- amps_d1files()
x <- system(sprintf("gdalinfo %s ", af$fullname[1]), intern = TRUE)
# ERROR 6: Failed to initialize PROJ.4 with `+proj=stere +lat_0=90 +lat_ts=60 +lon_0=180 +k=-90 +x_0=0 +y_0=0 +a=6367470 +b=6367470 +units=m +no_defs '.
# Warning 1: Unable to perform coordinate transformations, so the correct
# projected geotransform could not be deduced from the lat/long
# control points.  Defaulting to ungeoreferenced.
writeLines(x, "inst/amps/ampsfile_gdalinfo.txt")