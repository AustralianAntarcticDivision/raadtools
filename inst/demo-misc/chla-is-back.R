## https://github.com/AustralianAntarcticDivision/raadtools/commit/f26cafbc10efce72f7cd24abbe70f5acd65ba270
library(raadtools)
dates <- seq(as.Date("2016-12-15"), length = 30, by = "1 day")
print(range(dates))
xylim <- raster::extent(100, 160, -70, -30)
system.time({
johnson <- readchla(dates, product = "MODISA", xylim = xylim)
nasa    <- readchla(dates, product = "MODISA", xylim = xylim, 
                    algorithm = "nasa")
})

pal <- palr::chlPal(palette = TRUE)
par(mar = rep(0, 4))
image(johnson, col = pal$cols[-1], breaks = pal$breaks, asp = "", useRaster = TRUE)
image(nasa, col = pal$cols[-1], breaks = pal$breaks, asp = "", useRaster = TRUE)

print(johnson)