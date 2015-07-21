## ----setup, include=FALSE, cache=FALSE-----------------------------------
library(knitr)
# set global chunk options
opts_chunk$set(fig.path='figure/minimal-', fig.align='center', fig.show='hold', cache = TRUE)
options(replace.assign=TRUE,width=90)

## ----get-start-----------------------------------------------------------
library(raadtools)
library(raster)

## ----read_ice------------------------------------------------------------
ice <- readice()
plot(ice)

## ----read-help,eval=FALSE,echo=TRUE--------------------------------------
## library(help = raadtools)

## ----read-other,echo=TRUE,output=FALSE-----------------------------------

chla <- readchla("2009-10-01")
curr <- readcurr("2012-01-09", magonly = TRUE, lon180 = FALSE)
sst <- readsst(c("2000-01-01", "2009-06-08"))
wind <- readwind()
topo <- readtopo("george_v_terre_adelie", xylim = extent(140, 141.5, -65, -64))

apal <- chl.pal(palette = TRUE)
op <- par(mfcol = c(2, 2))
plot(chla, col = chl.pal(apal$breaks), breaks = apal$breaks,
     main = "Johnson chl-a", legend = FALSE)
plot(curr, main = "ocean surface current magnitude")
plot(sst[[2]], main = "SST")
plot(topo, main = "George V Terre Adelie bathymetry",
     col = topo.colors(68)[-c(40:68)])
par(op)

plot(wind, nr = 2)

## ----explore-ice---------------------------------------------------------
ice

## ----raster-vig,eval=FALSE-----------------------------------------------
## library(raster)
## ## opens an index page to all of the package's functions
## library(help = raster)
## ## opens a PDF document with extended help and examples
## vignette("Raster")

## ----read-topo,eval=FALSE------------------------------------------------
## geb <- readtopo("gebco_08", xylim = extent(150, 220, -75, -60))

## ----wind-plot-----------------------------------------------------------
## set up data in advance, to feed the plotting function
tst <- readcurr()
xylim <- extent(projectExtent(raster(extent(130, 150, -50, -30), crs = "+proj=longlat"), projection(tst)))

plotcurrents <- function(date = as.Date("1999-11-24"), ext = NULL, scale = 2000, ...) {
  x <- readcurr(date)
  if (!is.null(ext)) x <- crop(x, ext)
  crds <- coordinates(x)
  plot(sqrt(x[[1L]]^2 + x[[2L]]^2), ...)
  x1 <- crds[,1]
  y1 <- crds[,2]
  x2 <- crds[,1] + values(x[[1L]]) * scale
  y2 <- crds[,2] + values(x[[2L]]) * scale
  op <- options(warn = -1)
  arrows(x1, y1, x2, y2, length = 0.03)
  options(op)
  invisible(NULL)
}

plotcurrents(date = as.Date("2000-01-05"), ext = xylim, scale = 2100)



## ----animate-currents,eval=FALSE-----------------------------------------
## dts <- seq(as.Date("2005-10-01"), length = 104, by = "1 week")
## pal <- sst.pal(palette = TRUE)
## cols <- pal$cols[seq(1, length(pal$cols), length = 16)]
## cols <- gsub("ff$", "99", cols)
## brks <- pal$breaks[seq(1, length(pal$breaks), length = 16)]
## 
## library(animation)
## ani.start()
## for (i in seq_along(dts)) plotcurrents(dts[i], ext = xylim, scale = 2500, col = cols, breaks = brks)
## ani.stop()
## 
## 

## ----spatio-temporal,eval=FALSE,echo=FALSE-------------------------------
## 
## library(raadtools)
## 
## 
## a <- structure(list(x = c(174, 168, 156, 111, 99, 64, 52, 46, -4,
##                           -15, -30, -38, -47, -62, -87, -127, -145, -160, -161), y = c(-72,
##                                                                                        -39, -50, -58, -35, -38, -48, -60, -48, -35, -37, -51, -68, -72,
##                                                                                        -69, -54, -40, -49, -54)), .Names = c("x", "y"), row.names = c(NA,
##                                                                                                                                                       -19L), class = "data.frame")
## 
## a$time <-  sort(as.Date("2005-01-01") + sample(c(0, 0, 0, 8, 20, 50), nrow(a), replace = TRUE))
## 
## extract(readssh, a)
## extract(readssh, a, ssha = TRUE)
## extract(readcurr, a, magonly = TRUE)
## extract(readice, a)
## 
## extract(readchla, time.resolution = "weekly")
## extract(readchla, a, time.resolution = "weekly")
## 
## 
## 
## ##extract(readwind, a)
## extract(readwind)
## readwind(dironly = TRUE)
## 
## 
## 
## a$time <- sort(as.Date("1985-01-01") + sample(c(0, 0, 0, 10, 100, 50), nrow(a), replace = TRUE))
## 
## 
## ##x <- extract(readsst)
## ##extract(readsst, a)
## 
## 
## ##extract(readsst)
## ##extract(readsst, time.resolution = "daily")
## 
## 
## ##extract(readsst, a)
## 
## 

## ----example-aurora, eval=FALSE,echo=FALSE-------------------------------
## data(aurora)
## 
## aurora$sst <- extract(readsst, aurora)
## aurora$chla <- extract(readchla, aurora)
## 
## ## nothing for 2013 yet
## ##aurora$windmag <- extract(readwind, aurora[,1:3], magonly = TRUE)
## ##aurora$winddir <- extract(readwind, aurora[,1:3], dironly = TRUE)
## 
## ##aurora$currmag <- extract(readcurr, aurora[,1:3], magonly = TRUE)
## ##aurora$currdir<- extract(readcurr, aurora[,1:3], dironly = TRUE)
## 
## ##aurora$ssh <- extract(readssh, aurora[,1:3])
## ##aurora$ssha <- extract(readssh, aurora[,1:3], ssha = TRUE)
## 
## ##aurora$ice <- extract(readice, aurora[,1:3])
## 
## 
## ## note this is slightly different, since there's no point in generalizing out time
## ##aurora$bathy <- extract(readbathy(), aurora[,1:2])
## 

## ----example-extract-withinterp,message=FALSE,results=FALSE,eval=FALSE----
## d1 <- data.frame(lon = c(130, 125, 120, -52.27, 123.21, -25.42, 36.96, -36.28),
##                  lat = c(-50, -52, -55,-66.31, -74.55, -69.66, -71.85, -68.56),
##                  gmt = as.POSIXct("2006-02-08 00:00:00") + 24 *3600 * c(0, 1, 2, 3, 106, 118, 118, 190))
## 
## llproj <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
## 
## library(raadtools)
## library(rgdal)
## 
## ## test extract vs extractxyt
## sst1 <- extractxyt("oisst", d1)
## sst2 <- extract(readsst, d1)
## 
## mag1 <- extractxyt("aviso", d1, magonly = TRUE)
## mag2 <- extract(readcurr, d1, magonly = TRUE)
## 
## ice1 <- extractxyt("nsidc", d1)
## ice2 <- extract(readice, d1)
## 
## ## these diffs should be  zorro
## max(abs(sst1 - sst2), na.rm = TRUE)
## max(abs(mag1 - mag2), na.rm = TRUE)
## max(abs(ice1 - ice2), na.rm = TRUE)
## 
## 
## ## now the goodstuff
## sst3 <- extract(readsst, d1, method = "bilinear")
## sst4 <- extract(readsst, d1, ctstime = TRUE)
## sst5 <- extract(readsst, d1, ctstime = TRUE, method = "bilinear")
## 
## data.frame(sst = sst2, sst_xyinterp = sst3, sst_timeinterp = sst4, sst_spacetimeinterp = sst5)
## 
## ## some time when we get chl-a
## d2 <- data.frame(x = c(-40, -25, -6, 12, 33, 60, 89, 107),
##                  y = c(-56, -55, -59, -62, -64, -62, -60, -60),
##                  time = as.POSIXct("2006-12-18 00:00:00") + 24 *3600 * 7 * seq_len(8))
## 
## ## resize by a factor in space so we get "more coverage"
## chla1 <- extract(readchla, d2)
## chla2 <- extract(readchla, d2, ctstime = TRUE, method = "bilinear")
## chla3 <- extract(readchla, d2, ctstime = TRUE, method = "bilinear", fact = 16)
## data.frame(chla = chla1, chla_spacetimeinterp = chla2, chla_resizespacetimeinterp = chla3)
## 
## chl <- readchla(d2$time, xylim = extent(as.matrix(d2[,1:2])))
## chl2 <- aggregate(chl, fact = 16)
## pal <- chl.pal(palette = TRUE)
## 
## ## compare the two plots, with and without spatial aggregation for "coverage"
## plot(chl, col = pal$col, breaks = pal$breaks, legend = FALSE, addfun = function() lines(d2[,1:2]))
## 
## plot(chl2, col = pal$col, breaks = pal$breaks, legend = FALSE, addfun = function() lines(d2[,1:2]))
## 
## 

