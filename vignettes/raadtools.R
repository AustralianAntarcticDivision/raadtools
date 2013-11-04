### R code from vignette source 'raadtools.Rnw'

###################################################
### code chunk number 1: setup
###################################################
library(knitr)
# set global chunk options
opts_chunk$set(fig.path='figure/minimal-', fig.align='center', fig.show='hold', cache = TRUE)
options(replace.assign=TRUE,width=90)


###################################################
### code chunk number 2: get-start
###################################################
library(raadtools)


###################################################
### code chunk number 3: read-ice
###################################################
ice <- readice()
plot(ice)


###################################################
### code chunk number 4: read-help
###################################################
library(help = raadtools)


###################################################
### code chunk number 5: read-other
###################################################
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


###################################################
### code chunk number 6: explore-ice
###################################################
ice


###################################################
### code chunk number 7: raster-vig (eval = FALSE)
###################################################
## library(raster)
## ## opens an index page to all of the package's functions
## library(help = raster)
## ## opens a PDF document with extended help and examples
## vignette("Raster")


###################################################
### code chunk number 8: raadtools.Rnw:167-209
###################################################

library(raadtools)


     a <- structure(list(x = c(174, 168, 156, 111, 99, 64, 52, 46, -4,
     -15, -30, -38, -47, -62, -87, -127, -145, -160, -161), y = c(-72,
     -39, -50, -58, -35, -38, -48, -60, -48, -35, -37, -51, -68, -72,
     -69, -54, -40, -49, -54)), .Names = c("x", "y"), row.names = c(NA,
     -19L), class = "data.frame")

a$time <-  sort(as.Date("2005-01-01") + sample(c(0, 0, 0, 8, 20, 50), nrow(a), replace = TRUE))

extract(readssh, a)
extract(readssh, a, ssha = TRUE)
extract(readcurr, a, magonly = TRUE)
extract(readice, a)




extract(readwind, a)
extract(readwind)
readwind(dironly = TRUE)



a$time <- sort(as.Date("1985-01-01") + sample(c(0, 0, 0, 10, 100, 50), nrow(a), replace = TRUE))


x <- extract(readsst)
extract(readsst, a)

x
 extract(readsst)
 extract(readsst, time.resolution = "daily")


extract(readsst, a)
extract(readchla, time.resolution = "weekly")
extract(readchla, a, time.resolution = "weekly")




