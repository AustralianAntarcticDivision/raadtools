metres2lon <- function(m, lat, radius = 6378137.0) {
  (m * cos
   (lat * pi/180)) / (2 * pi * radius / 360)
}

metres2lat <- function(m, radius = 6378137.0) {
  m / (2 * pi * radius / 360)
}
library(raadtools)
e <- extent(-180, 180, -90, -20)
w <- readwind("2011-02-01", xylim = e); u <- w[[1]]; v <- w[[2]]
ice <- readice(getZ(w)[1])
prj <- projection(ice)
n <- 100
xypts <- cbind(runif(n, xmin(ice), xmax(ice)), 
                runif(n, ymin(ice), ymax(ice)))
llpts <- project(xypts, prj, inv = TRUE)
u0 <- extract(u, llpts)
v0 <- extract(v, llpts)

ntime <- 13 * 24 * 3600
time_step <- 3600/2
ntrace <- ntime/time_step
trace1 <- array(0, c(nrow(llpts), 2, ntrace))
trace1[,,1] <- xypts

newpts <- llpts
for (i in seq(2, ntrace)) {
##bad <- is.na(u0) | is.na(v0)
newpts <- cbind(newpts[,1] + (metres2lon(u0,  newpts[,2]) * time_step) , 
                newpts[,2] + (metres2lat(v0) * time_step)) 

trace1[,,i] <- project(newpts, prj)

}

plot(extent(xypts) + 10e6, type = "n", asp = 1)
apply(trace1, 1, function(x) lines(t(x)))

