

require(testthat)
require(raadtools)
test_that("sst data is returned as a raster object", {
          expect_that(readsst("2000-01-01"), is_a("RasterLayer"))
      })

test_that("multiple dates return a multilayer object", {
          expect_that(readsst(c("2000-01-01", "2003-01-10", "1998-08-01")), is_a("RasterStack"))

        
          expect_that(readsst(c("2000-01-01", "2003-01-10", "1998-08-01"), time.resolution = "monthly"), throws_error())
      })

d <- readsst(c("2000-01-01", "2003-01-10"))
test_that("readsst multi read returns data in -180,180", {
    expect_that(xmin(d) < 0, is_true())
    expect_that(xmax(d) < 190, is_true())

})

d <- readsst(c("2003-01-10"))
test_that("readsst single read returns data in -180,180", {
    expect_that(xmin(d) < 0, is_true())
    expect_that(xmax(d) < 190, is_true())

})

test_that("input crop extent works for a time series", {

  ext <- extent(100, 150, -75, -30)

  dts <- seq(as.Date("2001-01-03"), by = "1 week", length = 10)
  sst <- readsst(dts, xylim = ext)
  expect_that(sst, is_a("RasterBrick"))
  expect_that(dim(sst), equals(c(180, 200, 10)))

})


## test_that("dates not available within 1.5 days give error", {
##     expect_that(readice("1978-10-18"), throws_error("no ice data file within"))
## })

## test_that("dates  within 1.5 months succeed", {
##     expect_that(readice("1978-10-18", time.resolution = "monthly"), is_a("RasterLayer"))
## })

## test_that("input data can be Date",
##           expect_that(readice(as.Date("2000-01-01")), is_a("RasterLayer"))
##           )

## test_that("input data can be POSIXct",
##           expect_that(readice(as.POSIXct("2000-01-01")), is_a("RasterLayer"))
##           )

## x <- readice(); y <- readice(rescale = FALSE);

## test_that("missing values are constant for setNA scaled or not",
##       expect_that(cellStats(is.na(x) - is.na(y), "sum"), equals(0))
##           )

## x <- readice(setNA = TRUE); y <- readice(setNA = FALSE);
## test_that("missing values are greater in number for setNA",
##      expect_true(cellStats(is.na(x), "sum") >  cellStats(is.na(y), "sum"))
##           )



## ## first test for readmulti
## test_that("valid multi dates is returned as a raster object", {
##          expect_that(readice(c("2000-01-01", "2000-01-10")), is_a("RasterBrick"))
## })

## b1 <- readice("1997-04-06")
## b2 <- readice("2005-10-11")
## b <- readice(c("1997-04-06", "2005-10-11"))
## ## does multi-read give the same result?
## test_that("multi read gives the same data as single", {
##     expect_that(quantile(b1), equals(quantile(b[[1]])))
##     expect_that(quantile(b2), equals(quantile(b[[2]])))

## })

## x <- c("1997-04-06", "2005-10-11", "1997-04-06")
## test_that("multi read on duplicated dates give only non-dupes", {
##     expect_that(nlayers(readice(x)), equals(length(x) - 1L))
## })

## x <- as.POSIXct(c("1997-04-06", "2005-10-11", "1997-04-09"))
## test_that("multi read on out of order dates sorts them", {
##     expect_that(format(getZ(readice(x))), equals(format(sort(x))))
## })



