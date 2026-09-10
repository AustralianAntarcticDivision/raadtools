library(testthat)
library(raadtools)
#require(ncdf)
context("surface currents")
cf <- currentsfiles()

test_that("current data is returned as a raster object", {
           expect_s4_class(readcurr("2000-01-01"), "SpatRaster")
  expect_s4_class(readcurr("2000-01-01", inputfiles = cf), "SpatRaster")
      })

test_that("multiple dates are supported only with magonly/dironly, and if given the length is 2", {
           expect_warning(readcurr(c("2000-01-01", "2003-01-10"), inputfiles = cf))
           expect_s4_class(readcurr(c("2000-01-01", "2003-01-10"), dironly = TRUE, inputfiles = cf), "SpatRaster")
           expect_equals(nlyr(readcurr(c("2000-01-01", "2003-01-10"), magonly = TRUE, inputfiles = cf)), 2L)
                 })

test_that("multi layers returned with magonly or dironly", {
    expect_equal(nlyr(readcurr(c("2000-01-01", "2003-01-10"), magonly = TRUE, inputfiles = cf)), 2)
    expect_equal(nlyr(readcurr(c("2000-01-01", "2003-01-10"), dironly = TRUE, inputfiles = cf)), 2)
    expect_error(readcurr(c("2000-01-01"), magonly = TRUE, dironly = TRUE, inputfiles = cf))
    expect_error(readcurr(c("2000-01-01"), vonly = TRUE, dironly = TRUE, inputfiles = cf))
})



test_that("dates not available within 4 days give error", {
    expect_error(readcurr("1992-10-08", inputfiles = cf), "no data file within")
    ## we now have data for this date
    expect_s4_class(readcurr("1999-11-19", inputfiles = cf), "RasterStack")
    expect_s4_class(readcurr(cf$date[1000:1005], xylim = extent(-20, 20, -20, 20), lon180 = TRUE, dironly = TRUE, inputfiles = cf), "RasterStack")
    expect_s4_class(readcurr(cf$date[1000:1003], xylim = extent(160, 200, -20, 20), filename = sprintf("%s.grd", tempfile()), lon180 = FALSE, magonly = TRUE, inputfiles = cf), "RasterBrick")
})


test_that("input crop extent works for a time series", {

  ##ext <- extent(0, 20037508, -13500000, -5400000)
ext <- extent(-180, 180, -90, -30)
  dts <- seq(as.Date("2001-01-03"), by = "1 month", length = 5)
  curr <- readcurr(dts, xylim = ext, magonly = TRUE, inputfiles = cf)
  expect_s4_class(curr, "RasterStack")
  expect_equal(dim(curr), c(240L, 1440L, 5L))

})

test_that("curr projection is not missing", {
  prj <- projection(readcurr(inputfiles = cf))
  expect_false(is.na(prj))
  
})



xyt <- data.frame(x = c(100, 120, 130, 145, 150), y = seq(-80, 20, length = 5),   
                  dts = seq(as.Date("2001-01-03"), by = "1 month", length = 5)
)
test_that("read is ok with inputfiles", {
  expect_s4_class(readcurr("2015-01-01", inputfiles = cf), "RasterStack")
  expect_type(extract(readcurr, xyt, uonly = TRUE), "double") %>% expect_warning() 
})
