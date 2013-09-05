
require(testthat)
require(raadtools)
require(ncdf)
test_that("current data is returned as a raster object", {
           expect_that(readcurr("2000-01-01"), is_a("RasterBrick"))
      })

test_that("multiple dates are supported only with magonly/dironly, and if given the length is 2", {
           expect_that(readcurr(c("2000-01-01", "2003-01-10")), gives_warning())
           expect_that(readcurr(c("2000-01-01", "2003-01-10")), is_a("RasterBrick"))
           expect_that(nlayers(readcurr(c("2000-01-01", "2003-01-10"))), equals(2))
                 })

test_that("multi layers returned with magonly or dironly", {
    expect_that(nlayers(readcurr(c("2000-01-01", "2003-01-10"), magonly = TRUE)), equals(2))
    expect_that(nlayers(readcurr(c("2000-01-01", "2003-01-10"), dironly = TRUE)), equals(2))
    expect_that(readcurr(c("2000-01-01", "2003-01-10"), magonly = TRUE, dironly = TRUE), gives_warning())
})



test_that("dates not available within 4 days give error", {
     expect_that(readcurr("1999-11-19"), throws_error("no data file within"))
})



