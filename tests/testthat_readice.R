## this is perhaps overkill?

require(testthat)
require(raadtools)
test_that("ice data is returned as a raster object", {
          expect_that(readice("2000-01-01"), is_a("RasterLayer"))
      })

test_that("valid multi dates is returned as a raster object", {
         expect_that(readice("2000-01-01", "2000-01-10"), is_a("RasterBrick"))
})
