

require(testthat)
require(raadtools)
test_that("ssh data is returned as a raster object", {
          expect_that(readssh("2000-01-01"), is_a("RasterLayer"))
          expect_that(readssh("2000-01-01", ssha = TRUE), is_a("RasterLayer"))
      })

test_that("multiple dates return a multilayer object", {
          expect_that(readssh(c("2000-01-01", "2003-01-10", "1998-08-01")), is_a("RasterBrick"))

          expect_that(readssh(c("2000-01-01", "2003-01-10", "1998-08-01"), time.resolution = "monthly"), is_a("RasterBrick"))
      })
