## this is perhaps overkill?

require(testthat)
require(raadtools)
test_that("ice data is returned as a raster object", {
          expect_that(readice("2000-01-01"), is_a("RasterLayer"))
      })

test_that("dates not available within 1.5 days give error", {
    expect_that(readice("1978-10-18"), throws_error("no ice data file within"))
})

test_that("dates  within 1.5 months succeed", {
    expect_that(readice("1978-10-18", time.resolution = "monthly"), is_a("RasterLayer"))
})

test_that("input data can be Date",
          expect_that(readice(as.Date("2000-01-01")), is_a("RasterLayer"))
          )

test_that("input data can be POSIXct",
          expect_that(readice(as.POSIXct("2000-01-01")), is_a("RasterLayer"))
          )

x <- readice(); y <- readice(rescale = FALSE);

test_that("missing values are constant for setNA scaled or not",
      expect_that(cellStats(is.na(x) - is.na(y), "sum"), equals(0))
          )

x <- readice(setNA = TRUE); y <- readice(setNA = FALSE);
test_that("missing values are greater in number for setNA",
     expect_true(cellStats(is.na(x), "sum") >  cellStats(is.na(y), "sum"))
          )



## first test for readmulti
##test_that("valid multi dates is returned as a raster object", {
##         expect_that(readice("2000-01-01", "2000-01-10"), is_a("RasterBrick"))
##})
