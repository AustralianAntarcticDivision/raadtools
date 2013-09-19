require(testthat)
require(raadtools)

test_that("a raster layer is returned", {
    expect_that(readchla(), is_a("RasterLayer"))})
