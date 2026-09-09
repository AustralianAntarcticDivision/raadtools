require(testthat)
require(raadtools)
chl <- readchla()
test_that("a SpatRaster is returned", {
  expect_s4_class(chl, "SpatRaster")
  expect_equal(terra::nlyr(chl), 1L)
})


test_that("chla projection is not missing", {
  expect_true(nzchar(terra::crs(chl)))
})
