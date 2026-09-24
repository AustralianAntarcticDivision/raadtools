
test_that("a SpatRaster is returned", {
  skip_if_no_raad()
  chl <- readchla()
  expect_s4_class(chl, "SpatRaster")
  expect_equal(terra::nlyr(chl), 1L)
})


test_that("chla projection is not missing", {
  skip_if_no_raad()
  chl <- readchla()
  expect_true(nzchar(terra::crs(chl)))
})
