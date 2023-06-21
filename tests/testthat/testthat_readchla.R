require(testthat)
require(raadtools)
chl <- readchla()
test_that("a raster layer is returned", {
    expect_that(chl, is_a("RasterLayer"))})


test_that("chla projection is not missing", {
  prj <- projection(chl)
  expect_false(is.na(prj))

})
