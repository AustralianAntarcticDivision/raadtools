require(testthat)
require(raadtools)

test_that("a raster layer is returned", {
    expect_that(chl <- readchla(), is_a("RasterLayer"))})


test_that("chla projection is not missing", {
  prj <- projection(chl)
  expect_false(is.na(prj))

})
