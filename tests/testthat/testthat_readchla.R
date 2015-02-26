require(testthat)
require(raadtools)

test_that("a raster layer is returned", {
    expect_that(readchla(), is_a("RasterLayer"))})


test_that("chla projection is not missing", {
  prj <- projection(readchla())
  expect_that(is.na(prj), is_false())
  
})
