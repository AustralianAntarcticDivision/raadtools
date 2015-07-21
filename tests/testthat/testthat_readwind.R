require(testthat)
require(raadtools)



test_that("read returns a Raster", {

    expect_that(readwind(), is_a("RasterStack"))
    expect_that(nlayers(readwind(magonly = TRUE)), equals(1L))

})
