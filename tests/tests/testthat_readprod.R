require(testthat)
require(raadtools)



test_that("prod data is returned", {
    expect_that(prodfiles(), is_a("data.frame"))
    expect_that(readprod("2005-01-10"), is_a("RasterBrick"))
})
