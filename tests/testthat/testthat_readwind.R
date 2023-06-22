context("winds")

require(testthat)
require(raadtools)



test_that("read returns a Raster", {

    expect_that(readwind(), is_a("RasterStack"))
    expect_that(nlayers(readwind(magonly = TRUE)), equals(1L))

})



cf <- windfiles(time.resolution = "6hourly")
xyt <- data.frame(x = c(100, 120, 130, 145, 150), y = seq(-80, 20, length = 5),   
                  dts = seq(as.Date("2001-01-03"), by = "1 month", length = 5)
)
test_that("read is ok with inputfiles", {
  expect_that(readwind("2015-01-01",  inputfiles = cf), is_a("RasterStack"))
  expect_that(extract(readwind, xyt, vonly = TRUE), is_a("numeric"))
})
