


test_that("read returns a Raster", {

    expect_s4_class(readwind(), "SpatRaster")
    expect_equal(nlyr(readwind(magonly = TRUE)), 1L)

})



cf <- windfiles(time.resolution = "6hourly")
xyt <- data.frame(x = c(100, 120, 130, 145, 150), y = seq(-80, 20, length = 5),   
                  dts = seq(as.Date("2001-01-03"), by = "1 month", length = 5)
)
test_that("read is ok with inputfiles", {

  expect_s4_class(readwind("2015-01-01", inputfiles = cf), "SpatRaster")
  expect_type(extract(readwind, xyt, vonly = TRUE), "double")
})
