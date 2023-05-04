context("sea surface height")

require(testthat)
require(raadtools)
test_that("ssh data is returned as a raster object", {
          expect_that(readssh("2000-01-01"), is_a("RasterBrick"))
          expect_that(readssh("2000-01-01", ssha = TRUE), is_a("RasterBrick"))
      })

test_that("multiple dates return a multilayer object", {
          expect_that(suppressWarnings(readssh(c("2000-01-01", "2003-01-10", "1998-08-01"))), is_a("RasterBrick"))
         
      })
test_that("using filename with two dates gives a brick", {
  expect_that(readssh(c("2000-01-01", "2002-01-01"), filename = sprintf("%s.grd", tempfile())), is_a("RasterBrick"))

})

test_that("nuances of different defaults for multilayer object", {
  expect_that(readssh(c( "1998-08-01", "2000-01-01", "2003-01-10")), is_a("RasterBrick"))
  expect_that(readssh(c( "1998-08-01", "2000-01-01", "2003-01-10"), lon180 = FALSE), is_a("RasterBrick"))
  expect_that(readssh(c( "1998-08-01", "2000-01-01", "2003-01-10"), xylim = extent(-180, 180, -90, -40)), is_a("RasterBrick"))
})

test_that("date ranges are valid", {
  dr <- range(sshfiles()$date)
  expect_that(dr, is_a("POSIXct"))
  expect_true(all(!is.na(dr)))
  dr2 <- range(sshfiles(ssha=TRUE)$date)
  expect_that(dr2, is_a("POSIXct"))
  expect_true(all(!is.na(dr2)))
  
  dr3 <- range(sshfiles(time.resolution = "daily", ssha=TRUE)$date)
  expect_that(dr3, is_a("POSIXct"))
  expect_true(all(!is.na(dr3)))
  
  # dr4 <- range(sshfiles(time.resolution = "monthly_clim", ssha=TRUE)$date)
  # expect_that(dr4, is_a("POSIXct"))
  # expect_that(all(!is.na(dr4)), is_true())
  # 
  # dr5 <- range(sshfiles(time.resolution = "seasonal_clim", ssha=TRUE)$date)
  # expect_that(dr5, is_a("POSIXct"))
  # expect_that(all(!is.na(dr5)), is_true())
  # 
  # expect_that(nrow(sshfiles(time.resolution = "seasonal_clim", ssha=TRUE)), equals(4))
  # expect_that(nrow(sshfiles(time.resolution = "monthly_clim", ssha=TRUE)), equals(12))

})

test_that("ssh projection is not missing", {
  prj <- projection(readssh())
  expect_false(is.na(prj))
  
})


cf <- sshfiles(time.resolution = "daily", ssha = FALSE)
cfa <- sshfiles(time.resolution = "daily", ssha = TRUE)
xyt <- data.frame(x = c(100, 120, 130, 145, 150), y = seq(-80, 20, length = 5),   
                  dts = seq(as.Date("2011-01-03"), by = "1 month", length = 5)
)
test_that("read is ok with inputfiles", {
  expect_s4_class(readssh("2015-01-01",  time.resolution = "daily", inputfiles = cf),"BasicRaster")
  expect_s4_class(readssh("2015-01-01", ssha = TRUE, time.resolution = "daily", inputfiles = cfa), "BasicRaster")
  expect_that(extract(readssh, xyt), is_a("numeric"))
  # 
  # expect_that(extract(readssh, xyt, ssha = TRUE,  inputfiles = cfa), is_a("numeric"))
})

