
test_that("ssh data is returned as a raster object", {
          expect_s4_class(readssh("2000-01-01"), "SpatRaster")
          expect_s4_class(readssh("2000-01-01", ssha = TRUE), "SpatRaster")
      })

test_that("multiple dates return a multilayer object", {
          expect_s4_class(suppressWarnings(readssh(c("2000-01-01", "2003-01-10", "1998-08-01"))), "SpatRaster")
         
      })
test_that("using filename with two dates gives a brick", {
  expect_s4_class(readssh(c("2000-01-01", "2002-01-01"), filename = sprintf("%s.grd", tempfile())), "SpatRaster")

})

test_that("nuances of different defaults for multilayer object", {
  expect_s4_class(readssh(c( "1998-08-01", "2000-01-01", "2003-01-10")), "RasterBrick")
  expect_s4_class(readssh(c( "1998-08-01", "2000-01-01", "2003-01-10"), lon180 = FALSE), "RasterBrick")
  expect_s4_class(readssh(c( "1998-08-01", "2000-01-01", "2003-01-10"), xylim = extent(-180, 180, -90, -40)), "RasterBrick")
})

test_that("date ranges are valid", {
  dr <- range(sshfiles()$date)
  expect_s3_class(dr, "POSIXct")
  expect_true(all(!is.na(dr)))
  dr2 <- range(sshfiles(ssha=TRUE)$date)
  expect_s3_class(dr2, "POSIXct")
  expect_true(all(!is.na(dr2)))
  
  dr3 <- range(sshfiles(time.resolution = "daily", ssha=TRUE)$date)
  expect_s3_class(dr3, "POSIXct")
  expect_true(all(!is.na(dr3)))
  
  # dr4 <- range(sshfiles(time.resolution = "monthly_clim", ssha=TRUE)$date)
  # expect_s3_class(dr4, "POSIXct")
  # expect_true(all(!is.na(dr4)))
  # 
  # dr5 <- range(sshfiles(time.resolution = "seasonal_clim", ssha=TRUE)$date)
  # expect_s3_class(dr5, "POSIXct")
  # expect_true(all(!is.na(dr5)))
  # 
  # expect_equal(nrow(sshfiles(time.resolution = "seasonal_clim", ssha=TRUE)), 4)
  # expect_equal(nrow(sshfiles(time.resolution = "monthly_clim", ssha=TRUE)), 12)

})

test_that("ssh projection is not missing", {
  prj <- projection(readssh())
  expect_false(is.na(prj))
  
})


cf <- sshfiles(time.resolution = "daily", ssha = FALSE)
cfa <- sshfiles(time.resolution = "daily", ssha = TRUE)
xyt <- data.frame(x = c(100, 120, 130, 145, 150), y = seq(-80, 20, length = 5),   
                  dts = as.POSIXct(seq(as.Date("2011-01-03"), by = "1 month", length = 5), tz = "UTC")
)
test_that("read is ok with inputfiles", {
  expect_s4_class(readssh("2015-01-01",  time.resolution = "daily", inputfiles = cf),"BasicRaster")
  expect_s4_class(readssh("2015-01-01", ssha = TRUE, time.resolution = "daily", inputfiles = cfa), "BasicRaster")
  expect_type(extract(readssh, xyt), "double")
  # 
  # expect_type(extract(readssh, xyt, ssha = TRUE, inputfiles = cfa), "double")
})

