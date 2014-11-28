

require(testthat)
require(raadtools)
test_that("ssh data is returned as a raster object", {
          expect_that(readssh("2000-01-01"), is_a("RasterLayer"))
          expect_that(readssh("2000-01-01", ssha = TRUE), is_a("RasterLayer"))
      })

test_that("multiple dates return a multilayer object", {
          expect_that(readssh(c("2000-01-01", "2003-01-10", "1998-08-01")), is_a("RasterBrick"))

      })

testthat("date ranges are valid", {
  dr <- range(sshfiles()$date)
  expect_that(dr, is_a("POSIXct"))
  expect_that(all(!is.na(dr)), is_true())
  dr2 <- range(sshfiles(ssha=TRUE)$date)
  expect_that(dr2, is_a("POSIXct"))
  expect_that(all(!is.na(dr2)), is_true())
  
  dr3 <- range(sshfiles(time.resolution = "monthly", ssha=TRUE)$date)
  expect_that(dr3, is_a("POSIXct"))
  expect_that(all(!is.na(dr3)), is_true())
  
  dr4 <- range(sshfiles(time.resolution = "monthly_clim", ssha=TRUE)$date)
  expect_that(dr4, is_a("POSIXct"))
  expect_that(all(!is.na(dr4)), is_true())
  
  dr5 <- range(sshfiles(time.resolution = "seasonal_clim", ssha=TRUE)$date)
  expect_that(dr5, is_a("POSIXct"))
  expect_that(all(!is.na(dr5)), is_true())
  
  expect_that(nrow(sshfiles(time.resolution = "seasonal_clim", ssha=TRUE)), equals(4))
  expect_that(nrow(sshfiles(time.resolution = "monthly_clim", ssha=TRUE)), equals(12))
  
  
}