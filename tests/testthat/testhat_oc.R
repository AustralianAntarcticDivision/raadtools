library(testthat)
library(raadtools)

test_that("we get ocean colour files", {
  ocd <- ocfiles()
  ocw <- ocfiles(time.resolution = "weekly")
  expect_that(ocd, is_a("data.frame"))
  expect_that(nrow(ocd) > nrow(ocw), is_true())
  
  ocdS <- ocfiles(product = "SeaWiFS", varname = "CHL")
  ocwS <- ocfiles(product = "SeaWiFS", varname = "CHL", time.resolution = "weekly")
  
  expect_that(ocdS, is_a("data.frame"))
  expect_that(nrow(ocdS) > nrow(ocwS), is_true())
  
  expect_that(min(ocdS$date), is_a("POSIXct"))
  
  ## this might fail if we didn't drop the ST93c files :)
  expect_that(all(file.exists(ocwS$fullname)), is_true())
})