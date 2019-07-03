context("basic oc")

library(testthat)
library(raadtools)

test_that("we get ocean colour files", {
  ocd <- ocfiles()
  #ocw <- ocfiles(time.resolution = "weekly")
  expect_that(ocd, is_a("data.frame"))
  #expect_that(nrow(ocd) > nrow(ocw), is_true())
  
  ocdS <- ocfiles(product = "SeaWiFS", varname = "RRS")
  ocwS <- ocfiles(product = "SeaWiFS", varname = "CHL", time.resolution = "monthly", type = "L3m")
  
  expect_that(ocdS, is_a("data.frame"))
  expect_true(nrow(ocdS) > nrow(ocwS))
  
  expect_that(min(ocdS$date), is_a("POSIXct"))
  
  ## this might fail if we didn't drop the ST93c files :)
  expect_true(all(file.exists(ocwS$fullname)))
})


