

test_that("we get ocean colour files", {
  ocd <- ocfiles()
  #ocw <- ocfiles(time.resolution = "weekly")
  expect_s3_class(ocd, "data.frame")
  #expect_true(nrow(ocd) > nrow(ocw))
  
  #ocdS <- ocfiles(product = "SeaWiFS", varname = "RRS")
  ocwS <- ocfiles(product = "SeaWiFS", varname = "CHL", time.resolution = "monthly", type = "L3m")
  
  #expect_s3_class(ocdS, "data.frame")
  #expect_true(nrow(ocdS) > nrow(ocwS))
  
  #expect_s3_class(min(ocdS$date), "POSIXct")
  
  ## this might fail if we didn't drop the ST93c files :)
  expect_true(all(file.exists(ocwS$fullname)))
})


