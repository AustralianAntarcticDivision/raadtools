context("trip-extract")


library(trip)
data("walrus818")
example(trip)
tr$tms <- ISOdatetime(2006, seq_len(nrow(tr)), 1, 0, 0, 0, tz = "GMT")
sst <- c(27.5199993848801, 28.3199993669987, 29.3799993433058, 29.2199993468821, 
         29.1699993479997, 27.6099993828684, 26.0499994177371, 23.0299994852394, 
         25.3399994336069, 26.2499994132668)

test_that("extracting for trip works", {
  expect_equal(extract(readsst, tr))
  expect_output(extract(readice, tr), "daily file")
  expect_output(extract(readcurr, walrus818[1:4, ], uonly = TRUE), "daily file")
  
  projection(tr) <- NA
  expect_warning(extract(readice, tr), "longitude/latitude")
})
