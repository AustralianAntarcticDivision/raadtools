context("trip-extract")


library(trip)
data("walrus818")
set.seed(1)
d <- data.frame(x=1:10, y=rnorm(10), tms=Sys.time() + 1:10, id=gl(2, 5))
context("trip-prep-1")

coordinates(d) <- ~x+y
proj4string(d) <- CRS("+proj=laea +ellps=sphere")
tr <- trip(d, c("tms", "id"))
context("trip-prep-2")

tr$tms <- ISOdatetime(2006, 1:10, 1, 0, 0, 0, tz = "GMT")
sst <- c(27.3899993877858, 28.3199993669987, 29.3799993433058, 29.2199993468821, 
         29.1699993479997, 27.2599993906915, 26.3899994101375, 23.0299994852394, 
         25.7199994251132, 26.0999994166195)
##sst <- extract(readsst, tr)


test_that("extracting for trip works", {
  expect_equal(extract(readsst, tr), sst)
  expect_type(extract(readice, tr[1:4, ]), "double")
  expect_type(extract(readcurr, walrus818[1:4, ], uonly = TRUE), "double")

  tr@proj4string@projargs <- NA_character_
  expect_warning(extract(readice, tr[1:4, ]), "longitude/latitude")
})
