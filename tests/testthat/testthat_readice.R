require(testthat)
require(raadtools)

test_that("all variants are available", {
  r1 <- readice(time.resolution = "monthly", hemisphere = "south")
  r2 <- readice(time.resolution = "monthly", hemisphere = "north")
  r3 <- readice(time.resolution = "daily", hemisphere = "south")
  r4 <- readice(time.resolution = "daily", hemisphere = "north")
  
  r5 <- readice(product = "amsr")
  expect_that(readice(product = "ssmi"), throws_error())
  
})

test_that("requested files only are returned as a data.frame", {
    ffs <- readice(returnfiles = TRUE)
    expect_that(ffs, is_a("data.frame"))

    expect_that(all(names(ffs) %in% c("date", "file", "fullname")), is_true())
    expect_that(all(file.exists(ffs$fullname[sample(nrow(ffs), 100)])), is_true())
    expect_that(sum(is.na(ffs$date)), equals(0))

})

test_that("spatial crop works as expected", {
    ext <- extent(-3086361, -1192990, 357501, 1882251)
    ice <- readice(c("2000-01-01", "2000-01-10"), xylim = ext)
    expect_that(dim(ice), equals(c(61, 75, 2)))
})

test_that("ice data is returned as a raster object", {
          expect_that(readice("2000-01-01"), is_a("RasterStack"))
      })

test_that("dates not available within 1.5 days give error", {
    expect_that(readice("1975-10-18"), throws_error("no data file within"))
})

##test_that("dates  within 1.5 months succeed", {
##    expect_that(readice("2002-10-18", time.resolution = "monthly"), is_a("RasterLayer"))
##})

test_that("input data can be Date",
          expect_that(readice(as.Date("2000-01-01")), is_a("RasterStack"))
          )

test_that("input data can be POSIXct",
          expect_that(readice(as.POSIXct("2000-01-01")), is_a("RasterStack"))
          )

x <- readice(); y <- readice(rescale = FALSE);

test_that("missing values are constant for setNA scaled or not",
      expect_that(cellStats(is.na(x) - is.na(y), "sum"), equals(0))
          )

x <- readice(setNA = TRUE); y <- readice(setNA = FALSE);
test_that("missing values are greater in number for setNA",
     expect_true(cellStats(is.na(x), "sum") >  cellStats(is.na(y), "sum"))
          )



## first test for readmulti
test_that("valid multi dates is returned as a raster object", {
         expect_that(readice(c("2000-01-01", "2000-01-10")), is_a("RasterStack"))
})

b1 <- readice("1997-04-06")
b2 <- readice("2005-10-11")
b <- readice(c("1997-04-06", "2005-10-11"))
## does multi-read give the same result?
test_that("multi read gives the same data as single", {
    expect_that(quantile(b1), equals(quantile(b[[1]])))
    expect_that(quantile(b2), equals(quantile(b[[2]])))

})

x <- c("1997-04-06", "2005-10-11", "1997-04-06")
test_that("multi read on duplicated dates give only non-dupes", {
    expect_that(nlayers(readice(x)), equals(length(x) - 1L))
})

x <- as.POSIXct(c("1997-04-06", "2005-10-11", "1997-04-09"))
test_that("multi read on out of order dates sorts them", {
    expect_that(format(getZ(readice(x))), equals(format(sort(x))))
})


test_that("ice projection is not missing", {
  prj <- projection(readice())
  expect_that(is.na(prj), is_false())
  
})


