library(testthat)
library(raadtools)

context("sea surface temperature")


test_that("sst data is returned as a raster object", {
          expect_that(readsst("2000-01-01"), is_a("RasterLayer"))
      })

test_that("multiple dates return a multilayer object", {
          expect_true(nlayers(readsst(c("2000-01-01", "2003-01-10", "1998-08-01"))) > 1L) %>% expect_warning()

        
          expect_that(tmon <- readsst(c("2000-01-01", "2003-01-10", "1998-08-01"), time.resolution = "monthly"),  gives_warning("dates out of order"))
          expect_that(tmon, is_a("RasterBrick"))
      })

d <- readsst(c("2000-01-01", "2003-01-10"))
test_that("readsst multi read returns data in -180,180", {
    expect_true(xmin(d) < 0)
    expect_true(xmax(d) < 190)

})

test_that("readsst latest works", {
  expect_that(readsst(latest = TRUE), is_a("RasterLayer"))
})
d <- readsst(c("2003-01-10"))
test_that("readsst single read returns data in -180,180", {
    expect_true(xmin(d) < 0)
    expect_true(xmax(d) < 190)

})

test_that("input crop extent works for a time series", {

  ext <- extent(100, 150, -75, -30)

  dts <- seq(as.Date("2001-01-03"), by = "1 week", length = 10)
  sst <- readsst(dts, xylim = ext)
  expect_that(sst, is_a("RasterBrick"))
  expect_that(dim(sst), equals(c(180, 200, 10)))

})

test_that("object projection is not missing", {
  prj <- projection(readsst())
  expect_false(is.na(prj))

})

test_that("dates  within 1.5 months succeed", {
  expect_that(readsst("1981-11-18", time.resolution = "monthly"), is_a("RasterLayer"))
})

test_that("daily is different from monthly", {
  x1 <- readsst("1981-11-18", time.resolution = "monthly")
  x2 <- readsst("1981-11-18", time.resolution = "daily")

  expect_that(compareRaster(x1, x2), throws_error("different number or columns"))
})



cf <- sstfiles(time.resolution = "daily")
xyt <- data.frame(x = c(100, 120, 130, 145, 150), y = seq(-80, 20, length = 5),   
                  dts = seq(as.Date("2001-01-03"), by = "1 month", length = 5)
)
test_that("read is ok with inputfiles", {
  expect_that(readsst("2015-01-01", time.resolution = "daily", inputfiles = cf), is_a("RasterLayer"))
  #expect_that(extract(readsst, xyt,  time.resolution = "daily"), is_a("numeric"))
})




library(testthat)
library(raadtools)
context("basic extract")

data(aurora)
aurora$DATE_TIME_UTC <- aurora$DATE_TIME_UTC - 720 * 24 * 3600
test_that("we get values", {
  expect_that(extract(readsst, aurora[c(1, 5, 10, 15), ]), is_a("numeric"))

})

test_that("another example works", {
  expect_that(extract(readsst, aurora[c(1, 5, 10, 11, 15), ]), is_a("numeric"))
})


test_that("expected arrangement of file data frame", { 
          expect_that(names(sstfiles()), equals(c("date", "fullname", "root"))) 
          expect_that(names(sstfiles(time.resolution = "monthly")), 
                      equals(c("date", "fullname", "band", "root")))
          
  })


