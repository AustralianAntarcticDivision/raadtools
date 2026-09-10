


test_that("sst data is returned as a raster object", {
          expect_s4_class(readsst("2000-01-01"), "SpatRaster")
      })

test_that("multiple dates return a multilayer object", {
          expect_true(suppressWarnings(nlyr(readsst(c("2000-01-01", "2003-01-10", "1998-08-01"))) > 1L))
expect_warning(readsst(c("2000-01-01", "2003-01-10", "1998-08-01")))
        
          expect_warning(tmon <- readsst(c("2000-01-01", "2003-01-10", "1998-08-01"), time.resolution = "monthly"), "dates out of order")
          expect_s4_class(tmon, "SpatRaster")
      })

d <- readsst(c("2000-01-01", "2003-01-10"))
test_that("readsst multi read returns data in -180,180", {
    expect_true(xmin(d) < 0)
    expect_true(xmax(d) < 190)

})

test_that("readsst latest works", {
  expect_s4_class(readsst(latest = TRUE), "SpatRaster")
})
d <- readsst(c("2003-01-10"))
test_that("readsst single read returns data in -180,180", {
    expect_true(xmin(d) < 0)
    expect_true(xmax(d) < 190)

})

test_that("input crop extent works for a time series", {

  ext <- ext(100, 150, -75, -30)

  dts <- seq(as.Date("2001-01-03"), by = "1 week", length = 10)
  sst <- readsst(dts, xylim = ext)
  expect_s4_class(sst, "SpatRaster")
  expect_equal(dim(sst), c(180, 200, 10))

})

test_that("object projection is not missing", {
  prj <- crs(readsst())
  expect_true(nzchar(prj))

})

test_that("dates  within 1.5 months succeed", {
  expect_s4_class(readsst("1981-11-18", time.resolution = "monthly"), "SpatRaster")
})

test_that("daily is different from monthly", {
  x1 <- readsst("1981-11-18", time.resolution = "monthly")
  x2 <- readsst("1981-11-18", time.resolution = "daily")

  expect_true(!all(na.omit(values(x1)[,1]) == na.omit(values(x2)[,1])))
})



cf <- sstfiles(time.resolution = "daily")
xyt <- data.frame(x = c(100, 120, 130, 145, 150), y = seq(-80, 20, length = 5),   
                  dts = seq(as.Date("2001-01-03"), by = "1 month", length = 5)
)
test_that("read is ok with inputfiles", {
  expect_s4_class(readsst("2015-01-01", time.resolution = "daily", inputfiles = cf), "SpatRaster")
  #expect_type(extract(readsst, xyt, time.resolution = "daily"), "double")
})




data(aurora)
aurora$DATE_TIME_UTC <- aurora$DATE_TIME_UTC - 720 * 24 * 3600
test_that("we get values", {
  expect_type(extract(readsst, aurora[c(1, 5, 10, 15), ]), "double")

})

test_that("another example works", {
  expect_type(extract(readsst, aurora[c(1, 5, 10, 11, 15), ]), "double")
})


test_that("expected arrangement of file data frame", { 
          expect_equal(names(sstfiles()), c("date", "fullname", "root")) 
          expect_equal(names(sstfiles(time.resolution = "monthly")), c("date", "fullname", "band", "root"))
          
  })


