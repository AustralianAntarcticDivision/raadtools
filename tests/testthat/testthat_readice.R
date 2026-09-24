

test_that("all variants are available", {
  skip_if_no_raad()
  ## the once-per-session transition notice would make expect_silent()
  ## depend on which file ran first; test-shim-notice.R covers the notice
  withr::local_options(list(raadtools.shim.warn = FALSE))

  ## readice() ignores time.resolution, readice_monthly() is the monthly reader
  expect_s4_class(readice(time.resolution = "monthly", hemisphere = "south"), "SpatRaster")
  r1 <- readice_monthly(hemisphere = "south")
  expect_silent(r2 <- readice_monthly(time.resolution = "monthly", hemisphere = "north"))
  expect_silent(r3 <- readice(time.resolution = "daily", hemisphere = "south"))
  expect_silent(r4 <- readice(time.resolution = "daily", hemisphere = "north"))
  
  expect_error(readice(product = "amsr"))
  expect_silent(r5 <- read_amsr_ice())
  expect_error(readice(product = "ssmi"))
  
})

test_that("requested files only are returned as a data.frame", {
    skip_if_no_raad()
    ffs <- readice(returnfiles = TRUE)
    expect_s3_class(ffs, "data.frame")

    expect_true(all(names(ffs) %in% c("date",  "fullname", "root")))
    expect_true(all(file.exists(ffs$fullname[sample(nrow(ffs), 100)])))
    expect_equal(sum(is.na(ffs$date)), 0)

})

test_that("spatial crop works as expected", {
    skip_if_no_raad()
    ext <- ext(-3086361, -1192990, 357501, 1882251)
    ice <- readice(c("2000-01-01", "2000-01-10"), xylim = ext)
    expect_equal(dim(ice), c(61, 75, 2))
})

test_that("ice data is returned as a raster object", {
          skip_if_no_raad()
          expect_s4_class(readice("2000-01-01"), "SpatRaster")
      })

test_that("dates not available within 1.5 days give error", {
    skip_if_no_raad()
    expect_error(readice("1975-10-18"), "no data file within")
})

##test_that("dates  within 1.5 months succeed", {
##    expect_s4_class(readice("2002-10-18", time.resolution = "monthly"), "SpatRaster")
##})

test_that("input data can be Date", {
  skip_if_no_raad()
  expect_s4_class(readice(as.Date("2000-01-01")), "SpatRaster")
})

test_that("input data can be POSIXct", {
  skip_if_no_raad()
  expect_s4_class(readice(as.POSIXct("2000-01-01")), "SpatRaster")
})



## first test for readmulti
test_that("valid multi dates is returned as a raster object", {
         skip_if_no_raad()
         expect_s4_class(readice(c("2000-01-01", "2000-01-10")), "SpatRaster")
})

## does multi-read give the same result?
test_that("multi read gives the same data as single", {
    skip_if_no_raad()
    b1 <- readice("1997-04-06")
    b2 <- readice("2005-10-11")
    b <- readice(c("1997-04-06", "2005-10-11"))
    expect_equal(quantile(values(b1), na.rm = TRUE), quantile(values(b[[1]]), na.rm = TRUE))
    expect_equal(quantile(values(b2, na.rm = TRUE)), quantile(values(b[[2]]), na.rm = TRUE))

})

test_that("multi read on duplicated dates give only non-dupes", {
    skip_if_no_raad()
    x <- c("1997-04-06", "2005-10-11", "1997-04-06")
    suppressWarnings(expect_equal(nlyr(readice(x)), length(x) - 1L))
})

test_that("multi read on out of order dates sorts them", {
    skip_if_no_raad()
    x <- as.POSIXct(c("1997-04-06", "2005-10-11", "1997-04-09"), tz = "UTC")
    expect_equal(format(terra::time(readice(x))), format(sort(x)))  %>% expect_warning()
})


test_that("ice projection is not missing", {
  skip_if_no_raad()
  prj <- crs(readice())
  expect_true(nzchar(prj))
  
})


xyt <- data.frame(x = c(100, 120, 130, 145, 150), y = seq(-80, 20, length = 5),   
                  dts = seq(as.Date("2011-01-03"), by = "1 month", length = 5)
)
test_that("read is", {
  skip_if_no_raad()
  cf <- raadfiles::amsr_daily_files()
  expect_error(readice("2015-01-01", product = "amsr", time.resolution = "daily", inputfiles = cf))
  expect_s4_class(read_amsr_ice("2015-01-01", inputfiles = cf), "SpatRaster")
  expect_type(extract(read_amsr_ice, xyt), "double")
  expect_type(extract(read_amsr_ice, xyt, product = "amsr"), "double")
})

