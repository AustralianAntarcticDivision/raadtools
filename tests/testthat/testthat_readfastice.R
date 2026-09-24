

test_that("reqeusted files only are returned as a data.frame", {
  skip_if_no_raad()
    ffs <- readfastice(returnfiles = TRUE)
    expect_s3_class(ffs, "data.frame")

    expect_true(all(names(ffs) %in% c("date", "band", "fullname")))
    expect_true(all(file.exists(ffs$fullname[sample(nrow(ffs), 100)])))
    expect_equal(sum(is.na(ffs$date)),0)

})

test_that("ice data is returned as a raster object", {
  skip_if_no_raad()
          expect_s4_class(readfastice("2000-06-01"), "SpatRaster")
      })

test_that("dates not available within 1.5 days give error", {
  skip_if_no_raad()
    expect_error(readfastice("1975-10-18"), "no data file within")
})

test_that("fastice projection is not missing", {
  skip_if_no_raad()
  prj <- crs(readfastice())
  expect_true(nzchar(prj))

})
