require(testthat)
require(raadtools)



test_that("reqeusted files only are returned as a data.frame", {
    ffs <- readfastice(returnfiles = TRUE)
    expect_that(ffs, is_a("data.frame"))

    expect_true(all(names(ffs) %in% c("date", "band", "fullname")))
    expect_true(all(file.exists(ffs$fullname[sample(nrow(ffs), 100)])))
    expect_that(sum(is.na(ffs$date)), equals(0))

})

test_that("ice data is returned as a raster object", {
          expect_that(readfastice("2000-06-01"), is_a("RasterLayer"))
      })

test_that("dates not available within 1.5 days give error", {
    expect_that(readfastice("1975-10-18"), throws_error("no data file within"))
})

test_that("fastice projection is not missing", {
  prj <- projection(readfastice())
  expect_false(is.na(prj))

})
