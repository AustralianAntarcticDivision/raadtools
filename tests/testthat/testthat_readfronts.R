require(testthat)
require(raadtools)



test_that("reqeusted files only are returned as a data.frame", {
    ffs <- readfronts(returnfiles = TRUE)
    expect_that(ffs, is_a("data.frame"))

    expect_that(all(names(ffs) %in% c("date", "file", "fullname")), is_true())
    expect_that(all(file.exists(ffs$fullname[sample(nrow(ffs), 100)])), is_true())
    expect_that(sum(is.na(ffs$date)), equals(0))

})

test_that("ice data is returned as a raster object", {
          expect_that(readfronts("2000-06-01"), is_a("RasterLayer"))
      })

test_that("dates not available within 1.5 days give error", {
    expect_that(readfronts("1975-10-18"), throws_error("no data file within"))
})

