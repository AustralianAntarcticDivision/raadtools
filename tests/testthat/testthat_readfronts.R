context("fronts")


test_that("requested files only are returned as a data.frame", {
    ffs <- readfronts(returnfiles = TRUE)
    expect_that(ffs, is_a("data.frame"))

    expect_true(all(c("date", "band", "fullname") %in% names(ffs)))
    expect_true(all(file.exists(ffs$fullname[sample(nrow(ffs), 100)])))
    expect_that(sum(is.na(ffs$date)), equals(0))

})

test_that("fronts data is returned as a raster object", {
          expect_that(readfronts("2000-06-01"), is_a("RasterLayer"))
      })

test_that("dates not available within 1.5 days give error", {
    expect_that(readfronts("1975-10-18"), throws_error("no data file within"))
})

test_that("fronts projection is not missing", {
  prj <- projection(readfronts())
  expect_true(!is.na(prj))
  
})
