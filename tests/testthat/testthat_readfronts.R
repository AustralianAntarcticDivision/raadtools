context("fronts")


test_that("requested files only are returned as a data.frame", {
    ffs <- readfronts(returnfiles = TRUE)
    expect_s3_class(ffs, "data.frame")

    expect_true(all(c("date", "band", "fullname") %in% names(ffs)))
    expect_true(all(file.exists(ffs$fullname[sample(nrow(ffs), 100)])))
    expect_equal(sum(is.na(ffs$date)), 0)

})

test_that("fronts data is returned as a raster object", {
          expect_s4_class(readfronts("2000-06-01"), "RasterLayer")
      })

test_that("dates not available within 1.5 days give error", {
    expect_error(readfronts("1975-10-18"), "no data file within")
})

test_that("fronts projection is not missing", {
  prj <- projection(readfronts())
  expect_true(!is.na(prj))
  
})
