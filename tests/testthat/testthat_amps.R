
test_that("file dates are unique", {
  skip_if_no_raad()
  files <- amps_d1files()
  expect_true(min(diff(unclass(files$date))) > 0)
  expect_equal(nrow(dplyr::distinct(files, date)), nrow(files))
  expect_equal(nrow(dplyr::distinct(files, fullname)), nrow(files))
})
