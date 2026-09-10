


files <- amps_d1files()
aurora$DATE_TIME_UTC <- aurora$DATE_TIME_UTC + 24 * 3 * 3600 * 365
library(dplyr)
test_that("file dates are unique", {
    expect_true(min(diff(unclass(files$date))) > 0)
  expect_equal(nrow(distinct(files, date)), nrow(files))
  expect_equal(nrow(distinct(files, fullname)), nrow(files))
})

