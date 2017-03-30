
context("AMPS data")
library(testthat)
#library(raadtools)

files <- amps_d1files()
aurora$DATE_TIME_UTC <- aurora$DATE_TIME_UTC + 24 * 3 * 3600 * 365

amps_ice <- extract(readamps_d1ice, aurora[24:38, ])
test_that("file dates are unique", {
    expect_true(min(diff(unclass(files$date))) > 0)
  expect_that(nrow(distinct(files, date)), equals(nrow(files)))
  expect_that(nrow(distinct(files, fullname)), equals(nrow(files)))
})

