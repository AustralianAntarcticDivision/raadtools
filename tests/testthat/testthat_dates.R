
context("basic date")
library(testthat)
library(raadtools)

dts <- c("2001-01-02", NA, "2009-10-01", "2000-01-01")

test_that("processed dates as expected", {
    expect_error(.processDates(dts, sort(dts[!is.na(dts)]), "awful")) %>% expect_warning() %>% expect_message()


})
test_that("date validation is correct", {
    expect_that(.valiDates(dts[!is.na(dts)], allOK = TRUE), is_a("POSIXct")) 
    expect_that(.valiDates(dts, allOK = TRUE), throws_error())
    expect_that(.valiDates(dts, allOK = FALSE), is_a("POSIXct")) %>% expect_warning()
    expect_that(length(.valiDates(dts, allOK = FALSE)), equals(sum(!is.na(dts)))) %>% expect_warning() 
})


test_that("date sorting is correct", {
  expect_warning(dts_posix <- .valiDates(dts, allOK = FALSE))
    expect_that(.sortDates(dts_posix, resortOK = FALSE), throws_error())

    expect_that(.sortDates(dts_posix, resortOK = TRUE), is_a("POSIXct")) %>% expect_warning() 
    expect_true(all(diff(.sortDates(dts_posix, resortOK = TRUE)) > 0)) %>% expect_warning() 
})
