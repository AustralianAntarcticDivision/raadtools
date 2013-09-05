library(testthat)
library(raadtools)

dts <- c("2001-01-02", NA, "2009-10-01", "2000-01-01")
test_that("date validation is correct", {
    expect_that(raadtools:::.valiDates(dts[!is.na(dts)], allOK = TRUE), is_a("POSIXct"))
    expect_that(raadtools:::.valiDates(dts, allOK = TRUE), throws_error())
    expect_that(raadtools:::.valiDates(dts, allOK = FALSE), is_a("POSIXct"))
    expect_that(length(raadtools:::.valiDates(dts, allOK = FALSE)), equals(sum(!is.na(dts))))


})

dts_posix <- raadtools:::.valiDates(dts, allOK = FALSE)
test_that("date sorting is correct", {
    expect_that(raadtools:::.sortDates(dts_posix, resortOK = FALSE), throws_error())

    expect_that(raadtools:::.sortDates(dts_posix, resortOK = TRUE), is_a("POSIXct"))
    expect_that(all(diff(raadtools:::.sortDates(dts_posix, resortOK = TRUE)) > 0), is_true())
})
