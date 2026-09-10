
dts <- c("2001-01-02", NA, "2009-10-01", "2000-01-01")

test_that("processed dates as expected", {
    suppressMessages( suppressWarnings(expect_error(.processDates(dts, sort(dts[!is.na(dts)]), "awful"))))


})
test_that("date validation is correct", {
    expect_s3_class(.valiDates(dts[!is.na(dts)], allOK = TRUE), "POSIXct")
    expect_error(.valiDates(dts, allOK = TRUE))
    suppressWarnings(expect_s3_class(.valiDates(dts, allOK = FALSE), "POSIXct"))
    suppressWarnings(expect_equal(length(.valiDates(dts, allOK = FALSE)), sum(!is.na(dts))))
})


test_that("date sorting is correct", {
  expect_warning(dts_posix <- .valiDates(dts, allOK = FALSE))
    expect_error(.sortDates(dts_posix, resortOK = FALSE))

    suppressWarnings(expect_s3_class(.sortDates(dts_posix, resortOK = TRUE), "POSIXct") )
    suppressWarnings(expect_true(all(diff(.sortDates(dts_posix, resortOK = TRUE)) > 0)))
})
