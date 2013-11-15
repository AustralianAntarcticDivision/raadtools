library(testthat)
library(raadtools)

data(aurora)
aurora$DATE_TIME_UTC <- aurora$DATE_TIME_UTC - 720 * 24 * 3600
test_that("we get values", {
    expect_that(extract(readchla, aurora), is_a("numeric"))
    expect_that(extract(readwind, aurora, magonly = TRUE), is_a("numeric"))
    expect_that(extract(readprod, aurora), is_a("numeric"))
})
