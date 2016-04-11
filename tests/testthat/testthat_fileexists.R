context("file exist")

require(testthat)

require(raadtools)
op <- getOption("default.datadir")
options(default.datadir = "/bogus/path/data")
test_that("file is not found", {


    expect_that(raadtools:::.updateicefiles(), throws_error())

}
)

options(default.datadir = op)

