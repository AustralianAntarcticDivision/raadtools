# tests/testthat/test-read-mld.R
# Tests for terra-native MLD climatology reader

skip_if_no_mld <- function() {
  tryCatch({
    f <- raadfiles::get_raad_filenames(all = TRUE) %>%
      dplyr::filter(stringr::str_detect(file, "sallee_mld2013.Rdata"))
    if (nrow(f) == 0) skip("MLD climatology file not found")
  }, error = function(e) {
    skip(paste("MLD not available:", e$message))
  })
}

test_that("read_mld_climatology returns SpatRaster", {
  skip_if_no_mld()

  r <- read_mld_climatology()

  expect_s4_class(r, "SpatRaster")
})

test_that("read_mld_climatology returns 12 layers by default", {
  skip_if_no_mld()

  r <- read_mld_climatology()

  expect_equal(terra::nlyr(r), 12L)
  expect_equal(names(r), month.abb)
})

test_that("read_mld_climatology subsets by month", {
  skip_if_no_mld()

  # Single month
  r <- read_mld_climatology("2020-01-15")
  expect_equal(terra::nlyr(r), 1L)
  expect_equal(names(r), "Jan")

  # Multiple months
  r <- read_mld_climatology(c("2020-01-01", "2020-06-01", "2020-12-01"))
  expect_equal(terra::nlyr(r), 3L)
  expect_true(all(names(r) %in% c("Jan", "Jun", "Dec")))
})

test_that("read_mld_climatology crop works", {
  skip_if_no_mld()

  bounds <- c(100, 150, -70, -50)
  r <- read_mld_climatology(xylim = bounds)

  ext <- as.vector(terra::ext(r))
  expect_true(ext[1] >= bounds[1] - 1)
  expect_true(ext[2] <= bounds[2] + 1)
})

test_that("read_mld_climatology returnfiles works", {
  skip_if_no_mld()

  files <- read_mld_climatology(returnfiles = TRUE)

  expect_s3_class(files, "tbl_df")
  expect_equal(nrow(files), 12L)
  expect_true("month" %in% names(files))
})

test_that("readmld shim returns RasterBrick", {
  skip_if_no_mld()

  withr::with_options(list(raadtools.shim.warn = FALSE), {
    expect_warning(r <- readmld(), "climatology")
  })

  expect_s4_class(r, "RasterBrick")
})
