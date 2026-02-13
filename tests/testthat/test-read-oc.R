# tests/testthat/test-read-oc.R
# Tests for terra-native ocean colour readers

skip_if_no_raad <- function() {
  tryCatch({
    roots <- raadfiles::get_raad_data_roots()
    if (length(roots) == 0) skip("raad data roots not configured")
  }, error = function(e) {
    skip(paste("raad data not available:", e$message))
  })
}

skip_if_no_oc <- function(time.resolution = "daily") {
  skip_if_no_raad()
  tryCatch({
    files <- ocfiles(time.resolution, product = "MODISA", varname = "CHL", type = "L3m")
    if (nrow(files) == 0) skip("no ocean colour files found")
  }, error = function(e) {
    skip(paste("ocean colour files not available:", e$message))
  })
}

skip_if_no_par <- function() {
  skip_if_no_raad()
  tryCatch({
    files <- raadfiles::par_files()
    if (nrow(files) == 0) skip("no PAR files found")
  }, error = function(e) {
    skip(paste("PAR files not available:", e$message))
  })
}

# =============================================================================
# Chlorophyll daily tests
# =============================================================================

test_that("read_oc_chl_daily returns SpatRaster", {
  skip_if_no_oc("daily")

  r <- read_oc_chl_daily()

  expect_s4_class(r, "SpatRaster")
  expect_equal(terra::nlyr(r), 1L)
})

test_that("read_oc_chl_daily has correct global extent", {
  skip_if_no_oc("daily")

  r <- read_oc_chl_daily()
  ext <- as.vector(terra::ext(r))

  expect_equal(ext[1], -180)
  expect_equal(ext[2], 180)
  expect_equal(ext[3], -90)
  expect_equal(ext[4], 90)
})

test_that("read_oc_chl_daily handles multiple dates", {
  skip_if_no_oc("daily")

  files <- read_oc_chl_daily(returnfiles = TRUE)
  dates <- head(files$date, 3)
  r <- read_oc_chl_daily(dates)

  expect_equal(terra::nlyr(r), 3L)
})

test_that("read_oc_chl_daily crop works", {
  skip_if_no_oc("daily")

  bounds <- c(100, 150, -60, -40)
  r <- read_oc_chl_daily(xylim = bounds)

  ext <- as.vector(terra::ext(r))
  expect_true(ext[1] >= bounds[1] - 0.1)
  expect_true(ext[2] <= bounds[2] + 0.1)
})

test_that("read_oc_chl_daily returnfiles returns tibble", {
  skip_if_no_oc("daily")

  files <- read_oc_chl_daily(returnfiles = TRUE)

  expect_s3_class(files, "tbl_df")
  expect_true("date" %in% names(files))
  expect_true("fullname" %in% names(files))
})

test_that("read_oc_chl_daily latest/earliest works", {
  skip_if_no_oc("daily")

  files <- read_oc_chl_daily(returnfiles = TRUE)

  r_latest <- read_oc_chl_daily(latest = TRUE)
  r_earliest <- read_oc_chl_daily(latest = FALSE)

  expect_equal(as.Date(terra::time(r_latest)), as.Date(max(files$date)))
  expect_equal(as.Date(terra::time(r_earliest)), as.Date(min(files$date)))
})

# =============================================================================
# Chlorophyll 8-day and monthly tests
# =============================================================================

test_that("read_oc_chl_8day returns SpatRaster", {
  skip_if_no_oc("weekly")

  r <- read_oc_chl_8day()

  expect_s4_class(r, "SpatRaster")
})

test_that("read_oc_chl_monthly returns SpatRaster", {
  skip_if_no_oc("monthly")

  r <- read_oc_chl_monthly()

  expect_s4_class(r, "SpatRaster")
})

# =============================================================================
# PAR tests
# =============================================================================

test_that("read_oc_par_8day returns SpatRaster", {
  skip_if_no_par()

  r <- read_oc_par_8day()

  expect_s4_class(r, "SpatRaster")
  expect_equal(terra::nlyr(r), 1L)
})

test_that("read_oc_par_8day crop works", {
  skip_if_no_par()

  bounds <- c(100, 150, -60, -40)
  r <- read_oc_par_8day(xylim = bounds)

  ext <- as.vector(terra::ext(r))
  expect_true(ext[1] >= bounds[1] - 0.1)
  expect_true(ext[2] <= bounds[2] + 0.1)
})

# =============================================================================
# Legacy shim tests
# =============================================================================

test_that("read_chla_daily shim returns Raster", {
  skip_if_no_oc("daily")

  withr::with_options(list(raadtools.shim.warn = FALSE), {
    r <- read_chla_daily()
  })

  expect_s4_class(r, "Raster")
})

test_that("read_chla_weekly shim returns Raster", {
  skip_if_no_oc("weekly")

  withr::with_options(list(raadtools.shim.warn = FALSE), {
    r <- read_chla_weekly()
  })

  expect_s4_class(r, "Raster")
})

test_that("read_par shim returns Raster", {
  skip_if_no_par()

  withr::with_options(list(raadtools.shim.warn = FALSE), {
    r <- read_par()
  })

  expect_s4_class(r, "Raster")
})

test_that("readCHL_month is defunct", {
  expect_error(readCHL_month(), "defunct")
})
