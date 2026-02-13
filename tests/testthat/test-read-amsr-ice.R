# tests/testthat/test-read-amsr-ice.R
# Tests for terra-native AMSR and CERSAT ice readers

skip_if_no_raad <- function() {
  tryCatch({
    roots <- raadfiles::get_raad_data_roots()
    if (length(roots) == 0) skip("raad data roots not configured")
  }, error = function(e) {
    skip(paste("raad data not available:", e$message))
  })
}

skip_if_no_amsr <- function() {
  skip_if_no_raad()
  tryCatch({
    files <- raadfiles::amsr_daily_files()
    if (nrow(files) == 0) skip("no AMSR files found")
  }, error = function(e) {
    skip(paste("AMSR files not available:", e$message))
  })
}

skip_if_no_amsr_3k <- function() {
  skip_if_no_raad()
  tryCatch({
    files <- raadfiles::amsr2_3k_daily_files()
    if (nrow(files) == 0) skip("no AMSR2 3k files found")
  }, error = function(e) {
    skip(paste("AMSR2 3k files not available:", e$message))
  })
}

skip_if_no_cersat <- function() {
  skip_if_no_raad()
  tryCatch({
    files <- raadfiles::cersat_daily_files()
    if (nrow(files) == 0) skip("no CERSAT files found")
  }, error = function(e) {
    skip(paste("CERSAT files not available:", e$message))
  })
}

# =============================================================================
# AMSR 6km tests
# =============================================================================

test_that("read_amsr_ice_daily returns SpatRaster", {
  skip_if_no_amsr()

  r <- read_amsr_ice_daily()

  expect_s4_class(r, "SpatRaster")
  expect_equal(terra::nlyr(r), 1L)
})

test_that("read_amsr_ice_daily has correct Antarctic extent", {
  skip_if_no_amsr()

  r <- read_amsr_ice_daily()
  ext <- as.vector(terra::ext(r))

  expect_equal(ext[1], -3950000)
  expect_equal(ext[2], 3950000)
  expect_equal(ext[3], -3950000)
  expect_equal(ext[4], 4350000)
})

test_that("read_amsr_ice_daily has polar stereographic CRS", {
  skip_if_no_amsr()

  r <- read_amsr_ice_daily()
  crs_str <- terra::crs(r, proj = TRUE)

  expect_true(grepl("stere", crs_str))
  expect_true(grepl("lat_0=-90", crs_str))
})

test_that("read_amsr_ice_daily values are 0-100 percentage", {
  skip_if_no_amsr()

  r <- read_amsr_ice_daily()
  vals <- terra::values(r)
  vals <- vals[!is.na(vals)]

  expect_true(all(vals >= 0))
  expect_true(all(vals <= 100))
})

test_that("read_amsr_ice_daily handles multiple dates", {
  skip_if_no_amsr()

  files <- raadfiles::amsr_daily_files()
  dates <- head(files$date, 3)
  r <- read_amsr_ice_daily(dates)

  expect_equal(terra::nlyr(r), 3L)
})

test_that("read_amsr_ice_daily crop works", {
  skip_if_no_amsr()

  bounds <- c(-1000000, 1000000, -1000000, 1000000)
  r <- read_amsr_ice_daily(xylim = bounds)

  ext <- as.vector(terra::ext(r))
  expect_true(ext[1] >= bounds[1] - 10000)
  expect_true(ext[2] <= bounds[2] + 10000)
})

test_that("read_amsr_ice alias works", {
  skip_if_no_amsr()

  r <- read_amsr_ice()
  expect_s4_class(r, "SpatRaster")
})

# =============================================================================
# AMSR2 3km tests
# =============================================================================

test_that("read_amsr_ice_3k_daily returns SpatRaster", {
  skip_if_no_amsr_3k()

  r <- read_amsr_ice_3k_daily()

  expect_s4_class(r, "SpatRaster")
  expect_equal(terra::nlyr(r), 1L)
})

test_that("read_amsr_ice_3k_daily has higher resolution than 6k", {
  skip_if_no_amsr_3k()

  r <- read_amsr_ice_3k_daily()
  res <- terra::res(r)

  # 3.125km = 3125m
  expect_equal(res[1], 3125, tolerance = 10)
})

test_that("read_amsr_ice_3k_daily setNA masks high values", {
  skip_if_no_amsr_3k()

  r_na <- read_amsr_ice_3k_daily(setNA = TRUE)
  r_raw <- read_amsr_ice_3k_daily(setNA = FALSE)

  # With setNA, max should be <= 100
  max_na <- terra::global(r_na, "max", na.rm = TRUE)$max
  expect_true(max_na <= 100)
})

test_that("read_amsr2_3k_ice alias works", {
  skip_if_no_amsr_3k()

  r <- read_amsr2_3k_ice()
  expect_s4_class(r, "SpatRaster")
})

# =============================================================================
# CERSAT tests
# =============================================================================

test_that("read_cersat_ice_daily returns SpatRaster", {
  skip_if_no_cersat()

  r <- read_cersat_ice_daily()

  expect_s4_class(r, "SpatRaster")
  expect_equal(terra::nlyr(r), 1L)
})

test_that("read_cersat_ice_daily has correct extent", {
  skip_if_no_cersat()

  r <- read_cersat_ice_daily()
  ext <- as.vector(terra::ext(r))

  expect_equal(ext[1], -3950000)
  expect_equal(ext[2], 3950000)
})

test_that("read_cersat_ice_daily setNA masks high values", {
  skip_if_no_cersat()

  r <- read_cersat_ice_daily(setNA = TRUE)
  max_val <- terra::global(r, "max", na.rm = TRUE)$max

  expect_true(max_val <= 100)
})

test_that("read_cersat_ice alias works", {
  skip_if_no_cersat()

  r <- read_cersat_ice()
  expect_s4_class(r, "SpatRaster")
})

# =============================================================================
# returnfiles tests
# =============================================================================

test_that("read_amsr_ice_daily returnfiles returns tibble", {
  skip_if_no_amsr()

  files <- read_amsr_ice_daily(returnfiles = TRUE)

  expect_s3_class(files, "tbl_df")
  expect_true("date" %in% names(files))
  expect_true("fullname" %in% names(files))
})

test_that("read_amsr_ice_3k_daily returnfiles returns tibble", {
  skip_if_no_amsr_3k()

  files <- read_amsr_ice_3k_daily(returnfiles = TRUE)

  expect_s3_class(files, "tbl_df")
})

test_that("read_cersat_ice_daily returnfiles returns tibble", {
  skip_if_no_cersat()

  files <- read_cersat_ice_daily(returnfiles = TRUE)

  expect_s3_class(files, "tbl_df")
})
