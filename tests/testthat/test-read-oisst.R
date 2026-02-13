# tests/testthat/test-read-oisst.R
# Tests for terra-native OISST readers

# Helper to check if raad data is available
skip_if_no_raad <- function() {
  # Check if raadfiles can find data roots
  tryCatch({
    roots <- raadfiles::get_raad_data_roots()
    if (length(roots) == 0) skip("raad data roots not configured")
    files <- raadfiles::oisst_daily_files()
    if (nrow(files) == 0) skip("no OISST files found")
  }, error = function(e) {
    skip(paste("raad data not available:", e$message))
  })
}

# =============================================================================
# read_oisst_daily() tests
# =============================================================================

test_that("read_oisst_daily returns SpatRaster", {
  skip_if_no_raad()

  r <- read_oisst_daily("2020-01-15")

  expect_s4_class(r, "SpatRaster")
  expect_equal(terra::nlyr(r), 1L)
  expect_false(is.na(terra::crs(r, proj = TRUE)))
})

test_that("read_oisst_daily sets time correctly", {
 skip_if_no_raad()

  r <- read_oisst_daily("2020-01-15")

  expect_equal(as.Date(terra::time(r)), as.Date("2020-01-15"))
  expect_equal(names(r), "2020-01-15")
})

test_that("read_oisst_daily handles multiple dates", {
  skip_if_no_raad()

  dates <- c("2020-01-15", "2020-06-15")
  r <- read_oisst_daily(dates)

  expect_equal(terra::nlyr(r), 2L)
  expect_equal(as.Date(terra::time(r)), as.Date(dates))
})

test_that("read_oisst_daily lon180 rotation works", {
  skip_if_no_raad()

  r_pacific <- read_oisst_daily("2020-01-15", lon180 = FALSE)
  r_atlantic <- read_oisst_daily("2020-01-15", lon180 = TRUE)

  ext_pac <- as.vector(terra::ext(r_pacific))
  ext_atl <- as.vector(terra::ext(r_atlantic))

  # Pacific: 0 to 360 (or close to it)
  expect_true(ext_pac[1] >= -0.5)
  expect_true(ext_pac[2] <= 360.5)

  # Atlantic: -180 to 180 (or close to it)
  expect_true(ext_atl[1] >= -180.5)
  expect_true(ext_atl[2] <= 180.5)
})

test_that("read_oisst_daily crop works", {
  skip_if_no_raad()

  bounds <- c(100, 150, -60, -40)  # xmin, xmax, ymin, ymax
  r <- read_oisst_daily("2020-01-15", xylim = bounds)

  ext <- as.vector(terra::ext(r))
  # Allow small tolerance for grid alignment
  expect_true(ext[1] >= bounds[1] - 0.5)
  expect_true(ext[2] <= bounds[2] + 0.5)
  expect_true(ext[3] >= bounds[3] - 0.5)
  expect_true(ext[4] <= bounds[4] + 0.5)
})

test_that("read_oisst_daily varname selection works", {
  skip_if_no_raad()

  sst <- read_oisst_daily("2020-01-15", varname = "sst")
  anom <- read_oisst_daily("2020-01-15", varname = "anom")
  ice <- read_oisst_daily("2020-01-15", varname = "ice")

  # Each should return data
  expect_s4_class(sst, "SpatRaster")
  expect_s4_class(anom, "SpatRaster")
  expect_s4_class(ice, "SpatRaster")

  # Values should differ (at least in range)
  expect_false(identical(
    range(terra::values(sst), na.rm = TRUE),
    range(terra::values(ice), na.rm = TRUE)
  ))
})

test_that("read_oisst_daily returnfiles returns tibble", {
  skip_if_no_raad()

  files <- read_oisst_daily(returnfiles = TRUE)

  expect_s3_class(files, "tbl_df")
  expect_true("date" %in% names(files))
  expect_true("fullname" %in% names(files))
  expect_true(nrow(files) > 0)
})

test_that("read_oisst_daily latest argument works", {
  skip_if_no_raad()

  files <- read_oisst_daily(returnfiles = TRUE)

  r_latest <- read_oisst_daily(latest = TRUE)
  r_earliest <- read_oisst_daily(latest = FALSE)

  expect_equal(as.Date(terra::time(r_latest)), as.Date(max(files$date)))
  expect_equal(as.Date(terra::time(r_earliest)), as.Date(min(files$date)))
})

test_that("read_oisst_daily errors on dates too far from available", {
  skip_if_no_raad()

  # way before data starts (OISST starts 1981-09-01)
  expect_error(read_oisst_daily("1950-01-01"), "no data file within")
})

test_that("read_oisst_daily accepts Date and POSIXct", {
  skip_if_no_raad()

  # Character
  r1 <- read_oisst_daily("2020-01-15")

  # Date
  r2 <- read_oisst_daily(as.Date("2020-01-15"))

  # POSIXct
  r3 <- read_oisst_daily(as.POSIXct("2020-01-15", tz = "UTC"))

  # All should return equivalent results
  expect_equal(terra::nlyr(r1), terra::nlyr(r2))
  expect_equal(terra::nlyr(r1), terra::nlyr(r3))
  expect_equal(as.Date(terra::time(r1)), as.Date(terra::time(r2)))
})

# =============================================================================
# read_oisst_monthly() tests
# =============================================================================

test_that("read_oisst_monthly returns SpatRaster", {
  skip_if_no_raad()

  # Skip if no monthly files available
  tryCatch({
    files <- raadfiles::oisst_monthly_files()
    if (nrow(files) == 0) skip("no monthly OISST files found")
  }, error = function(e) skip("monthly files not available"))

  r <- read_oisst_monthly("2020-01-01")

  expect_s4_class(r, "SpatRaster")
  expect_equal(terra::nlyr(r), 1L)
})

test_that("read_oisst_monthly returnfiles returns tibble", {
  skip_if_no_raad()

  tryCatch({
    files <- read_oisst_monthly(returnfiles = TRUE)
    expect_s3_class(files, "tbl_df")
    expect_true(nrow(files) > 0)
  }, error = function(e) skip("monthly files not available"))
})

# =============================================================================
# Shim equivalence tests (readsst vs read_oisst_daily)
# =============================================================================

test_that("readsst shim returns RasterBrick", {
  skip_if_no_raad()

  # Suppress deprecation warning for test
  withr::with_options(list(raadtools.shim.warn = FALSE), {
    r <- readsst("2020-01-15")
  })

  expect_s4_class(r, "RasterLayer")
})

test_that("readsst shim produces equivalent values to read_oisst_daily", {
  skip_if_no_raad()

  withr::with_options(list(raadtools.shim.warn = FALSE), {
    r_legacy <- readsst("2020-01-15")
  })
  r_terra <- read_oisst_daily("2020-01-15")

  # Convert both to matrix for comparison
  vals_legacy <- raster::values(r_legacy)
  vals_terra <- terra::values(r_terra)

  # Should be identical (or very close)
  expect_equal(vals_legacy, vals_terra, tolerance = 1e-6)
})

test_that("readsst shim preserves Z values", {
  skip_if_no_raad()

  dates <- c("2020-01-15", "2020-06-15")

  withr::with_options(list(raadtools.shim.warn = FALSE), {
    r_legacy <- readsst(dates)
  })
  r_terra <- read_oisst_daily(dates)

  # Compare time/Z values
  z_legacy <- raster::getZ(r_legacy)
  z_terra <- terra::time(r_terra)

  expect_equal(as.Date(z_legacy), as.Date(z_terra))
})

test_that("readsst shim emits deprecation warning by default", {
  skip_if_no_raad()

  expect_warning(
    readsst("2020-01-15"),
    "deprecated"
  )
})

test_that("readsst shim warning can be suppressed", {
  skip_if_no_raad()

  expect_silent({
    withr::with_options(list(raadtools.shim.warn = FALSE), {
      r <- readsst("2020-01-15")
    })
  })
})
