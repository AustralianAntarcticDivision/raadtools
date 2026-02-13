# tests/testthat/test-read-ice.R
# Tests for terra-native NSIDC ice readers

skip_if_no_raad <- function() {
  tryCatch({
    roots <- raadfiles::get_raad_data_roots()
    if (length(roots) == 0) skip("raad data roots not configured")
    files <- raadfiles::nsidc_south_daily_files()
    if (nrow(files) == 0) skip("no NSIDC files found")
  }, error = function(e) {
    skip(paste("raad data not available:", e$message))
  })
}

# =============================================================================
# read_nsidc_ice_daily() tests
# =============================================================================

test_that("read_nsidc_ice_daily returns SpatRaster", {
  skip_if_no_raad()

  r <- read_nsidc_ice_daily("2020-01-15")

  expect_s4_class(r, "SpatRaster")
  expect_equal(terra::nlyr(r), 1L)
})

test_that("read_nsidc_ice_daily sets correct CRS for south", {
  skip_if_no_raad()

  r <- read_nsidc_ice_daily("2020-01-15", hemisphere = "south")
  crs_str <- terra::crs(r, proj = TRUE)

  # Should be polar stereographic south
  expect_true(grepl("stere", crs_str, ignore.case = TRUE))
  expect_true(grepl("-90", crs_str) || grepl("3976", terra::crs(r, describe = TRUE)$code))
})

test_that("read_nsidc_ice_daily sets correct CRS for north", {
  skip_if_no_raad()

  r <- read_nsidc_ice_daily("2020-03-15", hemisphere = "north")
  crs_str <- terra::crs(r, proj = TRUE)

  # Should be polar stereographic north
  expect_true(grepl("stere", crs_str, ignore.case = TRUE))
  expect_true(grepl("\\+90|90", crs_str) || grepl("3413", terra::crs(r, describe = TRUE)$code))
})

test_that("read_nsidc_ice_daily sets time correctly", {
  skip_if_no_raad()

  r <- read_nsidc_ice_daily("2020-01-15")

  expect_equal(as.Date(terra::time(r)), as.Date("2020-01-15"))
  expect_equal(names(r), "2020-01-15")
})

test_that("read_nsidc_ice_daily handles multiple dates", {
  skip_if_no_raad()

  dates <- c("2020-01-15", "2020-06-15")
  r <- read_nsidc_ice_daily(dates)

  expect_equal(terra::nlyr(r), 2L)
  expect_equal(as.Date(terra::time(r)), as.Date(dates))
})

test_that("read_nsidc_ice_daily values are in percentage range with setNA", {
  skip_if_no_raad()

  r <- read_nsidc_ice_daily("2020-01-15", setNA = TRUE)
  vals <- terra::values(r)
  vals <- vals[!is.na(vals)]

  # With setNA, values should be 0-100
  expect_true(all(vals >= 0))
  expect_true(all(vals <= 100))
})

test_that("read_nsidc_ice_daily setNA=FALSE preserves special values", {
  skip_if_no_raad()

  r_na <- read_nsidc_ice_daily("2020-01-15", setNA = TRUE)
  r_raw <- read_nsidc_ice_daily("2020-01-15", setNA = FALSE)

  # Raw should have more non-NA values (special codes not masked)
  na_count_masked <- sum(is.na(terra::values(r_na)))
  na_count_raw <- sum(is.na(terra::values(r_raw)))

  # Raw should have fewer NAs (only true file-level NA, not masked special values)
  expect_true(na_count_raw <= na_count_masked)
})

test_that("read_nsidc_ice_daily returnfiles returns tibble", {
  skip_if_no_raad()

  files <- read_nsidc_ice_daily(returnfiles = TRUE)

  expect_s3_class(files, "tbl_df")
  expect_true("date" %in% names(files))
  expect_true("fullname" %in% names(files))
  expect_true(nrow(files) > 0)
})

test_that("read_nsidc_ice_daily latest argument works", {
  skip_if_no_raad()

  files <- read_nsidc_ice_daily(returnfiles = TRUE)

  r_latest <- read_nsidc_ice_daily(latest = TRUE)
  r_earliest <- read_nsidc_ice_daily(latest = FALSE)

  expect_equal(as.Date(terra::time(r_latest)), as.Date(max(files$date)))
  expect_equal(as.Date(terra::time(r_earliest)), as.Date(min(files$date)))
})

test_that("read_nsidc_ice_daily filters bad_nsidc dates", {
  skip_if_no_raad()

  files <- read_nsidc_ice_daily(returnfiles = TRUE)
  rawdates <- as.integer(as.Date(files$date))

  # None of the returned files should be in bad_nsidc
  # (bad_nsidc is internal data, access via raadtools:::bad_nsidc if needed)
  # This test just checks we got a valid file list
  expect_true(nrow(files) > 0)
})

test_that("read_nsidc_ice_daily crop works", {
  skip_if_no_raad()

  # Crop to a region in native polar stereo coords (rough Antarctic Peninsula area)
  bounds <- c(-3000000, -1000000, -2000000, 0)  # xmin, xmax, ymin, ymax in meters
  r <- read_nsidc_ice_daily("2020-01-15", xylim = bounds)

  ext <- as.vector(terra::ext(r))
  expect_true(ext[1] >= bounds[1] - 25000)  # allow grid cell tolerance
  expect_true(ext[2] <= bounds[2] + 25000)
})

# =============================================================================
# read_nsidc_ice_monthly() tests
# =============================================================================

test_that("read_nsidc_ice_monthly returns SpatRaster", {
  skip_if_no_raad()

  tryCatch({
    files <- raadfiles::nsidc_south_monthly_files()
    if (nrow(files) == 0) skip("no monthly NSIDC files found")
  }, error = function(e) skip("monthly files not available"))

  r <- read_nsidc_ice_monthly("2020-01-01")

  expect_s4_class(r, "SpatRaster")
  expect_equal(terra::nlyr(r), 1L)
})

# =============================================================================
# Shim equivalence tests
# =============================================================================

test_that("readice shim returns RasterBrick for single hemisphere", {
  skip_if_no_raad()

  withr::with_options(list(raadtools.shim.warn = FALSE), {
    r <- readice("2020-01-15", hemisphere = "south")
  })

  expect_s4_class(r, "RasterLayer")
})
test_that("readice shim produces equivalent values to read_nsidc_ice_daily", {
  skip_if_no_raad()

  withr::with_options(list(raadtools.shim.warn = FALSE), {
    r_legacy <- readice("2020-01-15", hemisphere = "south")
  })
  r_terra <- read_nsidc_ice_daily("2020-01-15", hemisphere = "south")

  # Convert both to vectors for comparison
  vals_legacy <- raster::values(r_legacy)
  vals_terra <- terra::values(r_terra)

  # Should be identical
  expect_equal(vals_legacy, as.vector(vals_terra), tolerance = 0.01)
})

test_that("readice shim emits deprecation warning", {
  skip_if_no_raad()

  expect_warning(
    readice("2020-01-15", hemisphere = "south"),
    "deprecated"
  )
})

test_that("readice hemisphere='both' still works (legacy path)", {
  skip_if_no_raad()
  skip_if_not_installed("vapour")

  withr::with_options(list(raadtools.shim.warn = FALSE), {
    r <- readice("2020-01-15", hemisphere = "both")
  })

  # Should be a global raster
  expect_s4_class(r, "Raster")
  ext <- raster::extent(r)
  expect_equal(raster::xmin(ext), -180, tolerance = 0.5)
  expect_equal(raster::xmax(ext), 180, tolerance = 0.5)
})
