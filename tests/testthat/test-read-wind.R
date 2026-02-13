# tests/testthat/test-read-wind.R
# Tests for terra-native NCEP2 wind readers

skip_if_no_raad <- function() {
  tryCatch({
    roots <- raadfiles::get_raad_data_roots()
    if (length(roots) == 0) skip("raad data roots not configured")
    files <- windfiles()
    if (nrow(files) == 0) skip("no wind files found")
  }, error = function(e) {
    skip(paste("raad data not available:", e$message))
  })
}

# =============================================================================
# read_ncep2_wind_6hourly() tests
# =============================================================================

test_that("read_ncep2_wind_6hourly returns SpatRaster with U and V", {
  skip_if_no_raad()

  r <- read_ncep2_wind_6hourly("2020-01-15")

  expect_s4_class(r, "SpatRaster")
  expect_equal(terra::nlyr(r), 2L)
  expect_equal(names(r), c("U", "V"))
})

test_that("read_ncep2_wind_6hourly magonly returns single layer", {
  skip_if_no_raad()

  r <- read_ncep2_wind_6hourly("2020-01-15", magonly = TRUE)

  expect_s4_class(r, "SpatRaster")
  expect_equal(terra::nlyr(r), 1L)

  # Values should be non-negative (it's a magnitude)
  vals <- terra::values(r)
  vals <- vals[!is.na(vals)]
  expect_true(all(vals >= 0))
})

test_that("read_ncep2_wind_6hourly dironly returns direction 0-360", {
  skip_if_no_raad()

  r <- read_ncep2_wind_6hourly("2020-01-15", dironly = TRUE)

  expect_s4_class(r, "SpatRaster")
  expect_equal(terra::nlyr(r), 1L)

  vals <- terra::values(r)
  vals <- vals[!is.na(vals)]
  expect_true(all(vals >= 0))
  expect_true(all(vals <= 360))
})

test_that("read_ncep2_wind_6hourly uonly returns single layer", {
  skip_if_no_raad()

  r <- read_ncep2_wind_6hourly("2020-01-15", uonly = TRUE)

  expect_s4_class(r, "SpatRaster")
  expect_equal(terra::nlyr(r), 1L)
})

test_that("read_ncep2_wind_6hourly vonly returns single layer", {
  skip_if_no_raad()

  r <- read_ncep2_wind_6hourly("2020-01-15", vonly = TRUE)

  expect_s4_class(r, "SpatRaster")
  expect_equal(terra::nlyr(r), 1L)
})

test_that("read_ncep2_wind_6hourly rejects multiple flags", {
  skip_if_no_raad()

  expect_error(
    read_ncep2_wind_6hourly("2020-01-15", magonly = TRUE, dironly = TRUE),
    "only one of"
  )
})

test_that("read_ncep2_wind_6hourly handles multiple dates with component flag", {
  skip_if_no_raad()

  # Two 6-hourly timestamps
  dates <- c("2020-01-15 00:00", "2020-01-15 06:00")
  r <- read_ncep2_wind_6hourly(dates, magonly = TRUE)

  expect_equal(terra::nlyr(r), 2L)
})

test_that("read_ncep2_wind_6hourly warns on multiple dates without component flag", {
  skip_if_no_raad()

  dates <- c("2020-01-15 00:00", "2020-01-15 06:00")

  expect_warning(
    r <- read_ncep2_wind_6hourly(dates),
    "only one time"
  )
  expect_equal(terra::nlyr(r), 2L)  # U and V for single timestep
})

test_that("read_ncep2_wind_6hourly crop works", {
  skip_if_no_raad()

  bounds <- c(100, 150, -60, -40)
  r <- read_ncep2_wind_6hourly("2020-01-15", xylim = bounds, uonly = TRUE)

  ext <- as.vector(terra::ext(r))
  # Allow for grid cell alignment
  expect_true(ext[1] >= bounds[1] - 5)
  expect_true(ext[2] <= bounds[2] + 5)
  expect_true(ext[3] >= bounds[3] - 5)
  expect_true(ext[4] <= bounds[4] + 5)
})

test_that("read_ncep2_wind_6hourly returnfiles returns tibble with band column", {
  skip_if_no_raad()

  files <- read_ncep2_wind_6hourly(returnfiles = TRUE)

  expect_s3_class(files, "tbl_df")
  expect_true("date" %in% names(files))
  expect_true("ufullname" %in% names(files))
  expect_true("vfullname" %in% names(files))
  expect_true("band" %in% names(files))
})

test_that("read_ncep2_wind_6hourly latest argument works", {
  skip_if_no_raad()

  files <- read_ncep2_wind_6hourly(returnfiles = TRUE)

  r_latest <- read_ncep2_wind_6hourly(latest = TRUE, uonly = TRUE)
  r_earliest <- read_ncep2_wind_6hourly(latest = FALSE, uonly = TRUE)

  # Times should differ
  expect_false(terra::time(r_latest) == terra::time(r_earliest))
})

# =============================================================================
# Shim tests
# =============================================================================

test_that("readwind shim returns Raster object", {
  skip_if_no_raad()

  withr::with_options(list(raadtools.shim.warn = FALSE), {
    r <- readwind("2020-01-15")
  })

  expect_s4_class(r, "Raster")
  expect_equal(raster::nlayers(r), 2L)
})

test_that("readwind shim magonly works", {
  skip_if_no_raad()

  withr::with_options(list(raadtools.shim.warn = FALSE), {
    r <- readwind("2020-01-15", magonly = TRUE)
  })

  expect_s4_class(r, "Raster")
  expect_equal(raster::nlayers(r), 1L)
})
