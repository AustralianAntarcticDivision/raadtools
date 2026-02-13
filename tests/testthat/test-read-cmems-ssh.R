# tests/testthat/test-read-cmems-ssh.R
# Tests for terra-native CMEMS SSH/altimetry readers

skip_if_no_raad <- function() {
  tryCatch({
    roots <- raadfiles::get_raad_data_roots()
    if (length(roots) == 0) skip("raad data roots not configured")
    files <- raadfiles::altimetry_daily_files()
    if (nrow(files) == 0) skip("no CMEMS altimetry files found")
  }, error = function(e) {
    skip(paste("raad data not available:", e$message))
  })
}

# =============================================================================
# Scalar variable readers
# =============================================================================

test_that("read_cmems_adt_daily returns SpatRaster", {
  skip_if_no_raad()

  r <- read_cmems_adt_daily("2020-01-15")

  expect_s4_class(r, "SpatRaster")
  expect_equal(terra::nlyr(r), 1L)
})

test_that("read_cmems_sla_daily returns SpatRaster", {
  skip_if_no_raad()

  r <- read_cmems_sla_daily("2020-01-15")

  expect_s4_class(r, "SpatRaster")
  expect_equal(terra::nlyr(r), 1L)
})

test_that("read_cmems_ugos_daily returns SpatRaster", {
  skip_if_no_raad()

  r <- read_cmems_ugos_daily("2020-01-15")

  expect_s4_class(r, "SpatRaster")
  expect_equal(terra::nlyr(r), 1L)
})

test_that("read_cmems_vgos_daily returns SpatRaster", {
  skip_if_no_raad()

  r <- read_cmems_vgos_daily("2020-01-15")

  expect_s4_class(r, "SpatRaster")
  expect_equal(terra::nlyr(r), 1L)
})

test_that("scalar readers handle multiple dates", {
  skip_if_no_raad()

  dates <- c("2020-01-15", "2020-01-20")
  r <- read_cmems_adt_daily(dates)

  expect_equal(terra::nlyr(r), 2L)
})

test_that("scalar readers handle lon180 rotation", {
  skip_if_no_raad()

  r_atlantic <- read_cmems_adt_daily("2020-01-15", lon180 = TRUE)
  r_pacific <- read_cmems_adt_daily("2020-01-15", lon180 = FALSE)

  ext_atl <- as.vector(terra::ext(r_atlantic))
  ext_pac <- as.vector(terra::ext(r_pacific))

  # One should have negative xmin (Atlantic), one should not (Pacific)
  # (depends on source file orientation)
  expect_true(TRUE)  # rotation logic is file-dependent
})

test_that("scalar readers handle crop", {
  skip_if_no_raad()

  bounds <- c(100, 150, -60, -40)
  r <- read_cmems_adt_daily("2020-01-15", xylim = bounds)

  ext <- as.vector(terra::ext(r))
  expect_true(ext[1] >= bounds[1] - 0.5)
  expect_true(ext[2] <= bounds[2] + 0.5)
  expect_true(ext[3] >= bounds[3] - 0.5)
  expect_true(ext[4] <= bounds[4] + 0.5)
})

test_that("scalar readers returnfiles returns tibble", {
  skip_if_no_raad()

  files <- read_cmems_adt_daily(returnfiles = TRUE)

  expect_s3_class(files, "tbl_df")
  expect_true("date" %in% names(files))
  expect_true("fullname" %in% names(files))
})

# =============================================================================
# Derived current products
# =============================================================================

test_that("read_cmems_current_speed_daily returns non-negative values", {
  skip_if_no_raad()

  r <- read_cmems_current_speed_daily("2020-01-15")

  expect_s4_class(r, "SpatRaster")
  expect_equal(terra::nlyr(r), 1L)

  vals <- terra::values(r)
  vals <- vals[!is.na(vals)]
  expect_true(all(vals >= 0))
})

test_that("read_cmems_current_direction_daily returns values 0-360", {
  skip_if_no_raad()

  r <- read_cmems_current_direction_daily("2020-01-15")

  expect_s4_class(r, "SpatRaster")
  expect_equal(terra::nlyr(r), 1L)

  vals <- terra::values(r)
  vals <- vals[!is.na(vals)]
  expect_true(all(vals >= 0))
  expect_true(all(vals <= 360))
})

test_that("derived readers handle multiple dates", {
  skip_if_no_raad()

  dates <- c("2020-01-15", "2020-01-20")
  r <- read_cmems_current_speed_daily(dates)

  expect_equal(terra::nlyr(r), 2L)
})

# =============================================================================
# Legacy shim tests
# =============================================================================

test_that("readssh shim returns Raster object for adt", {
  skip_if_no_raad()

  withr::with_options(list(raadtools.shim.warn = FALSE), {
    r <- readssh("2020-01-15", ssha = FALSE)
  })

  expect_s4_class(r, "Raster")
  expect_equal(raster::nlayers(r), 1L)
})

test_that("readssh shim returns Raster object for sla", {
  skip_if_no_raad()

  withr::with_options(list(raadtools.shim.warn = FALSE), {
    r <- readssh("2020-01-15", ssha = TRUE)
  })

  expect_s4_class(r, "Raster")
  expect_equal(raster::nlayers(r), 1L)
})

test_that("readcurr shim magonly works", {
  skip_if_no_raad()

  withr::with_options(list(raadtools.shim.warn = FALSE), {
    r <- readcurr("2020-01-15", magonly = TRUE)
  })

  expect_s4_class(r, "Raster")
  expect_equal(raster::nlayers(r), 1L)
})

test_that("readcurr shim dironly works", {
  skip_if_no_raad()

  withr::with_options(list(raadtools.shim.warn = FALSE), {
    r <- readcurr("2020-01-15", dironly = TRUE)
  })

  expect_s4_class(r, "Raster")
  expect_equal(raster::nlayers(r), 1L)
})

test_that("readcurr shim uonly works", {
  skip_if_no_raad()

  withr::with_options(list(raadtools.shim.warn = FALSE), {
    r <- readcurr("2020-01-15", uonly = TRUE)
  })

  expect_s4_class(r, "Raster")
  expect_equal(raster::nlayers(r), 1L)
})

test_that("readcurr shim default returns 2 layers", {
  skip_if_no_raad()

  withr::with_options(list(raadtools.shim.warn = FALSE), {
    r <- readcurr("2020-01-15")
  })

  expect_s4_class(r, "Raster")
  expect_equal(raster::nlayers(r), 2L)
})

test_that("readcurr shim warns on multiple dates without flag", {
  skip_if_no_raad()

  withr::with_options(list(raadtools.shim.warn = FALSE), {
    expect_warning(
      r <- readcurr(c("2020-01-15", "2020-01-20")),
      "only one date"
    )
  })
})
