# tests/testthat/test-read-topo.R
# Tests for terra-native topography readers

skip_if_no_raad <- function() {
  tryCatch({
    roots <- raadfiles::get_raad_data_roots()
    if (length(roots) == 0) skip("raad data roots not configured")
  }, error = function(e) {
    skip(paste("raad data not available:", e$message))
  })
}

skip_if_no_topo <- function(topo) {
  skip_if_no_raad()
  tryCatch({
    f <- topofile(topo)
    if (is.null(f) || is.na(f) || !file.exists(f)) {
      # Could be a VRT string
      if (!grepl("<VRT", f)) {
        skip(paste("topo file not found:", topo))
      }
    }
  }, error = function(e) {
    skip(paste("topo not available:", topo, "-", e$message))
  })
}

# =============================================================================
# Basic functionality
# =============================================================================

test_that("read_topo returns SpatRaster", {
  skip_if_no_topo("gebco_23")

  # Small crop to avoid loading entire global grid
  r <- read_topo("gebco_23", xylim = c(100, 102, -42, -40))

  expect_s4_class(r, "SpatRaster")
  expect_equal(terra::nlyr(r), 1L)
})

test_that("read_topo has CRS set", {
  skip_if_no_topo("gebco_23")

  r <- read_topo("gebco_23", xylim = c(100, 102, -42, -40))

  expect_false(is.na(terra::crs(r, proj = TRUE)))
})

test_that("read_topo crop works with numeric vector", {
  skip_if_no_topo("gebco_23")

  bounds <- c(100, 110, -50, -40)
  r <- read_topo("gebco_23", xylim = bounds)

  ext <- as.vector(terra::ext(r))
  expect_true(ext[1] >= bounds[1] - 0.1)
  expect_true(ext[2] <= bounds[2] + 0.1)
  expect_true(ext[3] >= bounds[3] - 0.1)
  expect_true(ext[4] <= bounds[4] + 0.1)
})

test_that("read_topo crop works with SpatExtent", {
  skip_if_no_topo("gebco_23")

  bounds <- terra::ext(100, 110, -50, -40)
  r <- read_topo("gebco_23", xylim = bounds)

  expect_s4_class(r, "SpatRaster")
})

test_that("read_topo warp works with SpatRaster template", {
  skip_if_no_topo("gebco_23")

  template <- terra::rast(
    nrows = 50, ncols = 50,
    xmin = 100, xmax = 110, ymin = -50, ymax = -40,
    crs = "EPSG:4326"
  )

  r <- read_topo("gebco_23", xylim = template)

  expect_equal(dim(r)[1:2], c(50, 50))
  expect_equal(terra::crs(r, proj = TRUE), terra::crs(template, proj = TRUE))
})

test_that("read_topo returnfiles returns path", {
  skip_if_no_topo("gebco_23")

  f <- read_topo("gebco_23", returnfiles = TRUE)

  expect_type(f, "character")
  expect_length(f, 1)
})

# =============================================================================
# Different datasets
# =============================================================================

test_that("read_topo works for etopo1",
 {
  skip_if_no_topo("etopo1")

  r <- read_topo("etopo1", xylim = c(100, 102, -42, -40))
  expect_s4_class(r, "SpatRaster")
})

test_that("read_topo works for ibcso", {
  skip_if_no_topo("ibcso_is")

  # IBCSO is Antarctic, use appropriate extent
  r <- read_topo("ibcso_is", xylim = c(-500000, 500000, -500000, 500000))
  expect_s4_class(r, "SpatRaster")
})

test_that("read_topo works for rema_1km", {
  skip_if_no_topo("rema_1km")

  r <- read_topo("rema_1km", xylim = c(-500000, 500000, -500000, 500000))
  expect_s4_class(r, "SpatRaster")
})

# =============================================================================
# Legacy shim tests
# =============================================================================

test_that("readtopo shim returns RasterLayer", {
  skip_if_no_topo("gebco_23")

  withr::with_options(list(raadtools.shim.warn = FALSE), {
    r <- readtopo("gebco_23", xylim = c(100, 102, -42, -40))
  })

  expect_s4_class(r, "RasterLayer")
})

test_that("readbathy alias works", {
  skip_if_no_topo("gebco_23")

  withr::with_options(list(raadtools.shim.warn = FALSE), {
    r <- readbathy("gebco_23", xylim = c(100, 102, -42, -40))
  })

  expect_s4_class(r, "RasterLayer")
})

test_that("read_bathy alias works", {
  skip_if_no_topo("gebco_23")

  r <- read_bathy("gebco_23", xylim = c(100, 102, -42, -40))
  expect_s4_class(r, "SpatRaster")
})
