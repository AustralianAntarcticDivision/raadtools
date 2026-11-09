# tests/testthat/test-read-to-grid.R
# Reading onto a target grid (crs, extent, dim) with the GDAL warper.

test_that(".as_grid accepts a list, a view state and a SpatRaster", {
  g <- .as_grid(list(crs = "EPSG:3031", extent = c(-1, 1, -1, 1) * 4e6,
                     dim = c(300, 200)))
  expect_s3_class(g, "raad_grid")
  expect_identical(g$dim, c(300L, 200L))
  expect_equal(g$extent, c(-4e6, 4e6, -4e6, 4e6))

  ## what an aobcore served page sends: size_px, not dim
  vs <- list(crs = "EPSG:3031", extent = c(-2e6, 2e6, -1e6, 1e6),
             size_px = c(800, 400), zoom = 3, units_per_pixel = 5000)
  expect_identical(.as_grid(vs)$dim, c(800L, 400L))

  r <- terra::rast(terra::ext(100, 150, -60, -40), ncols = 50, nrows = 20,
                   crs = "EPSG:4326")
  gr <- .as_grid(r)
  expect_identical(gr$dim, c(50L, 20L))
  expect_equal(gr$extent, c(100, 150, -60, -40))
  expect_true(nzchar(gr$crs))
})

test_that(".as_grid rejects bad specifications", {
  ok <- list(crs = "EPSG:4326", extent = c(0, 10, 0, 10), dim = c(10, 10))
  expect_error(.as_grid(1:4), "target grid")
  expect_error(.as_grid(modifyList(ok, list(crs = ""))), "crs")
  expect_error(.as_grid(modifyList(ok, list(extent = c(10, 0, 0, 10)))), "extent")
  expect_error(.as_grid(modifyList(ok, list(extent = c(0, 10, 0)))), "extent")
  expect_error(.as_grid(modifyList(ok, list(dim = c(10.5, 10)))), "dim")
  expect_error(.as_grid(modifyList(ok, list(dim = c(0, 10)))), "dim")
  expect_error(.as_grid(ok[c("crs", "extent")]), "dim")
})

test_that(".read_to_grid warps a synthetic 0,360 source onto other grids", {
  ## a lon/lat source on 0,360 with no CRS, as OISST is: value = longitude
  ## in -180,180 so the antimeridian handling is visible in the result
  src <- terra::rast(terra::ext(0, 360, -90, 90), ncols = 360, nrows = 180)
  lon <- terra::xFromCol(src, seq_len(ncol(src)))
  lon <- ifelse(lon > 180, lon - 360, lon)
  terra::values(src) <- rep(lon, nrow(src))
  terra::crs(src) <- ""
  f <- tempfile(fileext = ".tif")
  on.exit(unlink(f))
  terra::writeRaster(src, f)

  g <- list(crs = "EPSG:4326", extent = c(-180, 180, -90, 90), dim = c(36, 18))
  r <- .read_to_grid(c(f, f), g, resample = "near", source_crs = "EPSG:4326")
  expect_s4_class(r, "SpatRaster")
  expect_equal(terra::nlyr(r), 2L)
  expect_equal(dim(r)[1:2], c(18, 36))
  ## both halves of the -180,180 target are filled from the 0,360 source
  v <- terra::values(r[[1]], mat = FALSE)
  expect_false(anyNA(v))
  expect_equal(unname(v[1:36]), terra::xFromCol(terra::rast(r), 1:36),
               tolerance = 6)

  ## a polar grid: in range, finite, and on the requested grid
  p <- .read_to_grid(f, list(crs = "EPSG:3031", extent = c(-1, 1, -1, 1) * 3e6,
                              dim = c(40, 40)),
                     source_crs = "EPSG:4326")
  expect_equal(dim(p), c(40, 40, 1))
  expect_true(grepl("Polar", terra::crs(p, describe = TRUE)$name, ignore.case = TRUE))
  pv <- terra::values(p, mat = FALSE)
  expect_false(anyNA(pv))
  expect_true(all(pv >= -180 & pv <= 180))
})

test_that(".read_to_grid reports the failing source", {
  ## GDAL also warns that the file is missing; the error is what matters
  expect_error(suppressWarnings(
    .read_to_grid("no-such-file.tif",
                  list(crs = "EPSG:4326", extent = c(0, 1, 0, 1), dim = c(2, 2)))),
    "no-such-file.tif")
})

# -----------------------------------------------------------------------------
# with raad data
# -----------------------------------------------------------------------------

## list the catalogues once (memoized in any case) and pass them in
oisst_files <- function(res = c("daily", "monthly")) {
  res <- match.arg(res)
  files <- tryCatch(sstfiles(res), error = function(e) NULL)
  if (is.null(files) || nrow(files) == 0) skip(paste("no OISST", res, "files found"))
  files
}

test_that("read_oisst_daily reads onto a polar grid", {
  skip_if_no_raad()
  files <- oisst_files("daily")
  g <- list(crs = "EPSG:3031", extent = c(-1, 1, -1, 1) * 4e6, dim = c(200, 200))
  r <- read_oisst_daily("2020-01-15", grid = g, inputfiles = files)
  expect_equal(dim(r), c(200, 200, 1))
  expect_equal(as.Date(terra::time(r)), as.Date("2020-01-15"))
  v <- terra::values(r, mat = FALSE)
  ## scale and offset applied: degrees C, not packed integers
  expect_true(all(v > -3 & v < 35, na.rm = TRUE))
  expect_true(mean(is.na(v)) < 0.5)
})

test_that("read_oisst_daily on its own grid matches the native read", {
  skip_if_no_raad()
  files <- oisst_files("daily")
  native <- read_oisst_daily("2020-01-15", xylim = c(100, 150, -60, -40),
                             inputfiles = files)
  g <- read_oisst_daily("2020-01-15", grid = native, resample = "near",
                        inputfiles = files)
  expect_equal(terra::values(g), terra::values(native), ignore_attr = TRUE)
})

test_that("read_oisst_monthly reads the right band of each date onto a grid", {
  skip_if_no_raad()
  files <- oisst_files("monthly")
  d <- c("2020-01-15", "2020-07-15")
  native <- read_oisst_monthly(d, inputfiles = files)
  g <- read_oisst_monthly(d, grid = native, resample = "near", inputfiles = files)
  expect_equal(terra::nlyr(g), 2L)
  expect_equal(names(g), names(native))
  expect_equal(terra::values(g), terra::values(native), ignore_attr = TRUE)
})
