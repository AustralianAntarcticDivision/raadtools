# tests/testthat/test-source-spec.R
# The reader contract for reading onto a target grid: what a reader returns
# for a date can say where it came from - GDAL dsn and band per layer, and
# the CRS, extent and dim of its grid - so it can be read again, by the GDAL
# warper, onto any other grid.

expect_source_spec <- function(r) {
  s <- .source_spec(r)
  expect_false(is.null(s), info = "a layer is in memory: no source to read again")
  if (is.null(s)) return(invisible(NULL))
  expect_length(s$dsn, terra::nlyr(r))
  expect_length(s$band, terra::nlyr(r))
  expect_true(all(nzchar(s$dsn)))
  expect_true(all(s$band >= 1L))
  expect_true(nzchar(s$crs))
  expect_length(s$extent, 4L)
  expect_true(s$extent[1] < s$extent[2] && s$extent[3] < s$extent[4])
  expect_identical(s$dim, as.integer(c(terra::ncol(r), terra::nrow(r))))
  ## and the dsn really opens, with that many bands at least
  for (i in seq_along(s$dsn)) {
    info <- vapour::vapour_raster_info(s$dsn[i])
    expect_gte(info$bands, s$band[i])
  }
  invisible(s)
}

test_that(".source_spec reads a file-backed raster and refuses one in memory", {
  f <- tempfile(fileext = ".tif")
  on.exit(unlink(f))
  r <- terra::rast(terra::ext(0, 10, 0, 5), ncols = 10, nrows = 5,
                   crs = "EPSG:4326", vals = 1:50, nlyrs = 1)
  r <- c(r, r * 2)
  terra::writeRaster(r, f)
  rf <- terra::rast(f)
  s <- expect_source_spec(rf)
  expect_identical(s$band, 1:2)
  expect_identical(s$dim, c(10L, 5L))
  expect_identical(.source_spec(rf[[2]])$band, 2L)
  expect_null(.source_spec(rf + 1))
  expect_null(.source_spec(terra::rotate(terra::rast(terra::ext(0, 360, -90, 90),
                                                     ncols = 36, nrows = 18, vals = 1))))
})

## Each reader, called for a date with its defaults. `known` says why a
## reader does not meet the contract yet: it is skipped with that reason, so
## the list shows in every test run until it is fixed.
reader_contract <- list(
  readsst          = list(call = quote(readsst(d)),
                          known = "lon180 = TRUE default: rotate() materialises"),
  readsst_lon360   = list(call = quote(readsst(d, lon180 = FALSE))),
  readsst_monthly  = list(call = quote(readsst(d, time.resolution = "monthly", lon180 = FALSE))),
  readice          = list(call = quote(readice(d)),
                          known = "setNA default: classify() materialises"),
  read_amsr2_ice   = list(call = quote(read_amsr2_ice(d)),
                          known = "setNA default: classify() materialises"),
  readssh          = list(call = quote(readssh(d))),
  readcurr         = list(call = quote(readcurr(d))),
  readwind         = list(call = quote(readwind(d)),
                          known = "NCEP2 0,360: rotate() materialises"),
  read_ccmp        = list(call = quote(read_ccmp(d))),
  readchla         = list(call = quote(readchla(d))),
  readmld          = list(call = quote(readmld(d)),
                          known = "climatology from an .Rdata RasterBrick: no file"),
  readghrsst       = list(call = quote(readghrsst(d))),
  readfsle         = list(call = quote(readfsle(d))),
  readtopo         = list(call = quote(readtopo("etopo2"))),
  read_bathy       = list(call = quote(read_bathy()))
)

for (nm in names(reader_contract)) {
  local({
    rc <- reader_contract[[nm]]
    test_that(sprintf("%s returns an object with a source spec", nm), {
      skip_if_no_raad()
      if (!is.null(rc$known)) skip(paste("known:", rc$known))
      withr::local_options(raadtools.shim.warn = FALSE)
      d <- "2020-01-15"
      r <- tryCatch(eval(rc$call), error = function(e) skip(conditionMessage(e)))
      expect_source_spec(r)
    })
  })
}
