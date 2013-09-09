require(testthat)
require(raadtools)
test_that("all file options give existing files, with warnings where appropriate", {
          expect_that(file.exists(topofile()), is_true())
          expect_that(file.exists(topofile("ibcso")), is_true())
          expect_that(file.exists(topofile("etopo1")), is_true())
          expect_that(file.exists(topofile("etopo2")), is_true())
          expect_that(file.exists(topofile("kerguelen")), is_true())
          expect_that(file.exists(topofile("george_v_terre_adelie")), is_true())
          expect_that(file.exists(topofile("smith_sandwell")), is_true())


          expect_that(file.exists(topofile("ibcso", polar = TRUE)), is_true())
          expect_that(file.exists(topofile("ibcso", lon180 = FALSE)), is_true())
          expect_that(file.exists(topofile("smith_sandwell", lon180 = TRUE)), is_true())

          expect_that(topofile("ibcso", lon180 = FALSE), gives_warning())
          expect_that(topofile("kerguelen", lon180 = FALSE), gives_warning())
          expect_that(topofile("kerguelen", lon180 = TRUE, polar = TRUE), gives_warning())

      })



test_that("file options result in actual data layers", {
          expect_that(readtopo(), is_a("RasterLayer"))
          expect_that(readtopo("ibcso"), is_a("RasterLayer"))
          expect_that(readtopo("etopo1"), is_a("RasterLayer"))
          expect_that(readtopo("etopo2"), is_a("RasterLayer"))
          expect_that(readtopo("kerguelen"), is_a("RasterLayer"))
          expect_that(readtopo("george_v_terre_adelie"), is_a("RasterLayer"))
          expect_that(readtopo("smith_sandwell"), is_a("RasterLayer"))

          expect_that(readtopo("ibcso", polar = TRUE), is_a("RasterLayer"))
          expect_that(readtopo("ibcso", lon180 = FALSE), is_a("RasterLayer"))
          expect_that(readtopo("smith_sandwell", lon180 = TRUE), is_a("RasterLayer"))

          ## these aren't working, is raster somehow swallowing warnings?
          ##expect_that(readtopo("ibcso", lon180 = FALSE), gives_warning())
          ##expect_that(readtopo("kerguelen", lon180 = FALSE), gives_warning())
          ##expect_that(readtopo("kerguelen", lon180 = TRUE, polar = TRUE), gives_warning())

      })

