context("topography")

require(testthat)
require(raadtools)
test_that("all file options give existing files, with warnings where appropriate", {
          expect_false(file.exists(topofile()))  ## VRT now
          expect_true(file.exists(topofile("ibcso")))
          expect_false(file.exists(topofile("etopo1")))
          expect_false(file.exists(topofile("etopo2")))
          expect_true(file.exists(topofile("kerguelen")))
          #expect_true(file.exists(topofile("george_v_terre_adelie")))
          expect_true(file.exists(topofile("smith_sandwell")))


          expect_true(file.exists(topofile("ibcso", polar = TRUE)))
          expect_true(file.exists(topofile("ibcso", lon180 = TRUE)))
          expect_true(file.exists(topofile("smith_sandwell", lon180 = TRUE)))

          
      
      })



test_that("file options result in actual data layers", {
          expect_that(readtopo(), is_a("RasterLayer"))
          expect_that(readtopo("ibcso"), is_a("RasterLayer"))
          expect_that(readtopo("etopo1"), is_a("RasterLayer"))
          expect_that(readtopo("etopo2"), is_a("RasterLayer"))
          expect_that(readtopo("kerguelen"), is_a("RasterLayer"))
          expect_error(readtopo("george_v_terre_adelie"), "not available")
          expect_that(readtopo("smith_sandwell"), is_a("RasterLayer"))

          #expect_that(readtopo("ibcso", polar = TRUE), is_a("RasterLayer"))
          #expect_that(readtopo("ibcso", lon180 = FALSE), is_a("RasterLayer"))
          expect_that(readtopo("smith_sandwell", lon180 = TRUE), is_a("RasterLayer"))

          ## these aren't working, is raster somehow swallowing warnings?
          ##expect_that(readtopo("ibcso", lon180 = FALSE), gives_warning())
          ##expect_that(readtopo("kerguelen", lon180 = FALSE), gives_warning())
          ##expect_that(readtopo("kerguelen", lon180 = TRUE, polar = TRUE), gives_warning())

      })


test_that("topo projection is not missing", {
  prj <- projection(readtopo("kerguelen"))
  expect_false(is.na(prj))
  
  
})
