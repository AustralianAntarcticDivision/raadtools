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
          expect_s4_class(readtopo(), "RasterLayer")
          expect_s4_class(readtopo("ibcso"), "RasterLayer")
          expect_s4_class(readtopo("etopo1"), "RasterLayer")
          expect_s4_class(readtopo("etopo2"), "RasterLayer")
          expect_s4_class(readtopo("kerguelen"), "RasterLayer")
          expect_error(readtopo("george_v_terre_adelie"), "not available")
          expect_s4_class(readtopo("smith_sandwell"), "RasterLayer")

          #expect_s4_class(readtopo("ibcso", polar = TRUE), "RasterLayer")
          #expect_s4_class(readtopo("ibcso", lon180 = FALSE), "RasterLayer")
          expect_s4_class(readtopo("smith_sandwell", lon180 = TRUE), "RasterLayer")

          ## these aren't working, is raster somehow swallowing warnings?
          ##expect_warning(readtopo("ibcso", lon180 = FALSE))
          ##expect_warning(readtopo("kerguelen", lon180 = FALSE))
          ##expect_warning(readtopo("kerguelen", lon180 = TRUE, polar = TRUE))

      })


test_that("topo projection is not missing", {
  prj <- projection(readtopo("kerguelen"))
  expect_false(is.na(prj))
  
  
})
