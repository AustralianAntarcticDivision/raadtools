test_that("all file options give existing files, with warnings where appropriate", {
          expect_true(file.exists(topofile()))  
          expect_true(file.exists(topofile("ibcso")))
          expect_error(topofile("etopo1"))
          expect_false(file.exists(topofile("etopo2")))
          expect_true(file.exists(topofile("kerguelen")))
          #expect_true(file.exists(topofile("george_v_terre_adelie")))
          expect_true(file.exists(topofile("smith_sandwell")))


          expect_true(file.exists(topofile("ibcso", polar = TRUE)))
          expect_true(file.exists(topofile("ibcso", lon180 = TRUE)))
          expect_true(file.exists(topofile("smith_sandwell", lon180 = TRUE)))

          
      
      })



test_that("file options result in actual data layers", {
          expect_s4_class(readtopo(), "SpatRaster")
          expect_s4_class(readtopo("ibcso"), "SpatRaster")
          expect_error(readtopo("etopo1"))
          expect_s4_class(readtopo("etopo2"), "SpatRaster")
          expect_s4_class(readtopo("kerguelen"), "SpatRaster")
          expect_error(readtopo("george_v_terre_adelie"), "should be one of")
          expect_s4_class(readtopo("smith_sandwell"), "SpatRaster")

          #expect_s4_class(readtopo("ibcso", polar = TRUE), "SpatRaster")
          #expect_s4_class(readtopo("ibcso", lon180 = FALSE), "SpatRaster")
          expect_s4_class(readtopo("smith_sandwell", lon180 = TRUE), "SpatRaster")

          ## these aren't working, is raster somehow swallowing warnings?
          ##expect_warning(readtopo("ibcso", lon180 = FALSE))
          ##expect_warning(readtopo("kerguelen", lon180 = FALSE))
          ##expect_warning(readtopo("kerguelen", lon180 = TRUE, polar = TRUE))

      })


test_that("topo projection is not missing", {
  prj <- crs(readtopo("kerguelen"))
  expect_true(nzchar(prj))
  
  
})
