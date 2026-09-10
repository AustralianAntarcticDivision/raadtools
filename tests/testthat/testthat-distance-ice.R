
test_that("ice distance", {
  ## concentration is a fraction now, not a percentage
  s <- distance_to_ice(threshold = 0.5) %>% expect_s4_class("SpatRaster")
  n <- distance_to_ice(threshold = 0.5, hemisphere = "north") %>% expect_s4_class("SpatRaster")
  expect_true(all(dim(s) == c(332, 316, 1)))
  expect_true(all(dim(n) == c(448, 304, 1)))

  distance_to_ice_edge() %>% expect_s4_class("SpatRaster")
  distance_to_ice(as.Date("2016-08-10"), threshold = 0.15) %>% expect_s4_class("SpatRaster")
})
