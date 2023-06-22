context("distance-ice")

test_that("ice distance", {
  s <- distance_to_ice(threshold = 50) %>% expect_s4_class("RasterLayer")
  n <- distance_to_ice(threshold = 50, hemisphere = "north") %>% expect_s4_class("RasterLayer")
  expect_true(all(dim(s) == c(332, 316, 1)))
  expect_true(all(dim(n) == c(448, 304, 1)))
  
  distance_to_ice_edge() %>% expect_s4_class("RasterLayer")
  distance_to_ice(as.Date("2016-08-10"), threshold = 15) %>% expect_s4_class("RasterLayer")
})
