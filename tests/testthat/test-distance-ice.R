context("distance-ice")

## TODO: Rename context
## TODO: Add more tests

test_that("ice distance", {
  distance_to_ice(threshold = 50) %>% expect_s4_class("RasterLayer")
  distance_to_ice_edge(latest = TRUE) %>% expect_s4_class("RasterLayer")
  
  distance_to_ice(as.Date("2016-08-10"), threshold = 15, product = "nsidc") %>% expect_s4_class("RasterLayer")
})
