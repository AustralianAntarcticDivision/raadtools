# require(testthat)
# require(raadtools)
# 
# 
# 
# test_that("prod data is returned", {
#     expect_that(prodfiles(), is_a("data.frame"))
#     expect_that(readprod("2005-01-10"), is_a("RasterBrick"))
# })
# 
# test_that("prod projection is not missing", {
#   prj <- projection(readprod())
#   expect_that(is.na(prj), is_false())
#   
# })
