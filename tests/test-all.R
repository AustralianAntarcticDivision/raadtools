library(testthat)

options(default.datadir = "/rdsi/PRIVATE")
library(raadtools)
f <- "/mnt/aadc/Scientific_Data/Data/gridded_new"
options(default.datadir = f)

test_check("raadtools")

# #library(testthat)
# library(raadtools)
# library(raster)  ## for nlayers, not imported properly (not exported properly from raster?)
fs <- list.files("tests/testthat", pattern = ".R$", full.names = TRUE)
# ##source(grep("fileexists", fs, value = TRUE)[1])
for (i in seq_along(fs)) source(fs[i])
# 
