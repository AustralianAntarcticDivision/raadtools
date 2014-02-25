library(testthat)
library(raadtools)
fs <- list.files("tests", pattern = ".R$", full.names = TRUE)
##source(grep("chla", fs, value = TRUE)[1])
for (i in seq_along(fs)) source(fs[i])

