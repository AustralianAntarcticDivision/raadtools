library(testthat)
library(raadtools)
fs <- list.files("tests", full.names = TRUE)
source(grep("chla", fs, value = TRUE)[1])
##for (i in fs) source(i)

