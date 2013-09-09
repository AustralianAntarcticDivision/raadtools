library(testthat)
library(raadtools)
fs <- list.files("tests", full.names = TRUE)
for (i in fs) source(i)
