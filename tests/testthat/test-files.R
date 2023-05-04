context("files")

## library(raadtools)
## writeLines(grep("files", ls("package:raadtools"), value = TRUE))
# allfiles
# amps_d1files
# chlafiles
# cpolarfiles
# currentsfiles
# derivaadcfiles
# derivicefiles
# fasticefiles
# ghrsstfiles
# icefiles
# ocfiles
# oc_sochla_files
# rapid_responsefiles
# sshfiles
# sstfiles
# windfiles
fnames <- c("amps_d1files", "chlafiles", "cpolarfiles", "currentsfiles", 
             "fasticefiles", "ghrsstfiles", 
            "icefiles", "ocfiles", "oc_sochla_files", "rapid_responsefiles", 
            "sshfiles", "sstfiles", "windfiles")

fnames2 <- c("derivaadcfiles", "derivicefiles")
listoffuns <- setNames(lapply(fnames, get), fnames)


test_that("file set/s sensible", {
  for (i in seq_along(listoffuns)) {
    context(names(listoffuns)[i])
    db <- listoffuns[[i]]()
    db %>% expect_s3_class("data.frame")
  }
  
})
