##' Load raw list of all files available to raadtools from the data repository. 
##'
##' Raw list of all files. 
##' @param ... reserved for future use, currently ignored
##' @export
##' @return \code{data.frame} with columns \code{fullname} with all file paths
allfiles <- function(...) {
	data.frame(fullname = .allfilelist(), stringsAsFactors = FALSE)
}


## DEVELOPERS if fullname is TRUE, it's slow because the entire list is prepended
## only currentsfiles() uses fullname = FALSE
.allfilelist <- function(rda = TRUE, fullname = TRUE) {
  # datadir <- getOption("default.datadir")
  # if (rda) {
  #   fs <- NULL
  #   load(file.path(datadir, "admin", "filelist", "allfiles2.Rdata"))
  #   
  # } else { 
  #  fs <- readLines(file.path(datadir, "admin", "filelist", "allfiles2.txt"))
  # }
  # 
  # if (fullname) fs <- file.path(datadir, fs)
  # 
  # fs
  out <- raadfiles:::get_raw_raad_filenames()
  file.path(out[["root"]], out[["file"]])
}
