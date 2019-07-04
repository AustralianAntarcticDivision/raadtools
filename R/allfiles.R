##' Load raw list of all files available to raadtools from the data repository. 
##'
##' Raw list of all files. 
##' @param ... reserved for future use, currently ignored
##' @export
##' @return \code{data.frame} with columns \code{fullname} with all file paths
allfiles <- function(...) {
  raadfiles::get_raad_filenames(all = TRUE)
}


## DEVELOPERS if fullname is TRUE, it's slow because the entire list is prepended
## only currentsfiles() uses fullname = FALSE
.allfilelist <- function(rda = TRUE, fullname = TRUE) {
  out <- allfiles()
  file.path(out[["root"]], out[["file"]])
}
