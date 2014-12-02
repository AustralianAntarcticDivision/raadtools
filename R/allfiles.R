##' Load raw list of all files available to raadtools from the data repository. 
##'
##' Raw list of all files. 
##' @param ... reserved for future use, currently ignored
##' @export
##' @return \code{data.frame} with columns \code{fullname} with all file paths
allfiles <- function(...) {
	data.frame(fullname = .allfilelist(), stringsAsFactors = FALSE)
}
