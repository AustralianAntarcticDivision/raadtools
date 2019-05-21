#' Set the file system locations where data are stored
#'
#' This is a convenience wrapper around the corresponding admin functions in the \code{raadfiles} package, so that most users will not need to use those functions directly.
#' This function tells \code{raadtools} where to look for its data files, (by default) will build the necessary file list cache if it does not exist, and loads that cache into memory so that \code{raadtools} can use it.
#'
#' @param ... strings: one or more paths to directories containing data
#' @param build_cache_if_missing logical: \code{raadtools} maintains a cache that lists all of files in each data collection. If \code{build_cache_if_missing} is \code{TRUE}, a cache will be built if it does not exist
#' @param refresh_cache logical: for each path, should the file collection be re-scanned and the cache rebuilt? (May be slow)
#'
#' @return TRUE (invisibly) on success
#'
#' @examples
#' \dontrun{
#' ## assume that we have downloaded some data files to c:/my/data
#' library(raadtools)
#' set_data_roots("c:/my/data")
#' ## see the README/vignette for more detail
#' }
#'
#' @export
set_data_roots <- function(..., build_cache_if_missing = TRUE, refresh_cache = FALSE) {
    ok <- raadfiles::set_raad_data_roots(...)
    if (ok) {
        have_loaded <- FALSE
        raadfiles.data.roots <- raadfiles::get_raad_data_roots()
        raadfiles.data.filedbs <- raadfiles::raad_filedb_path(raadfiles.data.roots)
        miss <- !file.exists(raadfiles.data.filedbs)
        if ((build_cache_if_missing && any(miss)) || refresh_cache) {
            ## this will build cache for ALL roots
            raadfiles:::run_this_function_to_build_raad_cache()
            have_loaded <- TRUE
        }
        ## load cache info into memory if not already done so above
        if (!have_loaded) raadfiles::set_raad_filenames()
    }
    invisible(ok)
}


#' @rdname set_data_roots
#' @export
get_data_roots <- function() raadfiles::get_raad_data_roots()

