# tests/testthat/helper-raad.R
# testthat sources helper-*.R before any test file, so every file can call
# this. A file that needs a particular collection defines its own
# skip_if_no_raad() that also checks that collection's lister.
#
# get_raad_data_roots() is NULL where no data roots are configured, so outside
# the raad data library these tests skip instead of erroring. skip() signals
# a condition of class "skip", not "error", so the handler below never
# swallows it.

skip_if_no_raad <- function() {
  tryCatch({
    roots <- raadfiles::get_raad_data_roots()
    if (length(roots) == 0) skip("raad data roots not configured")
  }, error = function(e) {
    skip(paste("raad data not available:", e$message))
  })
}
