## tools/test-triage.R -- run the whole suite once, get a triage table back.

test_triage <- function(path = ".", filter = NULL, parallel = FALSE,
                        reporter = "summary") {
  testthat::set_max_fails(Inf)              # sets TESTTHAT_MAX_FAILS
  old <- Sys.getenv("TESTTHAT_PARALLEL", NA_character_)
  Sys.setenv(TESTTHAT_PARALLEL = if (isTRUE(parallel)) "TRUE" else "FALSE")
  on.exit(if (is.na(old)) Sys.unsetenv("TESTTHAT_PARALLEL")
          else Sys.setenv(TESTTHAT_PARALLEL = old), add = TRUE)
  
  res <- testthat::test_local(path, filter = filter, reporter = reporter,
                              stop_on_failure = FALSE)
  
  rank <- c(error = 1L, failure = 2L, warning = 3L, skip = 4L, success = 5L)
  rows <- lapply(res, function(blk) {
    exps  <- blk$results
    types <- vapply(exps, function(e) sub("^expectation_", "", class(e)[[1L]]), "")
    i     <- if (length(types)) which.min(rank[types]) else integer(0)
    worst <- if (length(i)) types[[i]] else "empty"
    e     <- if (length(i)) exps[[i]] else NULL
    sr    <- if (is.null(e)) NULL else e$srcref
    data.frame(
      file    = blk$file,
      test    = if (length(blk$test) == 1L && !is.na(blk$test)) blk$test
      else "(outside test_that)",
      outcome = worst,
      line    = if (inherits(sr, "srcref")) as.integer(sr[[1L]]) else NA_integer_,
      n       = length(exps),
      pass    = sum(types == "success"),
      secs    = round(blk$real, 1),
      msg     = if (is.null(e) || worst == "success") ""
      else sub("\n.*", "", conditionMessage(e)),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  out <- out[order(rank[out$outcome], out$file, out$line), ]
  rownames(out) <- NULL
  out
}

triage_save <- function(x, file = "tools/triage.csv") {
  utils::write.csv(x, file, row.names = FALSE)
  invisible(x)
}

## what changed since the last save: newly red, newly green, newly present
triage_diff <- function(x, file = "tools/triage.csv") {
  if (!file.exists(file)) return(x[0, ])
  old <- utils::read.csv(file, stringsAsFactors = FALSE)
  k   <- function(d) paste(d$file, d$test, sep = " :: ")
  m   <- match(k(x), k(old))
  x$was <- ifelse(is.na(m), "(new)", old$outcome[m])
  x[x$outcome != x$was, c("file", "test", "was", "outcome", "line", "msg")]
}

## what is still worth chasing: red, minus the ones you have decided to park.
## tools/expected-red.txt holds one "file :: test" line per parked block.
triage_open <- function(x, expected = "tools/expected-red.txt") {
  red <- if (file.exists(expected)) readLines(expected) else character(0)
  x[x$outcome %in% c("error", "failure") &
      !(paste(x$file, x$test, sep = " :: ") %in% red), ]
}