# tests/testthat/test-shim-notice.R
# The transition notice fires once per function per session.
# No data library needed - this is the notice machinery, not a reader.

test_that("a reader announces itself once, not on every call", {
  raadtools:::.shim_notice_reset()
  withr::with_options(list(raadtools.shim.warn = TRUE), {
    expect_warning(raadtools:::.shim_notice("readsst", "read_oisst_daily"),
                   "now returns a terra SpatRaster")
    expect_silent(raadtools:::.shim_notice("readsst", "read_oisst_daily"))
    expect_silent(raadtools:::.shim_notice("readsst", "read_oisst_daily"))
  })
})

test_that("each reader gets its own notice", {
  raadtools:::.shim_notice_reset()
  withr::with_options(list(raadtools.shim.warn = TRUE), {
    expect_warning(raadtools:::.shim_notice("readsst"), "'readsst'")
    expect_warning(raadtools:::.shim_notice("readice"), "'readice'")
    expect_silent(raadtools:::.shim_notice("readsst"))
  })
})

test_that("the notice points at the vignette", {
  raadtools:::.shim_notice_reset()
  withr::with_options(list(raadtools.shim.warn = TRUE), {
    expect_warning(raadtools:::.shim_notice("readsst"), "terra-transition")
  })
})

test_that("resetting makes a reader announce itself again", {
  raadtools:::.shim_notice_reset()
  withr::with_options(list(raadtools.shim.warn = TRUE), {
    expect_warning(raadtools:::.shim_notice("readsst"), "now returns")
    expect_silent(raadtools:::.shim_notice("readsst"))
    raadtools:::.shim_notice_reset()
    expect_warning(raadtools:::.shim_notice("readsst"), "now returns")
  })
})

test_that("the option silences it entirely", {
  raadtools:::.shim_notice_reset()
  withr::with_options(list(raadtools.shim.warn = FALSE), {
    expect_silent(raadtools:::.shim_notice("readsst", "read_oisst_daily"))
  })
})

test_that(".without_shim_warning keeps internal reads quiet", {
  raadtools:::.shim_notice_reset()
  withr::with_options(list(raadtools.shim.warn = TRUE), {
    expect_silent(
      raadtools:::.without_shim_warning(raadtools:::.shim_notice("readsst"))
    )
    ## and the notice is still owed, because it was never said
    expect_warning(raadtools:::.shim_notice("readsst"), "now returns")
  })
})
