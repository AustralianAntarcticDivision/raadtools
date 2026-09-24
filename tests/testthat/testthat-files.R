
## One block per lister, so a lister that errors is named in the results
## instead of stopping the loop over the rest.
fnames <- c("amps_d1files", "chlafiles", "cpolarfiles", "currentsfiles",
            "fasticefiles", "ghrsstfiles",
            "icefiles", "ocfiles", "rapid_responsefiles",
            "sshfiles", "sstfiles", "windfiles")

for (fname in fnames) {
  test_that(sprintf("%s() returns a data.frame", fname), {
    skip_if_no_raad()
    db <- get(fname)()
    expect_s3_class(db, "data.frame")
  })
}
