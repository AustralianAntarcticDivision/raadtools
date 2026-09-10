
# sst <- sstfiles()
# ice <- icefiles()
# wind <- windfiles()
# curr <- currentsfiles()
# amps <- raadtools::amps_d1files()
# oc <- raadtools::ocfiles()
 #ssh <- raadtools::sshfiles()
ffr <- function(x) unclass(min(x$date))
#ref <- c(ffr(sst), ffr(ice), ffr(wind), ffr(curr), ffr(amps), ffr(oc), ffr(ssh))
ref <- as.integer(c(368150400, 278208000, 283996800, 725846400, 1445774400, 1025654400, 
         725846400))

ffw <- function(x) {x <- as.integer(c(x)); attributes(x) <- NULL; x}
test_that("date time underlying values don't change", {
  skip(message = "Date from terra::time is a problem atm")
  expect_equal(ffw(terra::time(readsst(latest = FALSE))), ref[1])
  expect_equal(ffw(terra::time(readice(latest = FALSE))), ref[2])
  expect_equal(ffw(terra::time(readwind(latest = FALSE))[1]), ref[3])
  expect_equal(ffw(terra::time(readcurr(latest = FALSE))[1]), ref[4])
  expect_equal(ffw(terra::time(readamps_d1wind(latest = FALSE))[1]), ref[5])
  ## oc_sochla_files() retired 2026-09-09, see archive/
  expect_equal(ffw(terra::time(readssh(latest = FALSE))), ref[7])

})
