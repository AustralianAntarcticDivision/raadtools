# R/compat-ssh.R
# Front door for readssh() - dispatches to the terra-native readers

#' Read sea surface height data
#'
#' @description
#' \code{readssh} picks a sensible default and returns a terra
#' \code{SpatRaster}. For the historical defaults pinned explicitly, see
#' \code{\link{read_cmems_adt_daily}} and \code{\link{read_cmems_sla_daily}}.
#'
#' @param date date or dates of data to read
#' @param time.resolution time resolution (only "daily" supported)
#' @param xylim spatial extents to crop
#' @param lon180 if TRUE (default), use Atlantic-centered longitude
#' @param ssha if TRUE, return sea level anomaly; if FALSE (default), return ADT
#' @param latest if TRUE and date missing, return latest
#' @param returnfiles if TRUE, return file catalog
#' @param verbose ignored (for backward compatibility)
#' @param ... passed to underlying reader
#' @param inputfiles optional pre-filtered file catalog
#'
#' @return \code{SpatRaster}, or tibble if \code{returnfiles = TRUE}
#'
#' @seealso \code{\link{read_cmems_adt_daily}}, \code{\link{read_cmems_sla_daily}}
#'
#' @export
readssh <- function(date,
                    time.resolution = c("daily"),
                    xylim = NULL,
                    lon180 = TRUE,
                    ssha = FALSE,
                    latest = TRUE,
                    returnfiles = FALSE,
                    verbose = TRUE,
                    ...,
                    inputfiles = NULL) {

  time.resolution <- match.arg(time.resolution)

  new_fn <- if (ssha) "read_cmems_sla_daily" else "read_cmems_adt_daily"
  .shim_notice("readssh", new_fn)

  # Dispatch based on ssha flag
  r <- if (ssha) {
    read_cmems_sla_daily(
      date = date,
      xylim = xylim,
      lon180 = lon180,
      latest = latest,
      returnfiles = returnfiles,
      inputfiles = inputfiles,
      ...
    )
  } else {
    read_cmems_adt_daily(
      date = date,
      xylim = xylim,
      lon180 = lon180,
      latest = latest,
      returnfiles = returnfiles,
      inputfiles = inputfiles,
      ...
    )
  }

  if (returnfiles) return(r)

  r
}
