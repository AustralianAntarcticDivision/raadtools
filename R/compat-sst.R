# R/compat-sst.R
# Front door for readsst() - dispatches to the terra-native readers

#' Read sea surface temperature data
#'
#' @description
#' \code{readsst} picks a sensible default and returns a terra
#' \code{SpatRaster}. For the historical defaults pinned explicitly, see
#' \code{\link{read_oisst_daily}} and \code{\link{read_oisst_monthly}}.
#'
#' @param date date or dates of data to read
#' @param time.resolution time resolution: "daily" or "monthly"
#' @param xylim spatial extents to crop from source data
#' @param lon180 if TRUE (default), convert Pacific (0,360) to Atlantic (-180,180) view
#' @param varname variable to read: "sst", "anom", "err", or "ice"
#' @param setNA mask out land values (ignored in new implementation, kept for API compatibility
#' @param latest if TRUE and date missing, return latest available
#' @param returnfiles if TRUE, return the file catalog instead of data
#' @param ... passed to underlying reader
#' @param inputfiles optional pre-filtered file catalog
#'
#' @return \code{SpatRaster}, or tibble if \code{returnfiles = TRUE}
#'
#' @seealso
#' \code{\link{read_oisst_daily}} for the modern terra-based daily reader
#' \code{\link{read_oisst_monthly}} for the modern terra-based monthly reader
#'
#' @export
#' @examples
#' \dontrun{
#' # sensible default
#' sst <- readsst("2023-01-15")
#' class(sst)  # SpatRaster, one layer
#'
#' # historical default pinned explicitly
#' sst <- read_oisst_daily("2023-01-15")
#' class(sst)  # SpatRaster
#' }
readsst <- function(date,
                    time.resolution = c("daily", "monthly"),
                    xylim = NULL,
                    lon180 = TRUE,
                    varname = c("sst", "anom", "err", "ice"),
                    setNA = TRUE,
                    latest = TRUE,
                    returnfiles = FALSE,
                    ...,
                    inputfiles = NULL) {

  time.resolution <- match.arg(time.resolution)
  varname <- match.arg(varname)

  # Emit deprecation notice (can be made louder/quieter via option)
  .shim_notice("readsst", switch(time.resolution, daily = "read_oisst_daily", monthly = "read_oisst_monthly"))

  # Dispatch to appropriate terra-native reader
  r <- switch(time.resolution,
    daily = read_oisst_daily(
      date = date,
      xylim = xylim,
      lon180 = lon180,
      varname = varname,
      latest = latest,
      returnfiles = returnfiles,
      inputfiles = inputfiles,
      ...
    ),
    monthly = read_oisst_monthly(
      date = date,
      xylim = xylim,
      lon180 = lon180,
      varname = varname,
      latest = latest,
      returnfiles = returnfiles,
      inputfiles = inputfiles,
      ...
    )
  )

  # If returnfiles, pass through the tibble unchanged
  if (returnfiles) {
    return(r)
  }

  r
}


#' @rdname readsst
#' @export
readsst_daily <- function(date,
                          xylim = NULL,
                          lon180 = TRUE,
                          varname = c("sst", "anom", "err", "ice"),
                          setNA = TRUE,
                          latest = TRUE,
                          returnfiles = FALSE,
                          ...,
                          inputfiles = NULL) {

  .shim_notice("readsst_daily", "read_oisst_daily")

  r <- read_oisst_daily(
    date = date,
    xylim = xylim,
    lon180 = lon180,
    varname = match.arg(varname),
    latest = latest,
    returnfiles = returnfiles,
    inputfiles = inputfiles,
    ...
  )

  if (returnfiles) return(r)

  r
}
