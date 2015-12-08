

.loadMLD <- function() {
  p <- file.path(getOption("default.datadir"), "cache", "sallee_mld2013.Rdata")
  if (!file.exists(p)) return(NULL)
  mld <- NULL
  load(p)
  mld
}



##' Read the \enc{Sall?e}{Sallee} Mixed layer depth climatology.
##'
##' http://soki.aad.gov.au/display/Data/Mixed+layer+depth
##' @title Mixed Layer Depth
##' @param date date to extract, can only be a climatology month
##' @param xylim extent specification
##' @param returnfiles return the file details only
##' @param ... ignored
##' @return RasterBrick
##' @importFrom raster subset
##' @encoding ISO-8859-2
##' @export
readmld <- function(date, xylim = NULL, returnfiles = FALSE, ...) {
  if (missing(date)) date <- seq(as.Date("2013-01-01"), by = "1 month", length = 12L)
  
  date <- timedateFrom(date)
  
  date <- format(date, "%b")
  x <- .loadMLD()
  if (!is.null(xylim)) x  <- crop(x, extent(xylim))
  if (returnfiles) return(data.frame(file = file.path(getOption("default.datadir"), "data_local", "mld", "JBfitted", "sallee_mld2013.Rdata"), date = timedateFrom(seq(as.Date("2013-01-01"), by = "1 month", length = 12L))))
  warning("MLD data is only a climatology, returning matching month only")
  subset(x, date)
}
