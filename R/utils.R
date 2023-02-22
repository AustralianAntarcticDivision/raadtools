
nc_rawdata <- function(x, var) {
  nc <- ncdf4::nc_open(x)
  on.exit(ncdf4::nc_close(nc))
  ncdf4::ncvar_get(nc, var)
}

xrange <- function(x) c(xmin(x), xmax(x))
yrange <- function(x) c(ymin(x), ymax(x))

update <- function() {
  cat('\ndevtools::install_github("AustralianAntarcticDivision/raadtools")\n\n')
}

set_utc_format <- function(x) {
  attr(x, "tz") <- "UTC"
  x
}
## internal rotate to match old behaviour
## https://r-forge.r-project.org/scm/viewvc.php/pkg/raster/R/rotate.R?root=raster&r1=2782&r2=2981
#' @importFrom raster merge
.rotate <-  function(x, ...) {
  e <- extent(x)
  xrange <- e@xmax - e@xmin
  inverse <- FALSE
  if (xrange < 350 | xrange > 370 | e@xmin < -10 | e@xmax > 370) {
    if (xrange < 350 | xrange > 370 | e@xmin < -190 | e@xmax > 190) {
      warning('this does not look like an appropriate object for this function')
    } else {
      inverse <- TRUE
    }
  }
  hx <- e@xmin + xrange / 2
  r1 <- crop(x, extent(e@xmin, hx, e@ymin, e@ymax))
  r2 <- crop(x, extent(hx, e@xmax, e@ymin, e@ymax))
  if (inverse) {
    r1@extent@xmin <- r2@extent@xmax
    r1@extent@xmax <- r1@extent@xmin + 0.5 * xrange
  } else {
    r2@extent@xmin <- r2@extent@xmin - xrange
    r2@extent@xmax <- r2@extent@xmax - xrange
  }
  ln <- names(x)
  out <- merge(r1, r2, overlap=FALSE, ...)
  names(out) <- names(x)
  out@z <- x@z
  
  # suggested by Mike Sumner:
  p <- projection(out)	
  if (length(grep("\\+over", p)) > 0) {
    projection(out) <- gsub("[[:space:]]\\+over", "", p)
  }
  
  return(out)
}


## shared stuff
## datadir
## normalize input dates - need index and value

## private, but common
## dims, projection, bbox
## files
.processFiles <- function(dt, f, tr) {
  findex <- .processDates(dt, f$date, tr)
  f[findex, ]
}

# .fastNCvar <- function(x, varname) {
#   require(ncdf4)
#   ncvar_get(nc_open(x), varname)
# }
.expandFileDateList <- function(x) {
  vl <- vector("list", length(x))
  for (i in seq_along(x)) {
    b <- brick(x[i], quick = TRUE)
    dates <- timedateFrom(getZ(b))
    
    vl[[i]] <- data.frame(file = rep(x[i], length(dates)), date = dates, band = seq_along(dates), 
                          stringsAsFactors = FALSE)
  }
  do.call("rbind", vl)
}

.valiDates <- function(x, allOK = TRUE) {
  xs <- timedateFrom(x)
  bad <- is.na(xs)
  if (all(bad)) stop("no input dates are valid")
  if (any(bad)) {
    notOK <- "not all input dates are valid"
    if (allOK) stop(notOK) else warning(notOK)
  }
  xs[!bad]
}


.sortDates <- function(x, resortOK = FALSE) {
  ord <- order(x)
  if (any(diff(ord) < 0)) {
    sortOK <- "dates out of order and will be sorted"
    if (resortOK) warning(sortOK) else stop(sortOK)
    x <- x[ord]
  }
  x
}



.indexDates <- function(xdate, filedate) {
#   windex <- integer(length(xdate))
#   for (i in seq_along(xdate)) {
 #    windex[i] <- which.min(abs(xdate[i] - filedate))
#   }
   windex <- findInterval(xdate, filedate)
   windex[windex < 1] <- 1
   windex[windex > length(filedate)] <- length(filedate)
   windex
}

.dedupe <- function(index, date, removeDupes = TRUE) {
  nondupes <- !duplicated(index)
  if (sum(nondupes) < length(index)) {
    if (removeDupes) warning("duplicated dates will be dropped") else stop("duplicated dates not allowed")
    index <- index[nondupes]
    date <- date[nondupes]
  }
  list(index = index, date = date)
}

.matchFiles <- function(querydate, refdate, index, daytest = 7) {
  ##
  deltatime <- abs(difftime(querydate, refdate, units = "days"))
  deltatest <- deltatime > daytest
  if (all(deltatest)) {
    message(sprintf("\nnearest available date is %s", as.Date(refdate)))
    stop(sprintf("no data file within %.1f days of %s", daytest, format(querydate)))
  }
  if (any(deltatest)) {
    warning(sprintf("%i input dates have no corresponding data file within %f days of available files", sum(deltatest), daytest))
    index <- index[!deltatest]
  }
  index
}

.processDates <- function(qdate, fdate, timeres) {
  ## checks on dates, we drop any that are NA
  qdate <- .valiDates(qdate, allOK = FALSE)
  
  ## sort dates if need be
  qdate <- .sortDates(qdate, resortOK = TRUE)
  
  ## mapping of files/dates, so we can process time series
  findex <- .indexDates(qdate, fdate)
  
  ## check for duplicates
  dedupedates <- .dedupe(findex, qdate, removeDupes = TRUE)
  findex <- dedupedates$index
  date <- dedupedates$date
  
  .matchFiles(date, fdate[findex], findex, 
              daytest = switch(timeres, "4hourly" = 1/6, "12hourly" = 1/2, "3hourly" = 1/8, "6hourly" = 0.25, daily = 1.5, weekly = 4, monthly = 15, weekly3 = 26, "8daily" = 5, "8D" = 5))

}


##' Stable conversion to POSIXct from character and Date
##'
##' Conversion to POSIXct ensuring no local time zone applied. Currently supported is character, Date and
##' anything understood by \code{\link[base]{as.POSIXct}}.
##'
##' @param x input date-time stamp, character, Date or other supported type.
##' @param \dots ignored
##' @return the vector \code{x} converted (if necessary) to \code{POSIXct}
##' @export
timedateFrom <- function(x, ...) {
  as.POSIXct(x, tz = "GMT", ...)
}
