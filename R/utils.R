## Read a raster source from a file path.
##
## md = FALSE keeps terra on the classic GDAL raster path. The multidimensional
## route is only lightly exercised, it works against raadtools' design of 2D
## rasters with bands, and it pulls in geolocation-array determination that
## recent versions of the GDAL netCDF driver have made unnecessary.
##
## Every read of a file in raadtools goes through here, so the flag is set in
## one place rather than at each call site.
.rast_nc <- function(x, ...) {
  terra::rast(x, ..., md = FALSE)
}

## The raster-era readers passed ... straight to brick(), which in practice
## meant filename=. terra writes with a separate call, so honour the argument
## here rather than dropping it silently when a reader is converted.
.write_if_filename <- function(x, ...) {
  dots <- list(...)
  fn <- dots[["filename"]]
  if (is.null(fn) || !nzchar(fn)) return(x)
  terra::writeRaster(x, fn, overwrite = isTRUE(dots[["overwrite"]]))
}

## An extent in the form terra wants, from whatever the caller had.
##
## xylim has always been documented as "an extent, or an object that provides
## one", which in the raster era meant a raster Extent, a bare numeric of
## four, or a raster object. terra::ext() knows the last two and not the
## first, so that one is unpacked by hand. Readers being converted can go on
## accepting everything they used to.
.as_ext <- function(x) {
  if (is.null(x)) return(NULL)
  if (inherits(x, "SpatExtent")) return(x)
  if (inherits(x, "Extent")) {
    return(terra::ext(c(x@xmin, x@xmax, x@ymin, x@ymax)))
  }
  if (inherits(x, "BasicRaster")) {
    e <- raster::extent(x)
    return(terra::ext(c(e@xmin, e@xmax, e@ymin, e@ymax)))
  }
  terra::ext(x)
}

## Compass direction in degrees [0, 360) from U and V components.
##
## The legacy raster readers computed this inside raster::overlay(), so the
## modulo ran in base R, where the result takes the sign of the divisor and
## negatives wrap up into [0, 360). terra's `%%` follows C fmod and keeps the
## sign of the dividend instead, so the bare
## `(90 - atan2(v, u) * 180/pi) %% 360` used by the terra readers returns
## negative degrees wherever atan2 exceeds 90, i.e. a quarter of the compass.
## The doubled modulo below is correct under both conventions.
.uv_direction <- function(u, v) {
  d <- 90 - terra::atan2(v, u) * 180 / pi
  (d %% 360 + 360) %% 360
}

## The legacy read* names are not going away - they are the front doors that
## pick a sensible default. What changed is the return class, so that is what
## we announce, alongside the specific reader this call resolved to.
## Noisy by default: options(raadtools.shim.warn = FALSE) turns it off.
.shim_notice <- function(fun, specific = NULL) {
  if (!isTRUE(getOption("raadtools.shim.warn", TRUE))) return(invisible(NULL))
  msg <- sprintf("'%s' now returns a terra SpatRaster, not a Raster* object.", fun)
  if (!is.null(specific)) {
    msg <- paste0(msg, sprintf("\n  This call reads through '%s'.", specific))
  }
  msg <- paste0(msg, "\n  Set options(raadtools.shim.warn = FALSE) to silence this.")
  warning(msg, call. = FALSE)
  invisible(NULL)
}

## Internal calls into the compat shims should not raise the user-facing
## notice - it is aimed at package users, not at raadtools itself.
.without_shim_warning <- function(expr) {
  op <- options(raadtools.shim.warn = FALSE)
  on.exit(options(op), add = TRUE)
  force(expr)
}


nc_rawdata <- function(x, var) {
  nc <- ncdf4::nc_open(x)
  on.exit(ncdf4::nc_close(nc))
  ncdf4::ncvar_get(nc, var)
}

xrange <- function(x) c(raster::xmin(x), raster::xmax(x))
yrange <- function(x) c(raster::ymin(x), raster::ymax(x))

update <- function() {
  cat('\ndevtools::install_github("AustralianAntarcticDivision/raadtools")\n\n')
}

set_utc_format <- function(x) {
  attr(x, "tz") <- "UTC"
  x
}
## Terra rotate for a full-globe grid in PROJECTED coordinates. terra::rotate()
## only handles longitude/latitude, and these grids are Mercator metres, so the
## right half is cropped off and shifted a full width to the left. Same result
## as the non-inverse branch of the old raster .rotate(), now in archive/.
.rotate_projected <- function(x) {
  e <- as.vector(terra::ext(x))
  xrange <- e[["xmax"]] - e[["xmin"]]
  hx <- e[["xmin"]] + xrange / 2
  left <- terra::crop(x, terra::ext(e[["xmin"]], hx, e[["ymin"]], e[["ymax"]]))
  right <- terra::shift(terra::crop(x, terra::ext(hx, e[["xmax"]], e[["ymin"]], e[["ymax"]])),
                        dx = -xrange)
  out <- terra::merge(right, left)
  names(out) <- names(x)
  out
}


## shared stuff
## datadir
## normalize input dates - need index and value

## private, but common
## dims, projection, bbox
## files
.processFiles <- function(dt, f, tr) {
  findex <- .processDates(dt, f$date, tr)
 
  f <- f[findex, ]
  
  op <- getOption("raadtools.check.file.exists")
  ## first column that is "fullname" or "ufullname"
  nm <- grep("fullname$", names(f))
  if (isTRUE(op) && length(nm) > 0) {
    # ## but they might be /vsi protocol ...
    # browser()
    # if (grep("^/vsi", f[[nm[1]]])) {
    #   
    # } else {
    # exists <- fs::file_exists(f[[nm[1]]])
    # f <- f[exists, ]
    # 
    # if (nrow(f) < 1) stop("no files exist for this dataset!")
    # }
  }
  f
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
              daytest = switch(timeres, "4hourly" = 1/6, "12hourly" = 1/2, "3hourly" = 1/8, "6hourly" = 0.25, daily = 1.5, weekly = 4, monthly = 28, 
                               weekly3 = 26, "8daily" = 12, "8D" = 12))

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
  as.POSIXct(x, tz = "UTC", ...)
}
