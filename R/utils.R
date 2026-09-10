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
  suppressWarnings(
   terra::rast(x, ..., md = FALSE)
  )
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
    ## the extent is a slot on the object, so this still accepts a raster
    ## object without raster being loaded
    e <- x@extent
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

## Which readers have already announced themselves this session. An
## environment rather than an option, because the notice is per function and
## an option is one global switch; options(raadtools.shim.warn = FALSE) stays
## the master off switch.
.shim_said <- new.env(parent = emptyenv())

## Forget what has been said, so the next call to each reader announces itself
## again. For tests, and for anyone who wants to see the notices a second time.
.shim_notice_reset <- function() {
  rm(list = ls(.shim_said, all.names = TRUE), envir = .shim_said)
  invisible(NULL)
}

## The legacy read* names are not going away - they are the front doors that
## pick a sensible default. What changed is the return class, so that is what
## we announce, alongside the specific reader this call resolved to.
##
## Once per function per session. Saying it on every call punishes exactly the
## code that is doing the right thing, since reading a time series means
## calling the same reader in a loop; once is enough to send someone to the
## vignette, which is where the detail belongs.
.shim_notice <- function(fun, specific = NULL) {
  if (!isTRUE(getOption("raadtools.shim.warn", TRUE))) return(invisible(NULL))
  if (exists(fun, envir = .shim_said, inherits = FALSE)) return(invisible(NULL))
  assign(fun, TRUE, envir = .shim_said)
  msg <- sprintf("'%s' now returns a terra SpatRaster, not a Raster* object.", fun)
  if (!is.null(specific)) {
    msg <- paste0(msg, sprintf("\n  This call reads through '%s'.", specific))
  }
  msg <- paste0(msg,
                "\n  See vignette(\"terra-transition\", package = \"raadtools\").",
                "\n  Said once per function per session;",
                " options(raadtools.shim.warn = FALSE) turns it off entirely.")
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


## duckdb and DBI are in Suggests, not Imports. Only the time_since_melt
## reader touches them, and loading duckdb costs about 0.4 s of every
## library(raadtools) whether or not anybody calls it.
##
## dbplyr too: dplyr::tbl() on a DBIConnection dispatches to it, so the reader
## has always needed it, and it was never declared.
.need_duckdb <- function() {
  missing <- Filter(function(p) !requireNamespace(p, quietly = TRUE),
                    c("DBI", "duckdb", "dbplyr"))
  if (length(missing)) {
    quoted <- sprintf("'%s'", missing)
    listed <- if (length(quoted) > 1L) {
      n <- length(quoted)
      paste(paste(quoted[-n], collapse = ", "), "and", quoted[n])
    } else quoted
    stop(sprintf("reading time_since_melt needs %s.\n  install.packages(c(%s))",
                 listed, paste(sprintf('"%s"', missing), collapse = ", ")),
         call. = FALSE)
  }
  invisible(NULL)
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
