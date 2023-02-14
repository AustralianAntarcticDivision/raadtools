.dump_md <- function(x0, in_band = FALSE) {
  ## split on newlines
  x <- strsplit(x0, "\n")[[1]]
  s0 <- grep("<Metadata>", x)[1L]
  s1 <- grep("</Metadata>", x)[1L]
  e0 <- grep("<VRTRasterBand", x)
  ## abort
  if (length(c(s0, s1)) < 2L || is.na(s0) || is.na(s0) || s1 <= s0) return(x0)
  if (!in_band && length(e0) > 0 && e0[1] < s1) return(x0)
  paste0(x[-c(seq(s0, s1))], collapse = "\n")
}
.vrt_dsn <- function(filepaths, sds = NULL, bands =  NULL, projection = NULL, extent = NULL) {
  f1 <- filepaths[1]
  #info <- vapour::vapour_raster_info(f1)
  #if (length(bands) > 1) bands_process <- bands
  vrt_dsn <- vapour::vapour_vrt(f1, sds = sds, projection = projection, bands = bands, extent = extent)
  ## else blows up the size of the file list (could do later after temporal subset)
  vrt_dsn <- .dump_md(vrt_dsn)
  #template <- stringr::str_replace(vrt_dsn, f1, "%s")
  template <- gsub(f1, "%s", template)
  sprintf(rep(template, length(filepaths)), filepaths, filepaths, filepaths, filepaths)
  
}

## from gdalio
.handle_args <- function (x, extent = NULL, dimension = NULL, projection = NULL) 
{
  if (is.null(x)) 
    x <- list()
  if (!is.null(extent)) 
    x$extent <- extent
  if (!is.null(dimension)) 
    x$dimension <- dimension
  if (!is.null(projection)) 
    x$projection <- projection
  x
}
.default_dimension <- function (ex = NULL) 
{
  rat <- 1
  if (!is.null(ex)) {
    rat <- diff(ex[1:2])/diff(ex[3:4])
  }
  dfd <- c(1024, 1024)
  sort(c(1/rat, 1), decreasing = TRUE) * dfd
}
.griddish <- function (x, extent = NULL, dimension = NULL, projection = NULL, info = NULL) 
{
  if (isS4(x) && inherits(x, "BasicRaster")) {
    x <- list(extent = c(x@extent@xmin, x@extent@xmax, x@extent@ymin, 
                         x@extent@ymax), dimension = c(x@ncols, x@nrows), 
              projection = x@crs@projargs)
  }
  if (isS4(x) && inherits(x, "SpatRaster")) {
    x <- try(list(extent = x@ptr$extent@.xData[["vector"]], 
                  dimension = c(x@ptr$ncol(), x@ptr$nrow()), projection = x@ptr$get_crs("wkt")), 
             silent = TRUE)
    if (inherits(x, "try-error")) 
      stop("cannot use terra object to set default grid")
  }
  if (inherits(x, "stars")) {
    d <- attr(x, "dimension")
    ex <- c(c(d[[1]]$from - 1, d[[1]]$to) * d[[1]]$delta + 
              d[[1]]$offset, c(d[[2]]$from - 1, d[[2]]$to) * -d[[2]]$delta - 
              d[[2]]$offset)
    dim <- c(d[[1]]$to - d[[1]]$from + 1, d[[2]]$to - d[[2]]$from + 
               1)
    crs <- d[[1]]$refsys[["wkt"]]
    x <- list(extent = ex, dimension = dim, projection = crs)
  }
  if (inherits(x, "sf")) {
    ex <- attr(x[[attr(x, "sf_column")]], "bbox")[c("xmin", 
                                                    "xmax", "ymin", "ymax")]
    crs <- attr(x[[attr(x, "sf_column")]], "crs")$wkt
    dim <- .default_dimension(ex)
    x <- list(extent = ex, dimension = dim, projection = crs)
  }
  if (inherits(x, "SpatVector")) {
    ex <- x@ptr$extent()@.xData[["vector"]]
    crs <- x@ptr$get_crs("wkt")
    dim <- .default_dimension(ex)
    x <- list(extent = ex, dimension = dim, projection = crs)
  }
  if (inherits(x, "grd_rct")) {
    rct <- unclass(x$bbox[1])
    ex <- c(rct$xmin, rct$xmax, rct$ymin, rct$ymax)
    crs <- attr(rct, "crs")
    dim <- dim(x$data)[1:2]
    x <- list(extent = ex, dimension = dim, projection = crs)
  }
  if (is.character(x)) {
    tst <- try(vapour::vapour_raster_info(x))
    if (!inherits(tst, "try-error")) {
      tst$dimension <- tst$dimXY
      x <- tst[c("extent", "dimension", "projection")]
    }
  }
  x <- .handle_args(x, extent, dimension, projection)
  has_extent <- is.numeric(x[["extent"]]) && length(x[["extent"]] == 
                                                      4) && all(!is.na(x[["extent"]])) && diff(x[["extent"]][1:2]) > 
    0 && diff(x[["extent"]][3:4]) > 0
  if (!has_extent) 
    stop("invalid extent")
  if ("dimXY" %in% names(x)) {
    x[["dimension"]] <- x[["dimXY"]]
  }
  has_dim <- is.numeric(x[["dimension"]]) && length(x[["dimension"]]) == 
    2 && all(!is.na(x[["dimension"]])) && all(x[["dim"]] > 
                                                0)
  if (!has_dim) 
    stop("invalid dimension")
  has_proj <- is.character(x[["projection"]]) && length(x[["projection"]]) == 
    1 && nchar(x[["projection"]]) > 0 && !is.na(x[["projection"]])
  if (!has_proj) 
    stop("invalid projection")
  x[c("extent", "dimension", "projection")]
}