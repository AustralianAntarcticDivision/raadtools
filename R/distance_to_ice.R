## Contour a sea ice concentration layer at `threshold` and hand back the edge
## as lines.
##
## terra::as.contour() subsamples its input to `maxcells`, which defaults to
## 1e5 - smaller than the 332x316 southern NSIDC grid - so always give it the
## whole grid, otherwise the edge is drawn on a resampled raster.
.ice_contour <- function(ice, threshold) {
  cl <- try(terra::as.contour(ice, levels = threshold,
                              maxcells = terra::ncell(ice)), silent = TRUE)
  if (inherits(cl, "try-error") || nrow(cl) < 1L) {
    rng <- range(terra::values(ice), na.rm = TRUE)
    stop(sprintf("no sea ice contour at a concentration of %s (this layer runs from %s to %s)",
                 format(threshold), format(rng[1L]), format(rng[2L])))
  }
  ## as.contour() returns one geometry per level, with the separate pieces of
  ## the edge as parts of it; disagg() makes each piece its own line
  terra::disagg(cl)
}

## The shared body of distance_to_ice() and distance_to_ice_edge(): contour,
## optionally reduce the contour to the parts we want, measure.
##
## terra::distance() measures to the lines themselves, so a cell between two
## contour vertices gets its true distance to the edge rather than the
## distance to the nearest vertex, and it runs over the whole grid at once
## instead of once per vertex.
.distance_to_ice0 <- function(ice, date, threshold, min_length = NULL,
                              longest_only = FALSE, name = "distance_to_ice") {
  cl <- .ice_contour(ice, threshold)
  if (!is.null(min_length)) {
    len <- terra::perim(cl)
    keep <- len > min_length
    if (!any(keep)) {
      stop(sprintf("no sea ice contour longer than min_length = %s (the longest is %.0f m)",
                   format(min_length), max(len)))
    }
    cl <- cl[keep, ]
  } else if (longest_only) {
    cl <- cl[which.max(terra::perim(cl)), ]
  }
  out <- terra::distance(ice, cl)
  names(out) <- name
  .utctime(out) <- timedateFrom(date)
  out
}

## Both functions take the same arguments and resolve them the same way.
.distance_to_ice_setup <- function(date, hemisphere, product, latest,
                                   inputfiles, missing_date) {
  files <- if (!is.null(inputfiles)) inputfiles else icefiles(hemisphere = hemisphere, product = product)
  if (missing_date) {
    date <- if (latest) max(files$date) else min(files$date)
  }
  if (length(date) > 1L) {
    warning("'date' should be of length = 1, using first supplied")
    date <- date[1L]
  }
  list(files = files, date = date)
}

#' Distance to a sea ice 'edge'.
#'
#' Calculate the shortest distance (metres) to a threshold sea ice contour.  If in
#' doubt use `distance_to_ice_edge`, the definition of the edge is not straightforward, especially so for
#' the higher resolution products and near the coast.
#' `distance_to_ice_edge` computes a single "main" edge at continental scale
#' `distance_to_ice` computes all distances to any ice at threshold concentration
#'
#' The distance is always positive, use `readice` in the usual way to determine if a
#' location is inside or out of the ice field itself. (If inside means zero distance to ice
#' for you then set it explicitly based on the concentration a point is in.)
#'
#' The contour is measured as lines, so the distance reported for a cell is its
#' distance to the edge itself and not to the nearest contour vertex.
#'
#' The sea ice product contoured is whatever `readice` reads, which is the
#' NOAA/NSIDC Climate Data Record by default; pass `product = "nsidc"` for
#' NSIDC-0051 v2. Both are 25km products on which a single `min_length` free
#' `distance_to_ice_edge` gives a clean continental edge. Finer products (AMSR2
#' at 3km, say) break the edge into many pieces and are better served by setting
#' `min_length` to drop the short ones - but they are not available through
#' `readice`, so contour them directly with `terra::as.contour`.
#'
#' Future work may generalize this to other data sources.
#' @inheritParams readice
#' @param threshold the sea ice concentration threshold to contour at, as a
#'   fraction - 0.15 is the conventional 15 percent ice edge
#' @param hemisphere "north" or "south", default is "south"
#' @param min_length if set, keep every contour line longer than this many
#'   metres rather than only the single longest one
#' @return \code{SpatRaster} of distances (metres) to this date's sea ice edge
#' @export
#' @note beware that any queried location outside of this layer's range will be
#' undetermined, and the external boundary of this layer is not constant with
#' respect to the pole, and that in general a location may be closer to ice in the
#' opposite hemisphere.
#'
#' The argument `hemisphere` may be north or south (default is south), but this will only work if your locations
#' are on the actual map, so it's not possible to request the distance to ice in both poles for any point.
#' @examples
#' \dontrun{
#' terra::plot(distance_to_ice(latest = TRUE))
#' terra::plot(distance_to_ice_edge(latest = TRUE))
#' ## every piece of the edge longer than 500km, rather than just the longest
#' terra::plot(distance_to_ice_edge(min_length = 5e5))
#' extract(distance_to_ice, aurora[17:25, ])
#' extract(distance_to_ice, aurora[17:25, ], hemisphere = "south")
#' # library(trip)
#' # extract(distance_to_ice_edge, walrus818[seq(50, 400, by = 20), ], hemisphere = "north")
#' }
distance_to_ice_edge <- function(date, threshold = 0.15,  xylim = NULL,
                                 hemisphere = c("south", "north"),
                                 product = c("cdr", "nsidc"),
                                 min_length = NULL,
                                 returnfiles = FALSE,
                                 inputfiles = NULL,
                                 latest = TRUE, ...) {
  hemisphere <- match.arg(hemisphere)
  product <- match.arg(product)
  setup <- .distance_to_ice_setup(if (missing(date)) NULL else date, hemisphere,
                                  product, latest, inputfiles, missing(date))
  if (returnfiles) return(setup$files)
  ice <- .without_shim_warning(readice(setup$date, hemisphere = hemisphere,
                                       product = product, inputfiles = setup$files,
                                       setNA = FALSE, xylim = xylim))[[1L]]
  .distance_to_ice0(ice, setup$date, threshold = threshold,
                    min_length = min_length,
                    longest_only = is.null(min_length),
                    name = "distance_to_ice_edge")
}

#' @name distance_to_ice_edge
#' @export
distance_to_ice <- function(date, threshold = 0.15, xylim = NULL,
                            hemisphere = c("south", "north"),
                            product = c("cdr", "nsidc"),
                            returnfiles = FALSE, inputfiles = NULL,
                            latest = TRUE, ...) {
  hemisphere <- match.arg(hemisphere)
  product <- match.arg(product)
  setup <- .distance_to_ice_setup(if (missing(date)) NULL else date, hemisphere,
                                  product, latest, inputfiles, missing(date))
  if (returnfiles) return(setup$files)
  ice <- .without_shim_warning(readice(setup$date, hemisphere = hemisphere,
                                       product = product, inputfiles = setup$files,
                                       setNA = FALSE, xylim = xylim))[[1L]]
  .distance_to_ice0(ice, setup$date, threshold = threshold)
}
