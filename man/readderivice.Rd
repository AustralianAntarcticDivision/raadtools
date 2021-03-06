% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deriv.R
\name{readderivice}
\alias{readderivice}
\title{Read data from derived sea ice data products.}
\usage{
readderivice(
  date,
  time.resolution = c("daily"),
  product = c("time_since_melt"),
  xylim = NULL,
  latest = TRUE,
  returnfiles = FALSE,
  ...,
  inputfiles = NULL
)
}
\arguments{
\item{date}{date or dates of data to read, see Details}

\item{time.resolution}{time resoution data to read, daily or monthly}

\item{product}{choice of sea ice product, see Details}

\item{xylim}{spatial extents to crop from source data, can be anything accepted by \code{\link[raster]{extent}}}

\item{latest}{if TRUE and date input missing, return the latest time available, otherwise the earliest}

\item{returnfiles}{ignore options and just return the file names and dates}

\item{...}{passed to brick, primarily for \code{filename}}

\item{inputfiles}{input the file set to avoid rescanning that (for extract point-in-time)}
}
\value{
\code{\link[raster]{raster}} object
}
\description{
Derived sea ice data is read from files managed by \code{\link{derivicefiles}}.
}
\details{
Currently available products are

\describe{
}

Dates are matched to file names by finding the nearest match in
time within a short duration. If \code{date} is greater than
length 1 then the sorted set of unique matches is returned.

time_since_melt

32767 (treated as missing data)

32766 = land

32765 = open-ocean zone

-32768 = ice that hasn't melted during the data period

In terms of missing data 32767,  in the nc file, so should be NA once read into R): these are
either open water in the sea ice zone that hasn't re-frozen during the data period, or missing sea ice data that
couldn't be interpolated.
}
\seealso{
\code{\link{derivicefiles}} for details on the repository of
data files, \code{\link[raster]{raster}} for the return value
}
