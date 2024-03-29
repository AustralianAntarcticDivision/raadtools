% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fastice.R
\name{readfastice}
\alias{readfastice}
\title{Fast ice data}
\usage{
readfastice(
  date,
  product = c("circum_fast_ice", "binary_fast_ice"),
  xylim = NULL,
  latest = TRUE,
  returnfiles = FALSE,
  ...,
  inputfiles = NULL
)
}
\arguments{
\item{date}{date or dates to read (can be character, POSIXt, or Date)}

\item{product}{'circum_fast_ice' or 'binary_fast_ice'}

\item{xylim}{extent in native space of the grid}

\item{returnfiles}{return the file details only}

\item{...}{reserved for future use, currently ignored}

\item{time.resolution}{fixed, the underlying time step is 15 days}
}
\value{
RasterBrick see Details
}
\description{
Fast ice data
}
\details{
High-resolution mapping of circum-Antarctic landfast sea ice distribution, 2000–2018.

Fast ice data on original polar stereographic grid, the product "circum_fast_ice"
is 1000m resolution published in  \href{https://doi.org/10.5194/essd-12-2987-2020}{Alex Fraser et al. (2020)}.
}
\section{Circumpolar product from 2020}{


Classified surface type:
\itemize{
\item 0: pack ice or ocean
\item 1: continent
\item 2: islands
\item 3: ice shelf
\item 4: fast ice
\item 5: manual fast ice edge
\item 6: auto fast ice edge
}
}

\section{Old binary product}{

\itemize{
\item 0: Southern Ocean, pack ice or icebergs, corresponding to light blue in the PNG files.
\item 1: Antarctic continent (including ice shelves), as defined using the Mosaic of Antarctica product, corresponding to white in the PNG files.
\item 2: Fast ice, as classified from a single 20-day MODIS composite image, corresponding to dark blue in the PNG files
\item 3: Fast ice, as classified using a single 20-day AMSR-E composite image, corresponding to yellow in the PNG files
\item 4: Fast ice, as classified using the previous or next 20-day MODIS composite images, corresponding to red in the PNG files
}

\url{http://data.aad.gov.au/aadc/metadata/metadata.cfm?entry_id=modis_20day_fast_ice}
}

\examples{
## read a particular date, it's circumpolar grid with 7 discrete numerc classes
fice <- readfastice("2015-10-01")
## hone in on Davis
ex <- c(1742836L, 3315135L, 129376L, 1136611L)
davis_ice <- crop(fice, extent(c(1742836L, 3315135L, 129376L, 1136611L)))
plot(davis_ice >= 4) #, col = c("brown", "white", grey(c(0.2, 0.5, 0.8))), breaks = c(0, 1, 3, 4, 5, 6))

## compare 5 years change
davis_ice2 <- crop(readfastice("2010-10-01"), extent(ex))
par(mfrow = c(2, 1))
plot(davis_ice >= 4)
plot(davis_ice2 >= 4) 
}
