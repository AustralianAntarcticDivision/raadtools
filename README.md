
<!-- README.md is generated from README.Rmd. Please edit that file -->
<<<<<<< HEAD
[![Travis-CI Build Status](https://travis-ci.org/AustralianAntarcticDivision/raadtools.svg?branch=master)](https://travis-ci.org/AustralianAntarcticDivision/raadtools) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/AustralianAntarcticDivision/raadtools?branch=master&svg=true)](https://ci.appveyor.com/project/AustralianAntarcticDivision/raadtools)
=======
>>>>>>> master

# R tools for spatial data at the Australian Antarctic Division (AAD)

Tools for reading, plotting and manipulating spatial data used at the
Australian Antarctic Division (AAD).

<<<<<<< HEAD
The repository of data used by raadtools is available under RDSI/PUBLIC/raad and at the AAD under gridded/new/ in the scientific data collection.

Anyone with a [Nectar account](https://dashboard.rc.nectar.org.au) may run this by creating a VM from our `raadclient` image. Search the public images for **raadclient** (e.g. 'raadclient04\_20180513' but choose the latest one) and ensure that the SSH and RStudio port (8787) is open. Use the default *rstudio/rstudio* account, or create your own.

The [contents of the repository](https://github.com/AustralianAntarcticDivision/blueant#data-source-summary) is listed in the technical configuration for bowerbird.

If you would like a collection added please make a request via a [Github issue](https://github.com/AustralianAntarcticDivision/bowerbird/issues/new) or contact one of the authors directly.

You are welcome to make your own copies of data from the collection for your own use, but please respect the citation and usage requests of the data providers [listed in the summary](https://github.com/AustralianAntarcticDivision/blueant#data-source-summary).

Using raadtools
===============

There are two main ways to use it.
=======
The typical use-cases for raadtools are
>>>>>>> master

  - read a time series gridded data set as a function of date,
    optionally with spatial subsetting
  - match a data set of longitude, latitude, time to the corresponding
    value in a time series gridded data set

Examples of these workflows are outlined in this [rOpenSci blog
post](https://ropensci.org/blog/2018/11/13/antarctic/).

There are oceanography, topography, meteorology, altimetry, sea ice,
ocean colour, and many other data sources. These are mostly remote
sensing but including re-analysis and model output products.

The package uses the R [raster
package](https://CRAN.R-project.org/package=raster) and always provides
data as a standard raster (RasterLayer, RasterBrick, or RasterStack).
Each data set is invdividually handled by a function to ensure the
spatial and temporal registration is correct. There are several
`read*()` functions in raadtools, see the full list in the
documentation:

<http://australianantarcticdivision.github.io/raadtools/reference/index.html>

To build the underlying data collection raadtools relies on the
[bowerbird](https://github.com/ropensci/bowerbird) and
[blueant](https://github.com/AustralianAntarcticDivision/blueant)
packages.

# Acess to raadtools

Typically you will be provided with access, and won’t be aware of the
underlying details, but the repository of data used by raadtools is
available under RDSI/PUBLIC/raad and at the AAD under gridded\_new/ in
the scientific data collection.

Anyone with a [Nectar account](https://dashboard.rc.nectar.org.au) may
run this by creating a VM from our `raadclient` image. Search the public
images for **raadclient** (e.g. ’ raadclient06\_20181016’ but choose the
latest one) and ensure that the SSH and RStudio port (8787) is open. Use
the default *rstudio/rstudio* account, or create your own.

The [contents of the
repository](https://github.com/AustralianAntarcticDivision/blueant#data-source-summary)
is listed in the technical configuration for bowerbird.

If you would like a collection added please make a request via a [Github
issue](https://github.com/AustralianAntarcticDivision/bowerbird/issues/new)
or contact one of the authors directly.

You are welcome to make your own copies of data from the collection for
your own use, but please respect the citation and usage requests of the
data providers [listed in the
summary](https://github.com/AustralianAntarcticDivision/blueant#data-source-summary).

# Using raadtools

There are two main ways to use it.

## 1\. RStudio raadtools server

If you have access to a “raadtools-RStudio-server” then you need only
load the package to get started:

``` r
library(raadtools)
```

## 2\. Local computer, within the AAD network

If it’s not installed, trying installing with

``` r
devtools::install_github("AustralianAntarcticDivision/raadtools")
library(raadtools)
```

If neither of 1 or 2 work for you you, see your local raadtools expert.

Please note that the ‘raadtools’ project is released with a [Contributor
Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this project,
you agree to abide by its terms.
