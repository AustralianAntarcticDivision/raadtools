
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Travis-CI Build Status](https://travis-ci.org/AustralianAntarcticDivision/raadtools.svg?branch=master)](https://travis-ci.org/AustralianAntarcticDivision/raadtools) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/AustralianAntarcticDivision/raadtools?branch=master&svg=true)](https://ci.appveyor.com/project/AustralianAntarcticDivision/raadtools)

R tools for spatial data at the Australian Antarctic Division (AAD)
===================================================================

Tools for reading, plotting and manipulating spatial data used at the Australian Antarctic Division (AAD).

The repository of data used by raadtools is available under RDSI/PUBLIC/raad and at the AAD under gridded/new/ in the scientific data collection.

Anyone with a [Nectar account](https://dashboard.rc.nectar.org.au) may run this by creating a VM from our `raadclient` image. Search the public images for **raadclient** (e.g. 'raadclient04\_20180513' but choose the latest one) and ensure that the SSH and RStudio port (8787) is open. Use the default *rstudio/rstudio* account, or create your own.

The [contents of the repository](https://github.com/AustralianAntarcticDivision/blueant#data-source-summary) is listed in the technical configuration for bowerbird.

If you would like a collection added please make a request via a [Github issue](https://github.com/AustralianAntarcticDivision/bowerbird/issues/new) or contact one of the authors directly.

You are welcome to make your own copies of data from the collection for your own use, but please respect the citation and usage requests of the data providers [listed in the summary](https://github.com/AustralianAntarcticDivision/blueant#data-source-summary).

Using raadtools
===============

There are two main ways to use it.

1. RStudio raadtools server
---------------------------

If you have access to a "raadtools-RStudio-server" then you need only load the package to get started:

``` r
library(raadtools)
```

2. Local computer, within the AAD network
-----------------------------------------

If it's not installed, trying installing with

``` r
devtools::install_github("AustralianAntarcticDivision/raadtools")
library(raadtools)
```

If neither of 1 or 2 work for you you, see your local administrator.

Development
-----------

raadsync keeps it up to date:

<https://github.com/AustralianAntarcticDataCentre/raadsync>

A configuration like this is required for the tools to work:

<https://github.com/mdsumner/nectar/blob/master/r-spatial.sh>

Related, or similar, projects
-----------------------------

<https://github.com/mdsumner/roc> - read ocean colour files catalogued by raadtools::ocfiles()

<https://github.com/ropensci/rerddap>

<https://ropensci.org/tutorials/rnoaa_tutorial.html>

<https://github.com/rmendels/xtractomatic>
