%\VignetteEngine{knitr::knitr}
%\VignetteIndexEntry{Installing and maintaining raadtools}

# Installing the raadtools package, Michael Sumner

This document is currently relevant to working on Windows, much of the
same stuff will work on Linux but no care has been taken to point out
the differences here.

## Setup

To install the __raadtools__ package from within the AAD you
should be able to run the following code from within R. A relatively
recent version of R will be assumed, and definitely no earler than
__R 2.15.0__ can be expected to work.

This is the basic full install and configuration, see below for each step in greater
detail.


```r
install.packages(c("maptools", "raster", "testthat", "ncdf", "rgdal", "rgeos", "knitr"),
                 dependencies = TRUE)
devtools::install_github("AustralianAntarcticDivision/raadtools")
```

Now to load the package, use the usual package loading code here. If
there is a problem with determining the path to the data repository
this will provide a message about that.


```r
library(raadtools)
```

If a message reports that your data repository cannot be found, follow
the instructions to set it and send this package author your details.

The instructions go like this, by setting an option called
__default.datadir__, note that this only sets this for the current R
session and you will have to repeat it each time until a better
solution is developed.


```r
options(default.datadir = "/path/to/repository/of/data")
```

(The tip of the path there "data" should include files such as "chl",
"wind", "sst", etc. i.e. it looks like data/chl, data/wind, data/sst
which hold the individual families of the main data types).

## Setup details

The remaining text in this section repeats the installation steps
above individually, this might be required for customized
installations or development.

If the following step fails you may not have your network settings
setup appropriately, so first run __setInternet2__ and then try to
install __raster__ again. (This option can be set for an R installation
by choosing "Internet2" when R is installed, and this is what you
should do. Otherwise, R is launced with "Rgui.exe" and this can be
invoked with the option to setInternet2 by using __Rgui.exe --internet2__ 
which can be set on a Windows shortcut under Properties).


```r
setInternet2(TRUE)
install.packages(c("raster", "maptools"))
```

The following packages are not essential to run __raadtools__ but
otherwise some features will not be available. To get the full
functionality please do run the following installations as well.



```r
install.packages("ncdf", "rgdal", "rgeos", "devtools", "testthat")
```

Finally, install the latest version of the package. 


```r
devtools::install_github("AustralianAntarcticDivision/raadtools")
```

## Development of raadtools
In order to contribute to the development of raadtools, the following options are available. 

- Complain, report, plead, compel the package author/s. Polite emails
  are recommended and hardware store vouchers welcome, but do what you must to get the message through.
- Get hold of the source, make your changes and send patches to the author/s. See previous option. 
- Check out the source with Git, and push your changes through to the source repository. 

The following section expands on the Git option. 

## Process of committing changes to the Git source of raadtools

The raadtools package source is hosted on GithHub. 

https://github.com/AustralianAntarcticDivision/raadtools

### Install prerequisites for building raadtools

1. Install the most recent version of R, and set its location in your PATH. 
2. Install [RTools](http://cran.r-project.org/bin/windows/Rtools/) relevant to the latest version of R, and elect that it sets its location in your PATH (the installer can do this). 
3. Install [MikTeX](http://miktex.org/download) and ensure it is in your PATH (later versions do this on install). 
4. Install the full list of package dependencies listed above. 


