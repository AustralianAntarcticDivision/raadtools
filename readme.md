# R tools for spatial data at the Australian Antarctic Division (AAD)

Tools for reading, plotting and manipulating spatial data used at the Australian Antarctic Division (AAD).

Michael D. Sumner michael.sumner@aad.gov.au


Extreme minimal install of raadtools sytem
--------------------------------

 1. specify configuration to download the sea ice for 2014 (southern)
 2. trigger raadsync to build the repository
 3.  simple example to read the ice data

Analogous facilities exist for SST, Aviso currents/ssh, ocean colour, NCEP winds, topography, etc. 

```{r,eval=FALSE}
## install packages
devtools::install_github("AustralianAntarcticDivision/raadtools")
devtools::install_github("AustralianAntarcticDataCentre/raadsync")

## 1. configure to download  the southern ocean NSDIC 25km ice for 2014 to a local repo 
library(raadsync)
cfg <- read_repo_config(system.file("extdata", "raad_repo_config.json", package= "raadsync"))
ice_index <- 1
cfg$do_sync <- seq(nrow(cfg)) == ice_index
## limit our data to only the last year
cfg$method_flags[1] <- paste0(cfg$method_flags[1], " --accept=\"*nt_2014*\"")
## specify local repository location
my_datadir <- normalizePath("~/raaddata")
options(default.datadir = my_datadir)
cfg$local_file_root <- file.path(my_datadir, "data")

## 2. trigger raadsync to build the repository (this is set up in a cron-task ultimately)
er <- sync_repo(cfg, create_root = TRUE)
## build the file cache (many, many files for some data, so needs a system shortcut )
my_datadir <- getOption("default.datadir")
my_admindir <- file.path(my_datadir, 'admin', 'filelist')
if (!file.exists(my_admindir)) dir.create(my_admindir, recursive = TRUE)
fs <- list.files(file.path(my_datadir, 'data'), all = TRUE, recursive = TRUE, full.names = TRUE, no.. = TRUE)
fs <- gsub(paste0(my_datadir, "/"), "", fs)
save(fs, file = file.path(my_datadir, 'admin', 'filelist', 'allfiles2.Rdata'))
writeLines(fs, file.path(my_datadir, 'admin', 'filelist', 'allfiles2.txt'))

## 3.  simple example to read the ice data
library(raadtools)
ice <- readice(latest  = TRUE)

# class       : RasterLayer 
# dimensions  : 332, 316, 104912  (nrow, ncol, ncell)
# resolution  : 25000, 25000  (x, y)
# extent      : -3950000, 3950000, -3950000, 4350000  (xmin, xmax, ymin, ymax)
# coord. ref. : +proj=stere +lat_0=-90 +lat_ts=-70 +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs 
# data source : in memory
# names       : nt_20141231_f17_v01_s.bin 
# values      : 0, 100  (min, max)
# time        : 2014-12-31


```

Related projects
------------------------------

https://github.com/mdsumner/roc - read ocean colour files catalogued by raadtools::ocfiles()

https://github.com/ropensci/rerddap

https://ropensci.org/tutorials/rnoaa_tutorial.html

https://github.com/rmendels/xtractomatic



