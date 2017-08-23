# raadtools dev

* `readchla` returns, now providing MODISA/SeaWiFS at native mapped 
 resolution for a given set of input dates (the mean is calculated from
 the daily L3 bins) for either 'nasa' or 'johnson' algorithm
 
* removed prod files

* read rapid response is not fixable in place, but now uses the raadfiles-like approach

* read functions now default to `latest = TRUE` rather than returning the first available time

* `readssh` and `sshfiles` are now only daily, using the Copernicus sources

* new function `read_amsr_ice` to read the 6.25km southern hemisphere product

* readcurr/currentsfiles now uses new Copernicus source

* readice/icefiles now limited to daily NSIDC and using raadfiles

* readsst/sstfiles now uses raadfiles mechanism

* import from raadfiles for NCEP2 wind 6hourly files, removed the daily option for now

* now import from raadfiles for NSIDC monthly files, fixes bug https://github.com/AustralianAntarcticDivision/raadtools/issues/54

* modified AMSR-E Artist sea ice data location as per https://github.com/AustralianAntarcticDivision/raadtools/issues/52

* new Copernicus source for altimetry products from ftp.sltac.cls.fr/Core/ provided by new 
 functions `read_ugosa_daily`,`read_adt_daily`, `read_ugos_daily`, `read_sla_daily`, 
 `read_vgos_daily`, `read_vgosa_daily`, `read_err_daily`
 
* speed up for `extract(function, xyt)`

# raadtools 0.4.0

* a sensible value (4e9) for raster maxmemory is set on startup which
means that fewer temp files will be created unnecessarily

* `extract(fun, xyt)` now more efficient

* now imports orsifronts rather than using old bespoke copies

* startup is now much more robust

* raadtools now imports raadfiles, for OISST and GHRSST files

* `raadtools::sstfiles()` and c. now works without attaching the package

* ghrsst gains inputfiles, and is now useably fast for track extraction

* AMSR2 is back (per readice(product = "amsr")) and the file listing now relies on RSQLite
 via dplyr and is quite a bit faster
 
* change approach for readsst, to err on side of memory use

* add distance to ice function

* add metadata for AMPS data, and new general readamps function

* add extract method for trip objects

* level for amps files

* use inputfiles for extract, always

* big speedups for wind and currents file database loading

* improvements for readcurr in performance, and file handling

* new function readchl32 for rolling 32-day weekly files

* some improvement to the time resolution detection

* extract(function, data.frame) is now tibble friendly

* added AMPS data functions amps_d1files and readamps_d1wind

* converted main vignette from Sweave to RMarkdown

* package scaffolding


See ONEWS
