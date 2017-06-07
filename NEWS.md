# raadtools 0.3.2.9005

* now imports orsifronts rather than using old bespoke copies

* raadtools now imports raadfiles, for OISST and GHRSST files

* ghrsst gains inputfiles, and is now useably fast for track extraction

* AMSR2 is back (per readice(product = "amsr")) and the file listing now relies on RSQLite
 via dplyr and is quite a bit faster
 
* change approach for readsst, to err on side of memory use

* add distance to ice 

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
