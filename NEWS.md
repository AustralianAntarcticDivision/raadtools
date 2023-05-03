# raadtools dev

* readchla() now returns a mean from all input dates, and no longer supports Johnson alg. 

* Fix readcurr. 

* NSIDC netcdf files needed to disable 'rescale'. We don't currently control that. 

* New scheme for NSIDC netcdf files.  We now use these directly, there's no use for 'vrt_dsn'. Now depending
on raadfiles 0.1.3.9041, in raadfiles the dates are culled to be distinct. Here the empty dates (no variable exists in the shell netcdf), we remove those in `icefiles()`. 


* raadtools now imports package {reproj}, fixes #135. 

* New function `read_par()` contributed by Kimberlee Baldry @KimBaldry.

* Removed rgeos and maptools. 

* Removed rgdal imports, replaced with raster and reproj. 


*Function `readice()` has been expanded to allow 'hemisphere = "both"' and for 'xylim' to be a full raster grid (terra or raster format). If 'both' is specified the warper is applied to VRT versions of the NSIDC files, which allows them to be combined in one reprojection step. In this case 'xylim' can be specified, to give a projected grid of any form. If not supplied (when hemisphere = 'both') then longlat raster at 0.25 degrees is assumed. 

* New function `read_ccmp()` and `ccmp_files()` for RSS Cross-Calibrated Multi-Platform Ocean Surface Wind Project. 

* Function `topofile()` now returns VRT-augmented text for some sources where missing metadata needs to be added,
 not always just a file path.
 
* Function `readtopo()` and `readbathy()` now accept `terra::ext` or
`raster::extent` or just numeric objects for 'xylim'. Alternatively these can be
a terra `SpatRaster` or a raster `BasicRaster` (RasterLayer, RasterBrick, or
RasterStack) as a template target raster for crop and resize, or reprojection to
new raster grid.  The 'resample' argument controls the kind of sampling when
regridded or warped. (i.e. if xylim is extent, you get crop(), if it's a grid,
you get it remodelled to the grid. extent doesn't need crs, grid does).


* Now using imports from terra and vapour, primarlily for `topofile()` and `readtopo()` for now. 

* `readtopo()` and `topofile()` source "ga_srtm" is no longer available (better done with NASADEM_be and gdal warp anyhow).  Also
 "george_v_terre_adelie" and "srtm" no longer available. 

* Fixed documentation for units in surface currents, thanks Ryan Reisinger. #117

* New function `query_grid()` to build a query data frame for `extract(function, ...)` workflows. 

* New function `readCHL_month()` to read NASA's monthly CHL files. 

* New function `table_vgos()` to read surface current values as a data frame. 

* `readcurr()` and other altimetry read functions now behave consistently for use of 'lon180', default is Atlantic view but can be set to FALSE to give Pacific 
 view (and behaves correctly no matter the source file arrangement). The other functions include `readssh()`, `read_sla_daily()`, `read_ugosa_daily()`, 
 `read_vgosa_daily()`, `read_ugos_daily()`, `read_vgos_daily()`, `read_err_daily()`. 
 
 
* New data set `stations` with locations of Mawson, Davis, and Casey. 

* New functions `read_amsre_ice()`, `read_amsr2_ice()`, `read_amsr2_3k_ice()`  to round out read
of AMRS2 and AMSRE at 6km and 3km resolutions using new file list functions from raadfiles. 
 
* AMSR read ice functions now correctly apply `xylim` argument. 

* Lead frequency functions `read_leads_clim()`, `read_leads_north_clim()`, `read_leads_south_clim()`. 

# raadtools 0.6.0

* Updated 'kerguelen' topo to revised version (2019). 

* Function `readfastice()` now defaults to the 2020 updated circumpolar files. 

* Now importing `fasticefiles()` from raadfiles. 

* Align to updated raadfiles (0.1.3.9006) for new OISST file paths. 

# raadtools 0.5.5

* readamps_d1wind() now correctly read the right bands. New function readamps_d2wind() to read the d2 grid. 

* Bit of a clean up around AMPS and Copernicus documentation. 

* New function `read_cersat_ice()` for 12.5 km SSM/I. 

* New function `read_geoid()` to read geoid. 

* New function `read_sose()` to read SOSE Southern Ocean State Estimate layers by level and 
date, no capacity for xylim or multiple dates (only monthly for now). Explore available variables
with `sose_monthly_varnames()` and available files with 'raadfiles::sose_monthly_files()'. 

* Add SMAP functions `salfiles()` and `readsal()` for surface salinity. 

* Fixed longstanding bugs in several functions that treated `latest = TRUE` as an override of the date argument. The latest
 argument now only applies if date is missing.  Thanks to @maierhofert  https://github.com/AustralianAntarcticDivision/raadtools/issues/96. 
 
 

# raadtools 0.5.2

* Add GEBCO 2019 "gebco_19" topography. 

* `readchla()` is now parallelized with furrr, and includes a grid specification argument


* `distance_to_ice()` and `distance_to_ice_edge()` now include the date on the output and are
 strictly not vectorized. 
 
* new function readfsle. 

# raadtools 0.5.0

* Removed dependency on 'sosoc/croc' package, by internalizing the imported functions. 

* Now using new `raadfiles` approach. 

* New function to return polar coastline with 'polar_map' function. 

* First parts of REMA data now available from `readtopo`, with "rema_8m", "rema_100m" and `read_rema_tiles` (the 8m index). 

* AMPS data now supports basic access for 10km "d2" files, with `amps_d1files` moved to raadfiles. 

* Fixed the grid specification for d1 AMPS (and added d2). 

* New topo option "ga_srtm" for the Australian 1-second DEM (~30m), file available only on nectar for now.

* New `readice_area` function for "NSIDC SMMR-SSM/I Nasateam sea ice concentration". 

* Topography `readtopo` and `topofile` gain `topo = "ramp"` option for Radarsat 
Antarctic digital elevation model V2, elevations relative to the the OSU91a geoid.  

* Topography functions `readtopo` and `topofile` now support `topo = 'ibcso_is'` and `topo = 'ibcso_bed'` for the ice surface and bedrock versions respectively. The default 'ibcso' remains synonomous with 'ibcso_is'. 

* "cryosat2" and "lake_superior" added to readtopo

* establish readice_daily and readice_monthly model for future separation

* re-instated monthly NSIDC north and south hemisphere

* now use raadfiles for AMPS

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
