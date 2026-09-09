# archive

Code kept for the record and not built into the package. `.Rbuildignore`
excludes this directory, so nothing here is installed, checked or documented.

- `heuristic.R` - `read_sst_heuristic()`, internal experimentation and notes.
  Nothing else in the package called it, and it was never exported. Moved here
  2026-09-09 rather than converted to terra.

- `oc_L3deriv.R` - the Southern Ocean chlorophyll-a L3 bin machinery:
  `oc_sochla_files()`, `read_oc_sochla()`, `read_oc_sochla_day()`,
  `readchla_mean()`, the defunct `chla_johnsonfiles()`, and the bin helpers
  `.init_bin()`, `.lat2_row()`, `.lonlat2_bin()`, `.crop_init()`,
  `.snapout1()`, `.seqfl()` and `product2nrows()` that they used.

  These read per-day `.rds` files of L3 bin numbers and chlorophyll values
  under `acecrc.org.au/ocean_colour/`, produced by
  https://github.com/AustralianAntarcticDivision/ocean_colour and duplicating
  helpers from https://github.com/sosoc/croc. Nothing produces them any more:
  the MODISA collection runs 2002-07-03 to 2021-06-30 and stops there. Moved
  here 2026-09-09.

  If you want to bring this back, the pieces fit together like this.
  `.init_bin(NUMROWS)` builds the NASA integerised sinusoidal bin scheme -
  `latbin` (row centres), `numbin` (bins per row), `basebin` (first bin
  number of each row). `product2nrows()` maps MODISA to 4320 rows and SeaWiFS
  to 2160. `.lonlat2_bin()` turns a longitude and latitude into a bin number
  through that scheme, and `.crop_init()` turns an extent into the vector of
  bin numbers inside it, which is what let a read subset before touching the
  data. `read_oc_sochla()` maps over the daily files with furrr and inner
  joins on `bin_num`; `readchla_mean()` averaged the result over a date range.
  Everything came back as a tibble of bin numbers, never a raster - the
  function that gridded bins back onto a raster, `bin_chl()`, was already
  dead and was removed separately.

  Note `oc_sochla_files()` never validated `time.resolution`, so
  `oc_sochla_files("VIIRS")` silently set that argument and read MODISA. There
  was never a VIIRS collection here.

  The live ocean colour readers are in `R/read-oc.R` and read NASA L3m
  (mapped) files directly.
