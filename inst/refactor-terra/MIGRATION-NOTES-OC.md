# raadtools terra-migration: Ocean Colour (Chlorophyll, PAR)

## Files

```
raadtools/
├── R/
│   ├── read-oc.R     # NEW: terra-native readers
│   └── compat-oc.R   # NEW: backward-compatible shims
└── tests/testthat/
    └── test-read-oc.R
```

## New functions

| Function | Sensors | Resolution | Returns |
|----------|---------|------------|---------|
| `read_oc_chl_daily()` | SeaWiFS + MODISA + VIIRS | daily | SpatRaster |
| `read_oc_chl_8day()` | SeaWiFS + MODISA + VIIRS | 8-day | SpatRaster |
| `read_oc_chl_monthly()` | SeaWiFS + MODISA + VIIRS | monthly | SpatRaster |
| `read_oc_par_8day()` | MODISA | 8-day | SpatRaster |

## Legacy shims

| Shim | Dispatches to | Returns |
|------|---------------|---------|
| `read_chla_daily()` | `read_oc_chl_daily()` | RasterBrick |
| `read_chla_weekly()` | `read_oc_chl_8day()` | RasterBrick |
| `read_chla_monthly()` | `read_oc_chl_monthly()` | RasterBrick |
| `read_par()` | `read_oc_par_8day()` | RasterStack |

## Integration

1. Drop files into raadtools/

2. In `chla.R`:
   - **KEEP**: `ocfiles()` - file catalog, works fine
   - **KEEP**: `.multi_era_chlafiles()` → renamed to `.multi_era_ocfiles()` in read-oc.R
   - **COMMENT OUT**: `read_chla_daily()`, `read_chla_weekly()`, `read_chla_monthly()`
   - **DELETE**: `readCHL_month()` - redundant

3. In `read_par.R`:
   - **COMMENT OUT**: `read_par()` - replaced by shim

4. Run tests: `devtools::test(filter = "read-oc")`

## What stays in chla.R (for now)

These functions are **specialized** and should eventually move to a separate
package for L3 bin / Southern Ocean ocean colour work:

```r
# L3 bin infrastructure (elegant sinusoidal DGGS!)
.init_bin()
.lat2_row()
.lonlat2_bin()
.crop_init()
bin_chl()

# Southern Ocean derived products
readchla_mean()
read_oc_sochla()
read_oc_sochla_day()
oc_sochla_files()

# Legacy averaging function
readchla()       # uses vapour VRT for on-the-fly averaging
chlafiles()      # for old johnson product
readchla_old()   # defunct
```

These are niche tools for binned data and Southern Ocean ecosystem work.
Consider extracting to `sochla` or `oceancolour` package.

## Sensor coverage

| Sensor | Start | End | Notes |
|--------|-------|-----|-------|
| SeaWiFS | 1997-09 | 2010-12 | GAC reprocessing |
| MODISA | 2002-07 | present | Primary sensor |
| VIIRS | 2012-01 | present | SNPP platform |

The `read_oc_chl_*` functions combine all available sensors into a continuous
time series, keeping one observation per date (MODISA preferred when overlapping).

## File naming conventions

NASA changed file naming in October 2019:
- Old: `A2020366.L3b_DAY_RRS.nc`
- New: `AQUA_MODIS.20201231.L3b.DAY.RRS.nc`

`ocfiles()` handles both conventions - no changes needed.

## Why "8day" not "weekly"?

NASA composites are exactly 8 days, not 7. Using `8day` is more accurate
and matches NASA's naming (`8D`). The legacy `read_chla_weekly()` shim
maps to the 8-day products.
