# raadtools terra-migration bundle

## Contents

```
raadtools/
├── R/
│   ├── read-ice.R         # terra-native NSIDC ice readers
│   ├── compat-ice.R       # readice() shim
│   ├── read-currents.R    # terra-native currents readers  
│   └── compat-currents.R  # readcurr() shim
└── tests/testthat/
    ├── test-read-ice.R
    └── test-read-currents.R
```

## Quick start

```bash
# Unzip into parent of raadtools/
unzip raadtools-terra-bundle.zip

# In R
devtools::test(filter = "read-ice|read-currents")
```

## New functions (Layer 1 - terra-native)

| Function | Returns | Notes |
|----------|---------|-------|
| `read_nsidc_ice_daily()` | SpatRaster | hemisphere = "south"/"north" |
| `read_nsidc_ice_monthly()` | SpatRaster | hemisphere = "south"/"north" |
| `read_copernicus_current_daily()` | SpatRaster | U/V or mag/dir/u/v |

## Shims (Layer 2 - backward compatible)

| Shim | Dispatches to | Returns |
|------|---------------|---------|
| `readice()` | `read_nsidc_ice_*()` | RasterBrick |
| `readcurr()` | `read_copernicus_current_daily()` | RasterBrick |

Shims emit deprecation warnings. Suppress with:
```r
options(raadtools.shim.warn = FALSE)
```

## Integration checklist

- [ ] Unzip into parent of raadtools/
- [ ] Comment out `readice()` in ice.R (keep `icefiles()`, `readice_area()`)
- [ ] Comment out `readcurr()`/`readcurrents()` in currents.R
- [ ] Verify `raadfiles::altimetry_daily_files()` is correct function name
- [ ] Ensure `bad_nsidc` exists in sysdata.rda (see ice.R data-raw notes)
- [ ] Run tests: `devtools::test(filter = "read-ice|read-currents")`
- [ ] Git commit on branch

## Dependencies to add to DESCRIPTION

```
Imports:
    terra,
    raster,
    raadfiles
```

## Key behaviors

**Ice setNA:** masks values ≤0 and >100 (land, coast, missing, polar hole)

**Ice hemisphere="both":** NOT terra-ified - stays as vapour warper in legacy shim

**Currents components:**
- Default: 2-layer (U, V), single date only
- `magonly=TRUE`: speed, multiple dates OK
- `dironly=TRUE`: direction (degrees), multiple dates OK
- `uonly/vonly=TRUE`: single component, multiple dates OK
