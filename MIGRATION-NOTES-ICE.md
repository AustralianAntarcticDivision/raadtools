# raadtools terra-migration: NSIDC Ice batch

## Files in this archive

```
raadtools/
├── R/
│   ├── read-ice.R        # NEW: terra-native readers
│   └── compat-ice.R      # NEW: backward-compatible shim
└── tests/testthat/
    └── test-read-ice.R   # NEW: tests
```

## Integration steps

### 1. Copy files
Unzip over your raadtools directory - these are all new files, no conflicts.

### 2. Update NAMESPACE
Add exports:
```r
export(read_nsidc_ice_daily)
export(read_nsidc_ice_monthly)
# readice, readice_daily, readice_monthly already exported - shims replace existing
```

### 3. Handle ice.R conflict
The shim in `compat-ice.R` defines `readice()`, `readice_daily()`, `readice_monthly()`.

**Recommended approach:**
- Keep `ice.R` for: `icefiles()`, `readice_area()`, VRT templates, `bad_nsidc` reference
- Remove/comment: `readice()`, `readice_daily()`, `readice_monthly()` from `ice.R`
- Keep: `.get_both_hemisphere_files()` is duplicated in compat-ice.R, so either location works
- Keep: `read_ice_v2()`, `read_ice_internal()` can stay (compat-ice.R doesn't use them, 
  but they're not hurting anything)

### 4. Verify internal data
`bad_nsidc` is referenced - it should be in `R/sysdata.rda`. Confirm with:
```r
raadtools:::bad_nsidc
```

### 5. Run tests
```r
devtools::test(filter = "read-ice")
```

## Architecture notes

**Layer 1: Terra-native (read-ice.R)**
- `read_nsidc_ice_daily()` - single hemisphere, native projection, SpatRaster
- `read_nsidc_ice_monthly()` - single hemisphere, native projection, SpatRaster

**Layer 2: Legacy shim (compat-ice.R)**
- `readice()` - dispatches to Layer 1 for single hemisphere, keeps vapour warper for "both"
- `readice_daily()` - alias to `readice()`
- `readice_monthly()` - dispatches to `read_nsidc_ice_monthly()`

**Special handling:**
- `hemisphere = "both"` is NOT terra-formed - stays as vapour warper code
- `bad_nsidc` filtering happens in Layer 1 (same logic as before)
- `rescale` argument is ignored (v2 data is pre-scaled)

## Data notes

NSIDC v2 NetCDF files:
- Values 0-1 in file, multiply by 100 for percentage
- May have 0, 1, 2, or 3 sensor variables - we take first
- Some files are placeholder duds (no ice variable) - filtered by `bad_nsidc`
- CRS: EPSG:3976 (south), EPSG:3413 (north)

## What's NOT migrated (stays in legacy)

- `hemisphere = "both"` combined warping
- `icefiles()` - still needed, unchanged
- `readice_area()` - unchanged
- VRT templates for raw binary files (historical, probably unused now)
