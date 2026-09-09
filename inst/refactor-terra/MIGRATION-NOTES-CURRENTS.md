# raadtools terra-migration: Currents batch (Tier 2)

## Files in this archive

```
raadtools/
|-- R/
|   |-- read-currents.R   # NEW: terra-native reader
|   \-- compat-currents.R # NEW: backward-compatible shim
\-- tests/testthat/
    \-- test-read-currents.R
```

## Integration steps

### 1. Copy files
Unzip into parent of raadtools/ directory.

### 2. Update NAMESPACE
```r
export(read_copernicus_current_daily)
export(read_aviso_current_daily)  # alias
# readcurr, readcurrents already exported - shims replace
```

### 3. Handle currents.R conflict - DONE 2026-09-09
The shim in `compat-currents.R` defines `readcurr()` and `readcurrents()`.

`currents.R` now holds only `currentsfiles()` and the AVISO netCDF header
notes. Everything else went to `archive/currents.R`: `read_i_u()`,
`read_i_v()`, `read_i_uv()`, `read_i_dir()`, `read_i_mag()`, `vlen()`,
`readcurr_polar()`, `copernicus_is_atlantic()` (replaced by
`.needs_rotation()`), `copernicus_get_maxlon()`, `.currentsfiles1()` and
`.vrt_ds0()`, none of which had callers left. `.rotate()` went with them,
since those were its last callers.

### 4. Check raadfiles function name
The code assumes `raadfiles::altimetry_daily_files()` exists. If it's named differently
(e.g., `copernicus_current_daily_files()` or `aviso_daily_files()`), update line in
`read-currents.R`:

```r
files <- inputfiles %||% raadfiles::altimetry_daily_files()  # <- check this
```

### 5. Run tests
```r
devtools::test(filter = "read-currents")
```

## Architecture notes

**Layer 1: Terra-native (read-currents.R)**
- `read_copernicus_current_daily()` - returns SpatRaster
- `read_aviso_current_daily()` - alias to above

**Component flags:**
- Default: returns 2-layer raster (U, V)
- `magonly = TRUE`: current speed, sqrt(U^2 + V^2)
- `dironly = TRUE`: direction in degrees (oceanographic convention)
- `uonly = TRUE`: U component only
- `vonly = TRUE`: V component only

**Constraint:** Multiple dates only work with a component flag (single-layer output).
With default U+V output, only one date at a time.

**Layer 2: Legacy shim (compat-currents.R)**
- `readcurr()` - dispatches to Layer 1, converts to RasterBrick
- `readcurrents()` - alias

## Rotation logic

The `.needs_rotation()` helper checks if the raster extent has negative longitudes
(Atlantic view) or not (Pacific view), and rotates if needed to match `lon180` preference.

This replaces the `copernicus_is_atlantic()` approach which checked file paths.

## Dependencies

```r
# Functions used
terra::rast()
terra::rotate()
terra::crop()
terra::ext()
terra::atan2()  # for direction calculation
terra::c()      # for stacking U and V

# In shim
raster::brick()
raster::setZ()
```
