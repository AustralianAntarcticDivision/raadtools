# raadtools terra-migration: AMSR and CERSAT Ice

## Files

```
raadtools/
├── R/
│   └── read-amsr-ice.R    # NEW: terra-native readers
└── tests/testthat/
    └── test-read-amsr-ice.R
```

## New functions

| Function | Resolution | Sensor | Coverage |
|----------|------------|--------|----------|
| `read_amsr_ice_daily()` | 6.25km | AMSR-E + AMSR2 | 2002+ |
| `read_amsr_ice_3k_daily()` | 3.125km | AMSR2 only | 2012+ |
| `read_cersat_ice_daily()` | 12.5km | SSM/I | 1991+ |

All return SpatRaster in Antarctic polar stereographic projection.

## Aliases (backward compat)

Old names kept as aliases:
- `read_amsr_ice()` → `read_amsr_ice_daily()`
- `read_amsr2_3k_ice()` → `read_amsr_ice_3k_daily()`
- `read_cersat_ice()` → `read_cersat_ice_daily()`

## Integration

1. Drop files into raadtools/
2. In `amsr.R` (or wherever these live), comment out the old functions
3. Keep the `.antarctic_extent()` and `.antarctic_crs()` helpers (or use the ones in read-amsr-ice.R)
4. Run tests: `devtools::test(filter = "read-amsr")`

## Key changes

**terra translations:**
- `raster::flip(r, "y")` → `terra::flip(r, direction = "vertical")`
- `raster::extent(r) <- ext` → `terra::ext(r) <- terra::ext(ext)`
- `raster::projection(r) <-` → `terra::crs(r) <-`
- `raster::setExtent()` → `terra::ext<-`
- `raster::brick(purrr::map(...))` → `terra::rast(lapply(...))`

**AMSR-E scaling:**
The older AMSR-E files (pattern `asi.nl.s6250`) have values 0-1, need *100 for percentage.
This is handled automatically in `read_amsr_ice_daily()`.

**CERSAT flip:**
CERSAT NetCDF files have y-axis inverted, needs `flip(direction = "vertical")`.

## Data characteristics

All products:
- Antarctic polar stereographic projection
- Extent: -3950000, 3950000, -3950000, 4350000
- CRS: +proj=stere +lat_0=-90 +lat_ts=-70 +lon_0=0 ...
- Values: 0-100 (percentage ice concentration)

Resolutions:
- AMSR 6k: 1328 × 1264 pixels
- AMSR2 3k: 2656 × 2528 pixels
- CERSAT: varies (12.5km nominal)

## What about read_amsre_ice and read_amsr2_ice?

The original code has several variants:
- `read_amsre_ice()` - AMSR-E only (deprecated sensor)
- `read_amsr2_ice()` - AMSR2 6km only
- `read_amsr_ice()` - combined AMSR-E + AMSR2

For the terra migration, we're keeping:
- `read_amsr_ice_daily()` - the combined series (most useful)
- `read_amsr_ice_3k_daily()` - high-res AMSR2

The single-sensor variants (`read_amsre_ice`, `read_amsr2_ice`) can be deprecated
or kept as simple aliases that filter the file list by date range.
