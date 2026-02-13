# raadtools terra-migration: Topography/Bathymetry

## Files

```
raadtools/
├── R/
│   └── read-topo.R    # NEW: terra-native reader + shims
└── tests/testthat/
    └── test-read-topo.R
```

## New functions

```r
read_topo(topo, xylim, lon180, polar, resample, returnfiles)
read_bathy()  # alias
```

Returns SpatRaster. Dispatcher for ~20 datasets.

## Legacy shims

```r
readtopo()  # returns RasterLayer, handles BasicRaster xylim input
readbathy() # alias
```

## Integration

1. Drop files into raadtools/
2. In `topo.R`:
   - **Keep** `topofile()` - unchanged, used by `read_topo()`
   - **Keep** `read_rema_tiles()` - shapefile reader, separate concern
   - **Comment out** `readtopo()` and `readbathy()` - replaced by shims
3. Run tests: `devtools::test(filter = "read-topo")`

## Key changes

**xylim handling simplified:**
- Old: accepted numeric, Extent, SpatExtent, BasicRaster, SpatRaster
- New: accepts SpatRaster (for warp) or anything `terra::ext()` handles (for crop)
- Legacy shim converts BasicRaster → SpatRaster, Extent → numeric

**terra translations:**
- `raster::raster(tfile)` → `terra::rast(tfile)`
- `.rotate(r)` → `terra::rotate(r)`
- `raster::crop()` → `terra::crop()`
- `raster::projection()` → `terra::crs()`
- `terra::project()` already used - just remove `raster::raster()` wrapper

**Behavior:**
- `xylim` as SpatRaster with CRS → warp (project) to template
- `xylim` as anything else → crop to extent
- `rema_8m` requires SpatRaster template (too large for naive crop)

## Datasets supported

Global grids:
- gebco_23, gebco_21, gebco_19, gebco_14, gebco_08
- etopo1, etopo2
- smith_sandwell

Antarctic:
- ibcso, ibcso_is, ibcso_bed (500m)
- cryosat2
- ramp
- rema_8m, rema_100m, rema_200m, rema_1km

Regional:
- kerguelen
- lake_superior
- macrie1100m, macrie2100m
