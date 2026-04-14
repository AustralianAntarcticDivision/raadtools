# raadtools terra-migration: MLD Climatology

## Files

```
raadtools/
├── R/
│   └── read-mld.R    # NEW: terra-native reader + shim
└── tests/testthat/
    └── test-read-mld.R
```

## New function

```r
read_mld_climatology(date, xylim, returnfiles)
```

Returns SpatRaster with 1-12 layers (one per month).

## Legacy shim

```r
readmld()  # returns RasterBrick, emits climatology warning
```

## Integration

1. Drop files into raadtools/
2. In existing MLD file (wherever `.loadMLD()` and `readmld()` live):
   - **DELETE**: `.loadMLD()` - no longer needed
   - **COMMENT OUT**: `readmld()` - replaced by shim
3. Run tests: `devtools::test(filter = "read-mld")`

## Data source

Sallée et al. (2013) Southern Ocean MLD climatology.

**Location:** `/rdsi/PRIVATE/raad/data_local/mld/JBfitted/sallee_mld2013.Rdata`

**Format:** Serialized RasterBrick (12 layers, one per month)

**Coverage:** Southern Ocean, south of 30°S

**Note:** The `.Rdata` format is fragile - depends on raster package version.
Consider converting to NetCDF for long-term stability:

```r
mld <- read_mld_climatology()
terra::writeCDF(mld, "sallee_mld2013.nc", varname = "mld",
                longname = "Mixed Layer Depth", unit = "m")
```

Then update the reader to use the NetCDF directly.

## Behavior notes

- Default (no date): returns all 12 months
- With date: extracts month(s) from date, returns matching layers
- Dates are only used to determine month - actual year/day ignored
- The warning about "climatology only" is preserved in the shim
