# raadtools terra-migration: Wind

## Files

```
raadtools/
├── R/
│   ├── read-wind.R    # NEW: terra-native reader
│   └── compat-wind.R  # NEW: backward-compatible shim
└── tests/testthat/
    └── test-read-wind.R
```

## New function

`read_ncep2_wind_6hourly()` - returns SpatRaster

## Integration

1. Drop files into raadtools/
2. In `wind.R`, comment out `readwind()` (keep `windfiles()` - it's used by the new reader)
3. Run tests: `devtools::test(filter = "read-wind")`

## Notes

- Uses existing `windfiles()` for file catalog (does band expansion)
- Band-indexed: multiple 6-hourly timesteps per file
- U and V in separate files (`ufullname`, `vfullname`)
- Removed the extent alignment tweak from legacy - not needed with terra
- Same component flags as currents: `magonly`, `dironly`, `uonly`, `vonly`
