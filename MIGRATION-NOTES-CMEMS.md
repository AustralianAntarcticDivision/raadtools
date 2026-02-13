# raadtools terra-migration: CMEMS SSH/Altimetry/Currents

## Files

```
raadtools/
├── R/
│   ├── read-cmems-ssh.R   # NEW: terra-native readers (all variables)
│   ├── compat-ssh.R       # NEW: readssh() shim
│   └── compat-currents.R  # UPDATED: readcurr() shim now uses cmems functions
└── tests/testthat/
    └── test-read-cmems-ssh.R
```

## New functions

**Scalar readers** (stackable, time-series friendly):
- `read_cmems_adt_daily()` - absolute dynamic topography
- `read_cmems_sla_daily()` - sea level anomaly
- `read_cmems_ugos_daily()` - geostrophic current U component
- `read_cmems_vgos_daily()` - geostrophic current V component
- `read_cmems_ugosa_daily()` - geostrophic current anomaly U
- `read_cmems_vgosa_daily()` - geostrophic current anomaly V
- `read_cmems_err_daily()` - formal mapping error

**Derived readers** (stackable, computed from U/V):
- `read_cmems_current_speed_daily()` - sqrt(u² + v²)
- `read_cmems_current_direction_daily()` - degrees (oceanographic convention)

## Integration

1. Drop files into raadtools/
2. **Important**: This `compat-currents.R` **replaces** the earlier one from the currents migration
3. In `ssh.R`, comment out `readssh()` (keep `sshfiles()` if it exists)
4. In `currents.R`, comment out `readcurr()`/`readcurrents()` (keep helpers)
5. **Delete** or don't use `read-currents.R` from the earlier currents migration - the cmems functions replace it
6. Run tests: `devtools::test(filter = "read-cmems")`

## Naming rationale

Using `_cmems_` prefix because:
- Source is CMEMS Copernicus SEALEVEL_GLO_PHY_L4 product
- Consistent with `_oisst_`, `_nsidc_` naming pattern
- More specific than `_copernicus_` (which is an umbrella)

## Architecture

All variables come from the same NetCDF file. The internal `.read_cmems_ssh_var()` 
does the common work (file lookup, date processing, reading, rotation, cropping).
Each exported function is a thin wrapper specifying which subdataset to read.

Speed and direction are computed from U+V after reading both components.

## Legacy shim mapping

| Old call | Dispatches to |
|----------|---------------|
| `readssh(ssha = FALSE)` | `read_cmems_adt_daily()` |
| `readssh(ssha = TRUE)` | `read_cmems_sla_daily()` |
| `readcurr(magonly = TRUE)` | `read_cmems_current_speed_daily()` |
| `readcurr(dironly = TRUE)` | `read_cmems_current_direction_daily()` |
| `readcurr(uonly = TRUE)` | `read_cmems_ugos_daily()` |
| `readcurr(vonly = TRUE)` | `read_cmems_vgos_daily()` |
| `readcurr()` (default) | Both U and V, single date, 2-layer output |

## Aliases

Existing `read_*_daily()` names are preserved as aliases:
- `read_adt_daily()` → `read_cmems_adt_daily()`
- `read_sla_daily()` → `read_cmems_sla_daily()`
- `read_ugos_daily()` → `read_cmems_ugos_daily()`
- `read_vgos_daily()` → `read_cmems_vgos_daily()`
- `read_ugosa_daily()` → `read_cmems_ugosa_daily()`
- `read_vgosa_daily()` → `read_cmems_vgosa_daily()`
- `read_err_daily()` → `read_cmems_err_daily()`

So existing code using `read_adt_daily()` will continue to work, now terra-native.

## What about the earlier `read_copernicus_current_daily()`?

If you already integrated that, you can either:
1. Keep it as an alias/alternative
2. Rename it to `read_cmems_current_daily()` 
3. Remove it in favor of the individual readers

The new approach (individual scalar readers + derived speed/direction) is cleaner 
because every function returns stackable single-layer output.
