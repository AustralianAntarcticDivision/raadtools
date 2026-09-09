# NSIDC polar stereographic: 3412/3411 vs 3976/3413

Notes parked for Michael to pursue. Written 2026-09-09 during the raadtools
terra-refactor work.

## What was observed

With the CDR reader in place, the two ice products report different CRSs for
what is the same 25 km grid:

    readice()                     ->  EPSG:3412  (south)   NSIDC CDR, G02202 V6
    readice(hemisphere = "north") ->  EPSG:3411  (north)   NSIDC CDR, G02202 V6
    readice(product = "nsidc")    ->  EPSG:3976  (south)   NSIDC-0051 v2

`read_nsidc_cdr_daily()` takes the CRS from the file rather than asserting one,
so 3412/3411 is what the CDR files are declaring. `read_nsidc_ice_daily()`
asserts 3976/3413 explicitly in code.

## Is it a GDAL issue?

**No. Answered 2026-09-09 by reading the file's `crs` variable.** The reasoning
below stands, and the check at the end of this section came back unambiguous:
the file declares Hughes 1980 three separate ways, and names EPSG:3412
outright.

    semi_major_axis      6378273
    inverse_flattening   298.2794
    proj4text            +proj=stere +lat_0=-90 +lat_ts=-70 +lon_0=0 +k=1
                         +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m
    crs_wkt              SPHEROID["Hughes 1980",6378273,...,EPSG 7058]
                         ... AUTHORITY["EPSG","3412"]
    spatial_ref          (same WKT)
    srid                 urn:ogc:def:crs:EPSG::3412

GDAL had nothing to infer. It read `crs_wkt`/`spatial_ref`, which name the
code. `OSRAutoIdentifyEPSG` is not in the picture, so the GDAL-version concern
below does not apply either.

That leaves the interesting half of the question on the other product. Both
grids report identical geometry for the south (332 x 316, extent -3950000,
3950000, -3950000, 4350000), so they are the same cells; NSIDC-0051 v2 simply
labels those cells WGS 84 while the CDR labels them Hughes. NSIDC has published
guidance treating 3413/3976 as an acceptable relabelling of the original
3411/3412 grids, so this is most likely a deliberate choice on their side
rather than either file being wrong. If so, the offsets measured below are the
cost of the relabelling, not a disagreement about where the data sits.

### Original reasoning, kept for the record


The four codes differ in exactly one thing, the ellipsoid. Everything else
(latitude of standard parallel -70, longitude of origin 0, false easting and
northing 0) is identical:

    EPSG:3412  ELLIPSOID["Hughes 1980", 6378273, 298.279411123064]
    EPSG:3976  ELLIPSOID["WGS 84",      6378137, 298.257223563]
    EPSG:3411  ELLIPSOID["Hughes 1980", 6378273, 298.279411123064]
    EPSG:3413  ELLIPSOID["WGS 84",      6378137, 298.257223563]

So the ellipsoid is the only discriminator. GDAL's netCDF driver builds the SRS
from `crs_wkt` / `spatial_ref` when the grid mapping variable carries one, and
otherwise from the CF grid mapping parameters (`semi_major_axis` plus
`inverse_flattening` or `semi_minor_axis`). It cannot invent the Hughes
ellipsoid out of a WGS 84 declaration. If terra reports 3412, the most likely
explanation is that the file says Hughes, either by naming the code outright in
a `srid` / `spatial_ref` attribute or by giving a = 6378273.

That would also be consistent with the products themselves. The NSIDC polar
stereographic grids were originally defined on Hughes 1980, and G02202 has
historically documented 3411/3412. NSIDC-0051 v2 was regenerated more recently
and appears to have been tagged with the WGS 84 codes, which NSIDC now
recommends for new products. If so, the two products genuinely disagree at
source and raadtools is faithfully reporting that disagreement.

Two ways it could still be GDAL, worth ruling out rather than assuming:

1. The file declares no ellipsoid at all and GDAL fills one in. This would push
   toward WGS 84, not Hughes, so it does not explain a 3412 result. It is the
   less likely direction.
2. `OSRAutoIdentifyEPSG` preferring the deprecated code. Older GDAL has been
   known to attach 3411/3412 during identification. The numbers below came from
   GDAL 3.8.4 / PROJ 9.4.0 via terra 1.7.65; if the reading machine is on a
   different GDAL it is worth comparing.

### The check that settled it

One command, since raadtools already depends on ncdf4:

```r
f  <- raadfiles::nsidc_cdr_south_daily_files()$fullname[1]
nc <- ncdf4::nc_open(f)
ncdf4::ncatt_get(nc, "crs")
ncdf4::nc_close(nc)
```

It showed `semi_major_axis = 6378273` and a `srid` of
`urn:ogc:def:crs:EPSG::3412`. The file is explicit and GDAL is reporting it
correctly.

## How much does it actually matter

Two different quantities get conflated here, so both are measured.

**Cell geometry.** The ground length of a 25 km cell diagonal, read under each
CRS. This is the quantity the comment at the top of `R/ice.R` refers to, and it
checks out at well under a metre:

| grid y (m) | diagonal under 3412 | diagonal under 3976 | difference |
|-----------:|--------------------:|--------------------:|-----------:|
| -1 000 000 |          36 224.05 m |          36 224.81 m |     0.75 m |
| -2 000 000 |          35 541.92 m |          35 542.63 m |     0.71 m |
| -3 000 000 |          34 456.18 m |          34 456.83 m |     0.65 m |

**Position.** Where a given projected coordinate lands on the ground, which is
the quantity that bites when you overlay a CDR grid and an 0051 grid after
reprojecting both to longlat. This one is larger:

| grid x, y (m)         | lat under 3412 | lat under 3976 | ground offset |
|----------------------:|---------------:|---------------:|--------------:|
| 0, 0 (pole)           |      -90.00000 |      -90.00000 |           0 m |
| 0, -1 000 000         |      -80.78801 |      -80.78781 |        21.6 m |
| 0, -2 000 000         |      -71.68899 |      -71.68861 |        42.4 m |
| 0, -3 000 000         |      -62.80825 |      -62.80770 |        61.7 m |
| -3 000 000, -2 500 000|      -55.03476 |      -55.03406 |        77.5 m |
| 3 950 000, -3 950 000 |      -41.44695 |      -41.44604 |       101.7 m |

Zero at the pole, growing outward, about 50 to 70 m through the seasonal ice
edge latitudes and reaching roughly 100 m at the grid corner. That is a small
fraction of a 25 km cell, so nothing moves between cells, but it is a
systematic offset rather than noise, and it is two orders of magnitude larger
than the cell-diagonal figure.

## What raadtools does today

`read_nsidc_cdr_daily()` trusts the file:

```r
if (!nzchar(terra::crs(r))) {
  terra::crs(r) <- switch(hemisphere, south = "EPSG:3976", north = "EPSG:3413")
}
```

It only fills in a CRS when the file declares none. So today the CDR comes back
as 3412/3411 and NSIDC-0051 as 3976/3413.

If the decision goes the other way, normalising is a two-line change: drop the
`nzchar` guard so the `switch` always runs, and say in a comment that the
override is deliberate. The argument for normalising is one CRS across all ice
products plus NSIDC's own recommendation; the argument against is that
raadtools would then be contradicting what the file says, which is unpleasant
to discover later.

## Environment the numbers came from

terra 1.7.65, GDAL 3.8.4, PROJ 9.4.0. Distances computed with
`terra::distance(..., lonlat = TRUE)`.

## Unrelated, found while comparing the two grids

The two VRT templates in `R/ice.R` do not agree with each other about the
north grid.

| grid                     |     xmin |    xmax |     ymin |    ymax |
|--------------------------|---------:|--------:|---------:|--------:|
| `.south_ndsic_vrt`       | -3950000 | 3950000 | -3950000 | 4350000 |
| south, as the files report | -3950000 | 3950000 | -3950000 | 4350000 |
| `.north_nsidc_vrt`       | -3837500 | 3762500 | -5362500 | 5837500 |
| north, as the CDR reports | -3850000 | 3750000 | -5350000 | 5850000 |

South agrees exactly. North is out by 12500 m in x and -12500 m in y, which is
half a 25 km cell in each axis - the signature of a geotransform origin set to
the first cell's centre rather than its outer corner. The reported north extent
is also the canonical NSIDC psn25 one, so the template looks like the side
that is wrong.

Worth noting separately that both templates are `VRTRawRasterBand` with
`ImageOffset` 300, which describes the old raw binary NSIDC files. The listers
now return NetCDF (`readice(product = "nsidc")` reports `varname: F17_ICECON`),
so applying either template to a current file would read header bytes as
pixels. Nothing inside raadtools consumes `icefiles()$vrt_dsn` any more, but it
is still handed to users and documented as usable.
