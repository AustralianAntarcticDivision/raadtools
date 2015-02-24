# R tools for spatial data at the Australian Antarctic Division (AAD)

Tools for reading, plotting and manipulating spatial data used at the Australian Antarctic Division (AAD).

Michael D. Sumner michael.sumner@aad.gov.au



DEV

review the *files() functions

need to answer what is the "RAAD_DIR" value on rdsi and AAD?

DONE (these need /data added to default.datadir)
currentsfiles
icefiles
ocfiles
rapid_responsefiles
sshfiles
sstfiles


TODO
chlafiles
fasticefiles
prodfiles
topofile
windfiles


topofile("gebco_08")
[1] "/rdsi/PRIVATE/bathymetry/gebco_08/gebco_08.tif"
topofile("ibcso", polar = TRUE)
[1] "/rdsi/PRIVATE/bathymetry/ibcso/ps71/ibcso_v1_is.tif"
topofile("ibcso")
[1] "/rdsi/PRIVATE/bathymetry/ibcso/latlon/ibcso_v1_is.tif"
topofile("etopo1")
[1] "/rdsi/PRIVATE/bathymetry/etopo1/ETOPO1_Ice_g_gdal.grd"
topofile("etopo2", polar = F)
[1] "/rdsi/PRIVATE/bathymetry/etopo2/ETOPO2v2c_f4.nc"
topofile("kerguelen", polar = F)
[1] "/rdsi/PRIVATE/bathymetry/kerguelen/kerg_dem_100m.grd"
topofile("george_v_terre_adelie")
 topofile("george_v_terre_adelie")
[1] "/rdsi/PRIVATE/bathymetry/george_v_terre_adelie/gvdem100m_v3.nc"
topofile("smith_sandwell")
[1] "/rdsi/PRIVATE/bathymetry/smith_sandwell/topo_15.1_Atlantic.vrt"


