# https://ecat.ga.gov.au/geonetwork/srv/eng/search#!aac46307-fce8-449d-e044-00144fdd4fa6
## https://s3-ap-southeast-2.amazonaws.com/elvis.ga.gov.au/elevation/1sec-srtm/a05f7893-0050-7506-e044-00144fdd4fa6.zip

##raadfiles:::get_raw_raad_filenames() %>% 
##  dplyr::filter(stringr::str_detect(file, 
#                      "a05f7893-0050-7506-e044-00144fdd4fa6"))


## for subsetting, this seems to run in reasonable time
## the internal tiling is 512 * 4 so as long as
## we region.dim something aligned to that (and 0,0)
## a <- readGDAL(filename(r), offset = c(0, 0), region.dim = c(239, 288) * 512, output.dim = c(1024, 1024))
read_ga_srtm <- function() {
   f <- topofile("ga_srtm")
    if (!file.exists(f)) stop(sprintf("no such file %s", f))
  raster::raster(f)
}
