## CGIAR SRTM 4.1
files <- raadfiles::srtm_files()
## this will only work for nectar machines ... FIXME
vrtname <- file.path(dirname(dirname(files$fullname[1])), "srtm_4.1.vrt")
tfile <- tempfile()
writeLines(files$fullname, tfile)
system(sprintf("gdalbuildvrt -input_file_list %s %s", tfile, vrtname))

