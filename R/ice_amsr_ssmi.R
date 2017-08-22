#switch(product,
#               nsidc = nsidcdims,
#               amsr = c(1328, 1264),
#               ssmi = c(632L, 664L))
#witch(product,
#     nsidc = c(25000, 25000),
#     amsr = c(6250, 6250),
#     ssmi = c(12500, 12500))
.amsr625files <- function(allfiles, ext) {
  
  ## 2002:2011
  #  f1 <- "ftp-projects.zmaw.de/seaice/AMSR-E_ASI_IceConc/no_landmask/hdf/s6250/"
  ## modified from zmaw.de 2017-06-27 https://github.com/AustralianAntarcticDivision/raadtools/issues/52
  f1 <- "ftp-projects.cen.uni-hamburg.de/seaice/AMSR-E_ASI_IceConc/no_landmask/hdf/s6250/"
  ## 2012:2015+
  f2 <- "www.iup.uni-bremen.de\\+8084/amsr2data/asi_daygrid_swath/s6250"
  datadir <- getOption("default.datadir")
  f3 <- allfiles[c(grep(f1, allfiles), grep(f2, allfiles))]
  f4 <- grep(sprintf("%s", ext),  f3, value = TRUE)
  ## que?
  f5 <- f4[!grepl("s12500", f4)]
  files <- data.frame(fullname = file.path(datadir, f5), 
                      #file = gsub(paste0(datadir, "/data/"), "", f5), 
                      file = f5,
                      date = as.POSIXct(strptime(sapply(strsplit(basename(f5), "-"), "[", 4), "%Y%m%d"), tz = "UTC"), 
                      stringsAsFactors = FALSE)
  
  
  files <- files[order(files$date), ]  
  ## keep the last of the duplicates
  #files[!rev(duplicated(rev(files$date))), ]
  files[!duplicated(files$date), ]
  
}
# system.time(af <- .amsr625files(.allfilelist(), "hdf"))
# user  system elapsed 
# 7.296   0.080   7.379 
# system.time(af2 <- .amsr625_dbfiles())
# user  system elapsed 
# 0.476   0.240   1.713

#' @importFrom dplyr %>% filter collect mutate arrange distinct
.amsr625_dbfiles <- function() {
  datadir <- getOption("default.datadir")
  db <- dplyr::src_sqlite(file.path(datadir, "admin", "filelist", "allfiles.sqlite"))
  tab <- dplyr::tbl(db, "file_list") %>% ## split the string search into two simpler parts makes it faster
    dplyr::filter(fullname %like% "%hdf") %>% 
    filter(fullname %like% "%s6250%") %>% dplyr::collect() %>% 
    filter(!grepl("LongitudeLatitude", fullname)) %>% 
    filter(grepl("ftp-projects.cen.uni-hamburg.de", fullname) | 
             grepl("www.iup.uni-bremen.de", fullname)) %>% 
    mutate(file = fullname, fullname = file.path(datadir, file)) 
  ## modified from zmaw.de 2017-06-27 https://github.com/AustralianAntarcticDivision/raadtools/issues/52
  #f1 <- "ftp-projects.cen.uni-hamburg.de/seaice/AMSR-E_ASI_IceConc/no_landmask/hdf/s6250/"
  
  # f1 <- "ftp-projects.zmaw.de/seaice/AMSR-E_ASI_IceConc/no_landmask/hdf/s6250/"
  # f2 <- "www.iup.uni-bremen.de\\+8084/amsr2data/asi_daygrid_swath/s6250"
  # tab <- tab[c(grep(f1, tab$fullname), grep(f2, tab$fullname)), ]
  # 
  tab$date <- as.POSIXct(strptime(unlist(lapply(strsplit(basename(tab$fullname), "-"), "[", 4)), "%Y%m%d"), tz = "UTC")
  #tab <- tab[order(tab$date), ]
  #tab$file <- gsub(paste0(getOption("default.datadir"), "/"), "", tab$fullname)
  #tab[!duplicated(tab$date), ]
  tab %>% arrange(date) %>% distinct(date, .keep_all = TRUE)
}



.readAMSR <- function(fname) {
  x <- flip(raster(fname), direction = "y")
  extent(x) <- extent(rtemplate)
  ## earlier files were 0,1
  if (grepl("asi.nl.s6250", basename(fname))) x <- x * 100
  x
}
.readSSMI <- function(fname) {
  x <- raster(fname, varname = "concentration")
  x <- flip(x, "y")
  if (!setNA) {
    x[is.na(x)] <- -127
  } else {
    x[x > 100 | x < 1] <- NA
  }
  extent(x) <- extent(rtemplate)
  x
}

# old_ssmi_amsr_logic <- function() {
#   ftx <- .allfilelist(rda = TRUE, fullname = FALSE)
#   ## just shortcut here for AMSR (need to review code below)
#   if (product == "amsr") return(.amsr625files(ftx, "hdf"))
#   ppat <- switch(product, 
#                  nsidc = "sidads.colorado.edu",
#                  ## need to use the + for some reason
#                  amsr = "www.iup.uni-bremen.de\\+8084")
#   strpat <- switch(product, 
#                    nsidc = "nt_", 
#                    amsr = "AMSR2")
#   
#   epat <- switch(product, 
#                  nsidc = ".bin$", 
#                  
#                  amsr = ".hdf$")  ## this is ignored
#   if (product == "amsr" & hemisphere != "south") stop("no north hemisphere for amsr")
#   cfiles0 <- grep(ppat, ftx, value = TRUE)
#   cfiles1 <- if(product == "nsidc") {
#     c(grep(time.resolution, cfiles0, value = TRUE), grep("_nrt_", cfiles0, value = TRUE))
#   }  else {
#     cfiles0
#   }
#   cfiles2 <- if(product == "nsidc") grep(hemisphere, cfiles1, value = TRUE) else cfiles1
#   
#   cfiles3 <- grep(strpat, cfiles2, value = TRUE)
#   cfiles <- grep(epat, cfiles3, value = TRUE)
#   
#   if (length(cfiles) < 1) stop("no files found")
#   
#   doffs <- if(product == "nsidc") 3 else 1
#   sep <- if(product == "nsidc") "_" else "-"
#   datepart <- sapply(strsplit(basename(cfiles), sep), function(x) x[length(x) - doffs])
#   
#   
#   datepat <-  "%Y%m%d"
#   if (time.resolution == "monthly") datepart <- sprintf("%s01", datepart)
#   dates <- timedateFrom(as.POSIXct(strptime(datepart, datepat, tz = "GMT")))
#   
#   nas <- is.na(dates)
#   dates <- dates[!nas]
#   cfiles <- cfiles[!nas]
#   
#   cfs <- data.frame(file = cfiles,
#                     #file = gsub(paste(datadir, "/", sep = ""), "", cfiles), 
#                     date = dates,
#                     fullname = file.path(datadir, cfiles), 
#                     stringsAsFactors = FALSE)[order(dates), ]
#   
#   cfs <- cfs[!duplicated(cfs$date), ]
#   
#   cfs
# }