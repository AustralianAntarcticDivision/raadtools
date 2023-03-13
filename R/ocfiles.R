##' Load metadata and location of files of ocean colour data products.
##'
##' This function loads the latest cache of stored files for
##' NASA ocean colour products.
##' @param time.resolution daily or monthly files?
##' @param product choice of ocean colour sensor
##' @param varname which variable (or set of variables)
##' @param type which level of data
##' @param bz2.rm ignore files that are compressed
##' @param ext which extension, e.g. "nc" or "main"
##' @param ... reserved for future use, currently ignored
##' @export
##' @return data.frame of \code{file} and \code{date}
ocfiles <- function(time.resolution = c("daily", "weekly", "monthly", "weekly32"),
                    product = c("MODISA", "SeaWiFS", "VIIRS"), 
                    varname = c("RRS", "CHL", "POC", "KD490", "NPP_PAR", "SNPP_CHL", "SNPP_RRS"), 
                    type = c("L3b", "L3m"),
                    bz2.rm = TRUE, 
                    ext = c("nc", "main"), 
                    ...) {

  #ftx <- .allfilelist()
  ftx <- dplyr::transmute(raadtools:::allfiles() %>% dplyr::filter(stringr::str_detect(file, "oceandata.sci.gsfc.nasa.gov")), fullname = fs::path(root, file))$fullname
  time.resolution <- match.arg(time.resolution)
  product <- match.arg(product)
  ext <- match.arg(ext)
  varname <- varname[1L]
  type <- match.arg(type)
  
  time <- switch(time.resolution,
                 daily = "DAY",
                 weekly = "8D",
                 monthly = "MO",
                 weekly32 = "R32")
  ## NOTE file naming changed from October 2019 (date varied from data set to data set), see https://oceancolor.gsfc.nasa.gov/docs/filenaming-convention/
  ## old name e.g. A2020366.L3b_DAY_RRS.nc becomes AQUA_MODIS.20201231.L3b.DAY.RRS.nc
  ## - platform from abbreviation ("A") to full name ("AQUA_MODIS")
  ## - YYYYDOY to YYYYMMDD
  ## - separator between file name components is "." not "_"

  ## first look for files using the new naming convention
  prod <- switch(product, MODISA = "AQUA_MODIS", SeaWiFS = "SEASTAR_SEAWIFS_GAC", MODIST = "TERRA_MODIS", CZCS = "NIMBUS7_CZCS", VIIRS = "SNPP_VIIRS")
  ## note that there is also JPSS1_VIIRS
  mtag <- paste0(prod, ".*", paste(type, time, varname, sep = "\\."), ".*\\.", ext) ## separator is "." with new naming
  cfiles3 <- grep(mtag, ftx, value = TRUE)
  if (length(cfiles) > 0) {
      ## I think this is irrelevant with the new files (no bz2s?) but leave it anyway
      cfiles <- if (bz2.rm)  grep(paste0(ext, "$"), cfiles3, value = TRUE) else cfiles3
      if (length(cfiles) < 1) stop("no files found for ", paste(product, varname, type, time.resolution, collapse = ", "))
      dates <- sub("\\..+", "", sub("^[^\\.]+\\.", "", basename(cfiles))) ## discard before leading separator, and after second (keep date component)
      dates <- as.POSIXct(strptime(dates, "%Y%m%d", tz = "GMT"))
  } else {
    ## did not find any new files, try old file naming
      prod <- switch(product, MODISA = "^A", SeaWiFS = "^S", VIIRS = "^V")
      ## don't forget those ST93c files!
      ## see here: http://oceancolor.gsfc.nasa.gov/DOCS/FormatChange.html
      ##   Note: ST92 is the test set designation for the SeaWiFS test run, similarly
      ##   AT108 and AT109 are the MODIS-Aqua test set designators.  These designations
      ##   will NOT be part of the reprocessing filenames.  
      mtag <- sprintf(paste0("%s.*\\.", ext), paste(type, time, varname, sep = "_"))
      ##print(mtag)
      ##cfiles1 <- sapply(product, function(x) file.path("oceandata.sci.gsfc.nasa.gov", x)
      cfiles2 <- grep(mtag, ftx, value = TRUE)
      cfiles3 <- cfiles2[grep(prod, basename(cfiles2))]
      cfiles <- if (bz2.rm)  grep(paste0(ext, "$"), cfiles3, value = TRUE) else cfiles3
      if (length(cfiles) < 1) stop("no files found for ", paste(product, varname, type, time.resolution, collapse = ", "))
      tokens <- .filetok(basename(cfiles))
      dates <- as.POSIXct(strptime(paste0(tokens$year, tokens$jday), "%Y%j", tz = "GMT"))
  }
  tibble::tibble(fullname = cfiles, date = dates)[order(dates), ]
}


## This function is from roc
.filetok <- function(x) {
  sensortok <- substr(x, 1, 1)
  yeartok <- substr(x, 2, 5)
  jdaytok <- substr(x, 6, 8)
  ## Note: Aquarius is *versioned* so we need some extra handling here
  ## or we'll just smash them all together (might be ok since the files have the version name)
  
  list(sensor = sensortok, year = yeartok, jday = jdaytok)
}
