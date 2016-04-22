amsr2files <- function(date, product = c("tif", "hdf", "nic", "visual"), 
                       region = c("Amundsen", "AntarcticPeninsula", "Casey-Dumont", "DavisSea",
                                                                             "McMurdo", "Neumayer", "NeumayerEast", "Polarstern", "RossSea",
                                                                             "_RossSea", "ScotiaSea", "WeddellSea", "WestDavisSea"))
{
    #datadir <- getOption("default.datadir")
    ftx <- .allfilelist()
    product <- match.arg(product)
    region <- match.arg(region)
    format <- switch(product, 
                     tif = "tif$", 
                     hdf = "hdf$",
                     nic = "nic.png$", 
                     visual = "visual.png")
    #www.iup.uni-bremen.de+8084/amsr2data/asi_daygrid_swath/s3125/2015/nov/DavisSea/asi-AMSR2-s3125-20151130.tif
    
    cfiles1 <- grep("www.iup.uni-bremen.de", ftx, value = TRUE)
    cfiles2 <- grep(region, cfiles1, value = TRUE)
    cfiles <- grep(format, cfiles2, value = TRUE) 
   
    if (length(cfiles) < 1) stop("no files found for ", paste(product, region,  collapse = ", "))
    dates <- as.POSIXct(strptime(basename(cfiles), "asi-AMSR2-s3125-%Y%m%d", tz = "GMT"))  ## asi-AMSR2-s3125-20151121_
    cfs <- data.frame( fullname = cfiles, 
                      date = dates, stringsAsFactors = FALSE)[order(dates), ]
    cfs
    
}
