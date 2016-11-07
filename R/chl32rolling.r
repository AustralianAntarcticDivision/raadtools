#' Read R32 VIIRS files
#'
#' @param date 
#' @param time.resolution 
#' @param product 
#' @param platform 
#' @param xylim 
#' @param returnfiles 
#' @param latest 
#' @param verbose 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
readchl32 <- function(date, time.resolution = c("weekly32"),
                     product = c("oceancolor"),
                     platform = c("VIIRS"), 
                     xylim = NULL,
                     
                     ##lon180 = TRUE,
                     returnfiles = FALSE,
                     latest = FALSE,
                     verbose = TRUE,
                     ...) {
  
  files <- ocfiles(time.resolution = "weekly32", product = platform, varname = "SNPP_CHL", type = "L3m")
  if (returnfiles) return(files)
  
  if (missing(date)) date <- min(files$date)
  if (latest) date <- max(files$date)
  date <- timedateFrom(date)
  time.resolution <- c(weekly32 = "weekly")[time.resolution]
  ## from this point one, we don't care about the input "date" - this is our index into all files and that's what we use
  ##findex <- .processDates(date, files$date, time.resolution)
  files <- .processFiles(date, files, time.resolution)
  
 # rtemplate <-  raster(files$fullname[1L], varname = "chlor_a")
  ##if (lon180) rtemplate <- .rotate(rtemplate)
  
  ## process xylim
  cropit <- FALSE
  if (!is.null(xylim)) {
    cropit <- TRUE
    cropext <- extent(xylim)
    ##rtemplate <- crop(rtemplate, cropext)
  }
  
  nfiles <- nrow(files)
  r <- vector("list", nfiles)
  
  for (ifile in seq_len(nfiles)) {
    r0 <- raster(files$fullname[ifile],  varname = "chlor_a")
    ##if (lon180) r0 <- .rotate(r0)
    if(cropit) r0 <- crop(r0, cropext)
    ## r0[r0 < -2] <- NA
    r[[ifile]] <- r0
    ##if (verbose & ifile %% 10L == 0L) .progressreport(ifile, nfiles)
  }
  
  if (nfiles > 1)
    r <- brick(stack(r), ...)
  else r <- r[[1L]]
  names(r) <- basename(files$fullname)
  if (is.na(projection(r))) projection(r) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0" 
  
  r <- setZ(r, files$date)
  return(r)
}