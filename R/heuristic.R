#' Figure out what's best here. 
#'
#' Interestingly, putting in this feedback provides huge clarity over the bottleneck which 
#' is the number of files accessed. Reading from the file takes very small amount of time, 
#' so if we can be smart about whether the file-backed version is needed we make a huge saving. 
#' 
#' Also we will learn a lot about usage patterns if we start logging this ...
#' 
#' And we can determine the right cut-off size value by seeing what is reasonable on different machines. 
#' 
#' We need to be able to detect when raster decides to go for file-backing, keeping track of that will help
#' @param date 
#' @param time.resolution 
#' @param xylim 
#' @param lon180 
#' @param varname 
#' @param setNA 
#' @param latest 
#' @param returnfiles 
#' @param readall 
#' @param ... 
#' @param inputfiles 
#'
#' @return a raster layer
#' @noRd
#'
#' @examples
#' ## testing this? Use devtools tricks to get around the namespace while developing: 
#' ## devtools::load_all(".")
#' 
#' f <- sstfiles()
#' ## too easy
#' x <- read_sst_heuristic(f$date[1:100], inputfiles = f, xylim = extent(100, 110, -50, -40))
#'x <-  read_sst_heuristic(f$date[1:1000], inputfiles = f, xylim = extent(100, 110, -50, -40))
#' ## no sweat
#' read_sst_heuristic(f$date[1:1000], inputfiles = f, xylim = extent(10, 110, -50, -40))
#' 
#' ## coffee
#' x <- read_sst_heuristic(f$date[1:4000], inputfiles = f, xylim = extent(10, 110, -50, -40))
#' 
#' ## time to think
#' x <- read_sst_heuristic(f$date[1:1000], inputfiles = f, xylim = extent(10, 110, -50, 40))
#' 
#' read_sst_heuristic(f$date[1:4000], inputfiles = f, xylim = extent(-180, 110, -50, 40))
#' 
read_sst_heuristic <- function (date, time.resolution = c("daily", "monthly"), xylim = NULL, 
                                lon180 = TRUE, varname = c("sst", "anom", "err", "ice"), 
                                setNA = TRUE, latest = TRUE, returnfiles = FALSE, readall = FALSE, 
                                ..., inputfiles = NULL) {
  if (missing(inputfiles)) {
    files <- sstfiles(time.resolution = time.resolution)
  } else {
    files <- inputfiles
  }
  relative_vibe <- c(` easily` = 2e6, ` no sweat` = 10e6, ` sitting down` = 20e6, `in the time it takes to get a coffee ... Nescafe from the kitchenette!` = 50e6, 
                     `in the time it takes to get a coffee ... (real coffee, if you need pumpkin spice soy decaf latte I'll slow down a little)` = 80e6,
                     `, but give me some room to think here` = 2e9 -1)
  time.resolution <- match.arg(time.resolution)
  if (missing(date)) {
    if (latest) date <- max(files$date)  else date <- min(files$date)
  }
  files <- .processFiles(date, files, time.resolution)

  varname <- match.arg(varname)
  dummy <- crop_if_needed(readsst(varname = varname), xylim)
  nlayer_ <- length(date)
  dims <- c(nrow(dummy), ncol(dummy), nlayer_)
  ## really big
  really_big <- 3e8
  message(sprintf("limit number of cells: %s", format(really_big, sci = TRUE)))
  message(sprintf("dimension requested: [%s] = %i = %iMb", paste(dims, collapse = ","), as.integer(prod(dims)), as.integer(prod(dims) * 8 / 1e6)))
  message(sprintf("%f %% of the file-back limit", (prod(dims)/really_big) * 100))
 # glob_count_readsst_heuristic <- 0L

  if (prod(dims) > really_big) {

    message("wow, dude that's really a lot of data ... building a stack")
    r <- stack(files$fullname, quick = TRUE)
    message("we really need to pull out the big guns on this one ... preparing to WRITE TO FILE ... and return a brick! (woah)")
    filename <- list(...)$filename
    if (is.null(filename)) {
      filename <- sprintf("%s.grd", tempfile())
      
    }
    message(sprintf("writing to file: %s", filename))
    if (!is.null(xylim)) {
      r <- brick(crop(r, xylim, snap = "out"), filename = filename)  
    } else {
      r <- brick(r, filename = filename)  
    }
    
    print(dims)
  } else {
    pb <- progress::progress_bar$new(
      format = "  extracting [:bar] :percent in :elapsed",
      total = nlayer_, clear = FALSE, width= 60)
    pb$tick(0)
    message(sprintf("hey, nice request I think I can handle that%s", names(relative_vibe)[findInterval(prod(dims), relative_vibe) + 1]))
    read_fun <- function(xfile, ext) {
      #glob_count_readsst_heuristic <<- glob_count_readsst_heuristic + 1L
      pb$tick()
       crop_if_needed(raster(xfile, varname = varname), ext)
    }
    r <- brick(stack(lapply(files$fullname, read_fun, ext = xylim)), ...)
  }
  r
}

crop_if_needed <- function(x, ext = NULL) {
  if (!is.null(ext)) {
    crop(x, ext, snap = "out") 
  } else {
    x
  }
}