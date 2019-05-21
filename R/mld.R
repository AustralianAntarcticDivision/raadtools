

.loadMLD <- function() {
 f <-  raadfiles::get_raad_filenames() %>% dplyr::filter(stringr::str_detect(file, "sallee_mld2013.Rdata"))
 p <- file.path(f$root, f$file)[1]
  if (!file.exists(p)) return(NULL)
  mld <- NULL
  load(p)
  mld
}



#' Mixed Layer Depth
#' 
#' Read the \enc{Sall?e}{Sallee} Mixed layer depth climatology. 
#' 
#' Information from JB Sallee (2013-06-06) is below, and an example to
#' read and display it in R.
#' A recently updated dataset using the most recent Southern Ocean observation,
#' with updated gridding analysis so it should be better and have appropriate
#' land mask:  ftp://ftp.nerc-bas.ac.uk/jbsall/MLfitted_SO.mat
#
#' This is a compilation of ocean *observations*: a compilation of 225389
#' profiles from the Argo program and 106682 profiles from all kind of different
#' ships and PIs. This is *not* model evaluation.
#
#' This is likely the best estimate of monthly MLD , because it uses the largest
#' number of observations. There is no observational products with time
#' variability because the Southern Ocean only started to be observed (at basin
#' scale) in this last decade. No observation product can give a temporal
#' evolution at basin scale.
#
#' Model estimate could be use, but model are known to perform particularly badly
#' in the surface layer so the mixed-layer is always problematic from model.
#' Another possibility could be to use some reanalysis product like that
#' Met-Office product
#' (http://www.metoffice.gov.uk/hadobs/en3/data/EN3_v2a/download_EN3_v2a.html),
#' but this is an objective analysis of available observation, so in the southern
#' Ocean before 2002 (when Argo started), there is not much, so the results will
#' be strongly dominated by the climatology (World ocean Atlas).
#
#' Unfortunately there is no good answer for time variability of MLD.
#' @param date date to extract, can only be a climatology month
#' @param xylim extent specification
#' @param returnfiles return the file details only
#' @param ... ignored
#' @return RasterBrick
#' @importFrom raster subset
#' @encoding ISO-8859-2
#' @export
readmld <- function(date, xylim = NULL, returnfiles = FALSE, ...) {
  if (missing(date)) date <- seq(as.Date("2013-01-01"), by = "1 month", length = 12L)
  
  date <- timedateFrom(date)
  
  date <- format(date, "%b")
  x <- .loadMLD()
  if (!is.null(xylim)) x  <- crop(x, extent(xylim))
  f <-  raadfiles::get_raad_filenames() %>% dplyr::filter(stringr::str_detect(file, "sallee_mld2013.Rdata"))
  
  files <- tibble::tibble(fullname = file.path(f$root, f$file), date = timedateFrom(seq(as.Date("2013-01-01"), by = "1 month", length = 12L)))
  if (returnfiles) return(files)
  warning("MLD data is only a climatology, returning matching month only")
  subset(x, date)
}
