
.possiblepaths <- function() {
  a <- list(default.datadir =  c(
    "/Volumes/files/data",
    "/mnt/aadc/Scientific_Data/Data/gridded_new",
    "//aad.gov.au/files/AADC/Scientific_Data/Data/gridded_new",
    "/mnt/raad", 
    "/rdsi/PRIVATE", 
    "/rdsi/PRIVATE/raad"))
  a
}
.trysetpath <- function() {
  possibles <- .possiblepaths()[["default.datadir"]]
  success <- FALSE
  existing <- getOption("default.datadir")
  if (!is.null(existing)) {
    possibles <- c(existing, possibles)
  } else {
    
  }
  for (i in seq_along(possibles)) {
    fi <- file.info(file.path(possibles[i], "data"))
    if (!is.na(fi$isdir) & fi$isdir) {
      options(default.datadir = possibles[i])
      success <- TRUE
      break;
    }
  }
  ## try RAAD_DIR, which may only be available to R CMD check from ~/.R/check.Renviron
  r <- getOption("repos")
  dd <- getOption("default.datadir")
  if (is.null(dd["default.datadir"])) {
    dd["default.datadir"] <- Sys.getenv("RAAD_DIR");
    options(repos = r, default.datadir = dd); 
  }
  
  
  success
}
.onLoad <- function(libname, pkgname) {
  pathwasset <- .trysetpath()
  base::loadNamespace("raadfiles")
  oldway <- getOption("default.datadir")
  newway <- getOption("raadfiles.default.data.directory")
  print(sprintf("Old way: %s", oldway))
  print(sprintf("New way: %s", newway))
}


.onAttach <- function(libname, pkgname) {
  pathwasset <- .trysetpath()
  if (!pathwasset) {
    packageStartupMessage("\nWarning: could not find data repository at any of\n\n",
                          paste(.possiblepaths()[["default.datadir"]], collapse = "\n"), sep = "\n\n")
    
    packageStartupMessage("Consider setting the option for your system\n")
    packageStartupMessage('For example: options(default.datadir = "', gsub("\\\\", "/", normalizePath("/myrepository/data", mustWork = FALSE)), '")', '\n', sep = "")
    
  }
}