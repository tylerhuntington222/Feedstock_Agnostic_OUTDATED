
# rename_cntroid_files_IO.R

# PURPOSE: rename the weighted county centroid files 
# based on county FIPS codes instead of county names, 
# to avoid issues with non-unique county names. 

###### BASIC FUNCTIONS ######

# a basic function to get the filepath of the current script
csf <- function() {
  # install packages
  #install.packages("rstudioapi")
  # adapted from http://stackoverflow.com/a/32016824/2292993
  cmdArgs = commandArgs(trailingOnly = FALSE)
  needle = "--file="
  match = grep(needle, cmdArgs)
  if (length(match) > 0) {
    # Rscript via command line
    return(normalizePath(sub(needle, "", cmdArgs[match])))
  } else {
    ls_vars = ls(sys.frames()[[1]])
    if ("fileName" %in% ls_vars) {
      # Source'd via RStudio
      return(normalizePath(sys.frames()[[1]]$fileName)) 
    } else {
      if (!is.null(sys.frames()[[1]]$ofile)) {
        # Source'd via R console
        return(normalizePath(sys.frames()[[1]]$ofile))
      } else {
        # RStudio Run Selection
        # http://stackoverflow.com/a/35842176/2292993  
        return(normalizePath(rstudioapi::getActiveDocumentContext()$path))
      }
    }
  }
}

###### SET WORKING DIRECTORY ######
this.dir <- dirname(csf())
setwd(this.dir)
rm(list=ls())

# establish the contents of the directory containing weighted centroid data files
done.files <- list.files("../output/counties/")

for (file in done.files[1:length(done.files)]) {
  
  if (file != "Icon\r") {
    # update contents of directory
    dir.files <- list.files("../output/counties/")
    
    # load sptdf object from county centroid file
    infile <- paste0("../output/counties/", file)
    
    object <- readRDS(infile)
    
    # extract fips code
    fips <- object@data$FIPS[1]
    
    # generate output filepath
    outpath <- paste0("../output/counties/fips_", fips, "_wtd_cntroid.sptdf.RDS")
      
    # check if file with same name already exists in directory
    if (!(outpath %in% dir.files)) {
      saveRDS(object, outpath)
    }
  }
}










