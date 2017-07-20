
#-----------------------------------------------------------------------------#
  # CollateBiosheds_fun.R
  # Type: R function script
  
  # AUTHOR:
  # Tyler Huntington, 2017
  # JBEI Sustainability Research Group
  # Project: Feedstock Agnostic Biorefinery Study
  
  # PURPOSE:
  # A script to collate the feedsheds calculated for each biorefinery and stored
# in separate binary data files.


# SIDE-EFFECTS:
# outputs a binary data file (.RDS) to the 'output' directory which contains an
# object of class SpatialPointsData frame containing weighted centroids of 
# all US counties.

#-----------------------------------------------------------------------------#

# ###### BASIC FUNCTIONS ######
# 
# # a basic function to get the filepath of the current script
# csf <- function() {
#   # adapted from http://stackoverflow.com/a/32016824/2292993
#   cmdArgs = commandArgs(trailingOnly = FALSE)
#   needle = "--file="
#   match = grep(needle, cmdArgs)
#   if (length(match) > 0) {
#     # Rscript via command line
#     return(normalizePath(sub(needle, "", cmdArgs[match])))
#   } else {
#     ls_vars = ls(sys.frames()[[1]])
#     if ("fileName" %in% ls_vars) {
#       # Source'd via RStudio
#       return(normalizePath(sys.frames()[[1]]$fileName)) 
#     } else {
#       if (!is.null(sys.frames()[[1]]$ofile)) {
#         # Source'd via R console
#         return(normalizePath(sys.frames()[[1]]$ofile))
#       } else {
#         # RStudio Run Selection
#         # http://stackoverflow.com/a/35842176/2292993  
#         return(normalizePath(rstudioapi::getActiveDocumentContext()$path))
#       }
#     }
#   }
# }
# 
# ###### SET WORKING DIRECTORY ######
# this.dir <- dirname(csf())
# setwd(this.dir)
# rm(list=ls())

CollateBiosheds <- function(biorefs.data, crop, range) {
  
  ###### LOAD LIBRARIES #######
  
  require(raster)
  require(sp)
  require(rgeos)
  require(maptools)
  require(dplyr)
  
  
  ###### SET PARAMS ######

  # units are either "mi" for miles or "km" for kilometers
  range.units <- "mi"
  
  # init sptdf to store collated RID key
  rid.bioshed.df <- data.frame("RID" = character(), 
                               "BIOSHED_CIDS" = character())
  

  # iterate over RID bioshed files
  for (rid in seq(1, nrow(biorefs))) {
    par.bioshed.file <- (paste0("../output/bin/RID_", 
                                rid, "_", crop, 
                                "_feedshed_", range, range.units, ".RDS"))
    
    # check if file exists:
    check <- file.exists(par.bioshed.file)
    
    if (check) {
      par.bioshed <- readRDS(par.bioshed.file)
      par.bioshed <- as.list(par.bioshed)
      row <- data.frame(rid, I(list(par.bioshed)))
      names(row) <- c("RID", "BIOSHED_CIDS")
      rid.bioshed.df <- rbind(rid.bioshed.df, row)
    }
  }
  
  # re-arrange columns
  output.df <- data.frame(rid.bioshed.df$BIOSHED_CIDS, rid.bioshed.df$RID)
  names(output.df) <- c("CIDS_IN_RANGE", "RID")
  
  # export collated biosheds
  saveRDS(output.df, paste0("../output/data_products/cur.ref.biosheds.", 
                            crop, ".", range, range.units, ".RDS"))
  
}














