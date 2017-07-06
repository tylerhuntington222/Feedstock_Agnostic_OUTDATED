#-----------------------------------------------------------------------------#
# collate_feedsheds_IO.R
# Type: R I/O script

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

###### BASIC FUNCTIONS ######

# a basic function to get the filepath of the current script
csf <- function() {
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

###### LOAD LIBRARIES #######
packages <- c("ggmap", "raster", "sp", "ggplot2", "rgeos", "maptools", "dplyr")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

library(ggplot2)
library(ggmap)
library(raster)
library(sp)
library(rgeos)
library(maptools)
library(dplyr)

###### LOAD CLEANED DATA ######
biorefs <- readRDS("../clean_binary_data/biorefs.sptdf.RDS")

# init sptdf to store collated RID key
rid.bioshed.df <- data.frame("RID" = character(), "BIOSHED_FIPS" = character())

# specify bioshed data to read in
# range refers to the max drive dist used to determine
# biosheds in the net analysis
range <- 50

# units are either "mi" for miles or "km" for kilometers
range.units <- "mi"


# iterate over RID bioshed files
for (rid in seq(1, nrow(biorefs))) {
  par.bioshed.file <- (paste0("../output/feedsheds/RID_", rid,
                         "_feedshed_", range, range.units, 
                         ".RDS"))
  
  # check if file exists:
  check <- file.exists(par.bioshed.file)
  
  if (check == T) {
    par.bioshed <- readRDS(par.bioshed.file)
    par.bioshed <- list(par.bioshed)
    row <- data.frame(I(rid), I(par.bioshed))
    rid.bioshed.df <- rbind(rid.bioshed.df, row)
  }
}


# re-arrange columns

names(rid.bioshed.df) <- c("RID", "FIPS_IN_RANGE")

output.df <- data.frame(rid.bioshed.df$FIPS_IN_RANGE, rid.bioshed.df$RID)

names(output.df) <- c("FIPS_IN_RANGE", "RID")


# export collated biosheds
saveRDS(output.df, paste0("../output/curr_ref_biosheds_",
                               range, range.units, ".RDS"))


        











