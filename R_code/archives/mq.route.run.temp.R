# comp_routing_funs_analysis.R

# PURPOSE: an analysis script to compare different network routing functions.


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


###### LOAD LIBRARIES ######
packages <- c("raster", "sp", "ggplot2", "rgeos", "spatialEco")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

library(ggplot2)
library(raster)
library(sp)
library(rgeos)
library(spatialEco)

###### SOURCE FUNCTIONS ######
source("MqPointsInRange_fun.R")
source("TLPointsInRange_fun.R")

####### LOAD DATA ######
biorefs <- readRDS("../clean_binary_data/biorefs.sptdf.RDS")
county.cents <- readRDS("../output/US.wtd.cents.RDS")
roads.data <- readRDS("../clean_binary_data/roads.sldf.RDS")


###### SET PARAMS ######
constraint = c("distance")
max.dist = 50

###### PREP DATA ######
midwest.states <- c("IL", "IN", "IA", "KS", "MI",
                    "MN", "MS", "NE", "ND",
                    "OH", "SD", "WI")

biorefs <- biorefs[biorefs$STATE %in% midwest.states, ]

# # Determine points in range using the TIGER/Line routing function 
# tl.res <- TLPointsInRange(biorefs, county.cents, roads.data, constraint = constraint, 
#                           max.dist = max.dist)


biorefs
# Determine points in range for midwest states using the MapQuest Open Routing API
mq.res <- MqPointsInRange(biorefs, county.cents, constraint = constraint, 
                          max.dist = max.dist) 



