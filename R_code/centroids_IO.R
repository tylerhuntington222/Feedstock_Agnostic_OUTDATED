#------------------------------------------------------------------------------#
# centroids_analysis.R

# AUTHOR:
# Tyler Huntington, 2017

# JBEI Sustainability Team
# Feedstock Agnostic Study
# PI: Corinne Scown PhD

# PURPOSE:
# An analysis script to find the unweighted geographic centroids of all US 
# counties.


# OUTPUTS:
# An object of class SpatialPointsDataFrame in which each point represents a 
# county centroid

#------------------------------------------------------------------------------#

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

###### SOURCE LOCAL FUNCTIONS #######
source("weighted_centroids_fun.R")


# ###### INSTALL PACKAGES IF NECCESSARY ######
# install.packages("raster")
# install.packages("spdep")
# install.packages("maptools")
# install.packages("rgdal")
# install.packages("plyr")
# install.packages("geosphere")
# install.packages("rgeos")
# install.packages("raster")
# install.packages("spdep")
# install.packages("maptools")
# install.packages("rgdal")
# install.packages("plyr")


###### LOAD LIBRARIES ######
library(raster)
library(spdep)
library(maptools)
library(rgdal)
library(plyr)
library(geosphere)
library(rgeos)
library(raster)
library(spdep)
library(maptools)
library(rgdal)
library(plyr)

###### LOAD DATA ######

# load county boundaries data
counties.spdf <- readRDS("../clean_binary_data/counties.spdf.RDS")

# TEMP: crop counties layer to Iowa for testing functionality
counties.spdf <- subset(counties.spdf, counties.spdf$STATENAME == "IOWA")

###### COMPUTE WEIGHTED CENTROIDS ######
cntrs.sptdf <- gCentroid(counties.spdf, byid = T)

plot.new()
plot(counties.spdf)
plot(cntrs.sptdf, add = T)

# export result as binary data file in output directory
saveRDS(cntrs.sptdf, "../output/iowa_county_cntroids.sptdf.RDS")






