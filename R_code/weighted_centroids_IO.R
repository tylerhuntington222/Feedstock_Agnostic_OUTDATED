#------------------------------------------------------------------------------#
# weighted_centroids_analysis.R

# AUTHOR:
# Tyler Huntington, 2017

# JBEI Sustainability Team
# Feedstock Agnostic Study
# PI: Corinne Scown PhD

# PURPOSE:
# An analysis script to find the geographic centroids of all US counties 
# weighted by the spatial distribution of crop and pasture land within each 
# county. Weighted centroids will serve as the nodes in a network analysis for
# calculating potential feedstock supplies within range of existing US
# biorefinery locations. 

# OUTPUTS:
# An object of class SpatialPointsDataFrame in which each point 
# represents the weighted centroid of a county in the contiguous US.

#------------------------------------------------------------------------------#

###### BASIC FUNCTIONS ######

# a basic function to get the filepath of the current script
csf <- function() {
  # install packages
  install.packages("rstudioapi")
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

###### SOURCE LOCAL FUNCTIONS #######
source("weighted_centroids_fun.R")


###### INSTALL PACKAGES IF NECCESSARY ######
install.packages("raster")
install.packages("spdep")
install.packages("maptools")
install.packages("rgdal")
install.packages("plyr")
install.packages("geosphere")
install.packages("rgeos")
install.packages("raster")
install.packages("spdep")
install.packages("maptools")
install.packages("rgdal")
install.packages("plyr")


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

# start timer
#system.time({
  
###### LOAD DATA ######

# load county boundaries data
counties.spdf <- readRDS("../clean_binary_data/counties.spdf.RDS")

# load NLCD raster
nlcd <- 
  raster("../../raw_data_files/nlcd_2011_landcover_2011_edition_2014_10_10.img")

# TEMP: crop counties layer to Iowa for testing functionality
#counties.spdf <- subset(counties.spdf, counties.spdf$STATENAME == "IOWA")

# # TEMP: crop counties layer to US Midwest states only
# midwest.states <- c("IL", "IN", "IA", "KS", "MI", 
#                     "MN", "MS", "NE", "ND", 
#                     "OH", "SD", "WI")
# 
# counties.spdf <- subset(counties.spdf, 
#                         counties.spdf$STATEABBREV %in% midwest.states)

# TEMP: crop counties layer to ND
counties.spdf <- subset(counties.spdf, 
                        counties.spdf$STATEABBREV == "ND")

###### PREP DATA #######

# crop extent of nlcd RasterLayer to extent of US counties layer
nlcd <- crop(nlcd, counties.spdf)

# create matrix from mask raster layer
nlcd <- as.matrix(nlcd)

# make mask raster with binary vals based on crop/pasture (1) or other (0)
mask <- (nlcd == 81 | nlcd == 82)

# re-create raster layer from extracted matrix
mask <- raster(mask)

# re-project mask raster to standardized projection
proj4string(mask) <- crs(counties.spdf)

# set extent of mask to extent of county polys
extent(mask) <- extent(counties.spdf)

###### COMPUTE TRUE CENTROIDS ######
t.cents <- gCentroid(counties.spdf, byid = T)
  
###### COMPUTE WEIGHTED CENTROIDS ######
cntrs.sptdf <- WeightedCentroids(counties.spdf, mask)

# export result as binary data file in output directory
saveRDS(cntrs.sptdf, "../output/wtd_county_cntroids.sptdf.RDS")

#}) # end timer


# TEMP: Plots for testing and checking analysis
plot(counties.spdf)
plot(mask)
plot(counties.spdf, add = T)
plot(t.cents, add = T, col = 'blue')
plot(cntrs.sptdf, add = T, col = 'red')






