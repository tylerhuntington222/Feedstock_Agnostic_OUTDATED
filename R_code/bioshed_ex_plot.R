
#-----------------------------------------------------------------------------#
# bioshed_ex_plot.R
# Type: R cleaning script

# AUTHOR:
# Tyler Huntington, 2017
# JBEI Sustainability Research Group
# Project: Feedstock Agnostic Biorefinery Study

# PURPOSE:
# A plotting script to provide an visual example of a bioshed.  
#-----------------------------------------------------------------------------#

# ###### TEMP: SET WD TO THIS SCRIPT LOCATION FOR FUNCTION TESTING ######
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

###### LOAD LIBRARIES #######
packages <- c("ggmap", "raster", "sp", "ggplot2", "rgeos", "rgdal",
              "spatialEco", "geosphere", "shp2graph", "igraph")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

library(ggplot2)
library(ggmap)
library(raster)
library(sp)
library(rgeos)
library(spatialEco)
library(geosphere)
library(igraph)
library(shp2graph)
library(rgdal)



###### LOAD DATA ######

# load bioref locations data
biorefs <- readRDS("../clean_binary_data/biorefs.sptdf.RDS")

# load centroids data for all US counties
cents <- readRDS("../output/US.cluster.cents.sp.RDS")

# load counties data
counties <- readRDS("../clean_binary_data/counties.spdf.RDS")

# load roads data
roads.data <- readRDS("../clean_binary_data/roads.sldf.RDS")

aea.crs <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23.0
               +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 
               +units=m +no_defs")
wgs84.crs <- crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

# select bioref to examine
for (rid in (129)) {
  
  in.range.cids <- readRDS(paste0("../output/feedsheds/kmeans_clusters/RID_",
                                  rid,"_feedshed_50mi.RDS"))
  
  # set to projected CRS for geoprocessing
  counties <- spTransform(counties, aea.crs)
  biorefs <- spTransform(biorefs, aea.crs)
  cents <- spTransform(cents, aea.crs)
  
  # subset data
  bioref <- biorefs[biorefs$RID == rid,]
  in.range <- cents[cents$cid %in% in.range.cids,]
  
  # set buffer for search area
  buff <- buffer(bioref, width = 80468, quadsegs = 100)
  
  # determine candidate counties 
  cand.cents <- crop(cents, buff)
  
  # set to consistent CRS for plotting
  counties <- spTransform(counties, wgs84.crs)
  bioref <- spTransform(bioref, wgs84.crs)
  cand.cents <- spTransform(cand.cents, wgs84.crs)
  in.range <- spTransform(in.range, wgs84.crs)
  buff <- spTransform(buff, wgs84.crs)
  roads <- crop(roads.data, buff)
  
  # set up plot
  plot(buff, border = 'orange', main = paste0("RID: ", rid))
  plot(roads, add = T, col = "grey")
  plot(cand.cents, add = T, pch = 16, col = 'red')
  plot(in.range, add = T, col = 'green', pch = 16)
  plot(bioref, add = T, pch = 10, col = 'blue', cex = 1.2)
  plot(counties, add = T)
}



