#-----------------------------------------------------------------------------#
# main_feed_supply_network_analysis.R

# Type: R analysis script

# AUTHOR:
# Tyler Huntington, 2017
# JBEI Sustainability Research Group
# Project: Feedstock Agnostic Biorefinery Study

# PURPOSE:
# A script to perform a network analysis of feedstock supplies for biorefineries
# given the list of counties within range of each. 


# SIDE-EFFECTS:
# 

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
packages <- c("ggmap", "raster", "sp", "ggplot2", "rgeos", "rgdal")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

library(ggplot2)
library(ggmap)
library(raster)
library(sp)
library(rgeos)
library(rgdal)

###### SET GLOBAL VARS ######


###### LOAD CLEANED DATA ######

# load bioref locations data
curr.biorefs <- readRDS("../clean_binary_data/biorefs.sptdf.RDS")

# load county polygons data
counties <- readRDS("../clean_binary_data/counties.spdf.RDS")

# TODO: load centroids data for all US counties
centroids <- readRDS("../output/US.cluster.cents.sp.RDS")

# load US road network
road.net <- readRDS("../clean_binary_data/roads.sldf.RDS")

# load billion ton study biomass data
bt.data <- readRDS("../clean_binary_data/bt_biomass_18_30_40.df.RDS")


###### LOAD FUNCTIONS ######
source("parTLCidsInRange_fun.R")
source("SumBioshedFeeds_CIDS_fun.R")

# create boundary of midwest
mw.states <- c("IL", "IN", "IA", "KS", "MI", "MN", 
               "MO", "NE", "ND","OH", "SD", "WI")
  
mw.counties <- counties[counties$STATEABBREV %in% mw.states,]

mw.extent <- extent(mw.counties)

# generate raster layer with extent of midwest boundary
ras <- raster(mw.extent, ncol = 40, nrow = 40)
proj4string(ras) <- crs(counties)

# convert raster to pts
p.refs <- rasterToPoints(ras, spatial = T)

# get points within MW boundary
p.refs <- intersect(p.refs, mw.counties)
p.refs <- p.refs[,c("STATENAME", "STATEABBREV", "FIPS")]

# arrange points layer into similar format as current biorefs point layer
p.refs$RID <- 1:length(p.refs)
rownames(p.refs@data) <- rownames(p.refs@coords) <- p.refs$RID 


# determine ag cents in range of p.refs

# catchments <- parTLPointsInRange(biorefs.sptdf, centroids.data, 
biosheds <- TLPointsInRange(p.refs, centroids, crop = "ECrops", 
                            road.net, 
                            constraint = "distance", max.dist = 50, 
                            max.time = NULL)



feeds <- c("Sorghum", "Switchgrass", "Miscanthus")
p.refs <- SumBioshedFeeds(counties,
                          p.refs,
                          biosheds,
                          centroids,
                          bt.data,
                          feed_choices = feeds)










