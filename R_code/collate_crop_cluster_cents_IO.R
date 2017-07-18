
#-----------------------------------------------------------------------------#
# collate_crop_cluster_cents_IO.R
# Type: R I/O script

# AUTHOR:
# Tyler Huntington, 2017
# JBEI Sustainability Research Group
# Project: Feedstock Agnostic Biorefinery Study

# PURPOSE:
# A script to collate the county-level kmeans cluster centroids for production
# of a particular crop from separate binary data files (one for each county).


# SIDE-EFFECTS:
# outputs a binary data file (.RDS) to the 'output' directory which contains an
# object of class SpatialPointsData frame containing k-means cluster centroids
# for the specified crop across the continental US. 

#-----------------------------------------------------------------------------#

# ##### BASIC FUNCTIONS ######
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

###### LOAD LIBRARIES ######
require(broom)
require(dplyr)
require(ggplot2)
require(colorspace)
require(sp)
require(maptools)
require(raster)
require(rgdal)
require(spatial)
require(foreach)
require(iterators)
require(parallel) 
require(parallel)
require(snow)
require(doSNOW)
require(rgeos)

aea.crs <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23.0 
               +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 
               +units=m +no_defs")

###### LOAD DATA ######


# Set params
crop <- "Oats"
counties <- readRDS("../clean_binary_data/counties.spdf.RDS")
fips.codes <- as.character(counties$FIPS)

dir <- sprintf("../output/county_kmeans_%s/", crop)

county.files <- list.files(dir)
county.files <- grep(".RDS", county.files, value = T)

# init result df for all US cents
US.cluster.cents <- NULL

# init pass counter
pass <- 1

for (file in county.files) {
  
  # load county cluster cents
  filepath <- paste0(dir, file)
  county.cents <- readRDS(filepath)
  
  if (pass == 1) {
    US.cluster.cents <- county.cents
  } else {
    
    US.cluster.cents <- rbind(US.cluster.cents, county.cents)
  }
  pass <- pass + 1
}


# convert cluster centers in spatial points
US.cluster.cents.coords <- US.cluster.cents[ ,1:2]
US.cluster.cents.sp <- SpatialPoints(US.cluster.cents.coords,
                                     proj4string = aea.crs)
cluster.data <- US.cluster.cents[ ,3:(ncol(US.cluster.cents))]
US.cluster.cents.sp <- sp::SpatialPointsDataFrame(US.cluster.cents.sp,
                                                  cluster.data,
                                                  proj4string = aea.crs)

# export all US cluster cents in binary data file
crop <- gsub(" ", "_", crop)
saveRDS(US.cluster.cents.sp,
        paste0("../output/US.cluster.cents.", crop, ".RDS"))


  