
#-----------------------------------------------------------------------------#
# TLPointsInRange_fun.R

# Type: R function script

# AUTHOR:
# Tyler Huntington, 2017
# JBEI Sustainability Research Group
# Project: Feedstock Agnostic Biorefinery Study

# PURPOSE:
# A function to perform a network analysis to determine which counties are 
# within a specified range (either drive time or drive distance) of a focal 
# point using the TIGER/Line US primary and secondary road network.

# PARAMETERS:
# hubs.data - object of class SpatialPointsDataFrame with the points to use as
# "hubs," or start points of the network analysis. 
# E.g. biorefineries. 
# nodes.data - object of class SpatialPointsDataFrame containing the 
# destination points to be used in the network analsysis. 
# E.g. county centroids.
# constraint - a charater (either "time" or "distance") indicating type of 
# constraint to set for determining whether points are in range of hubs. 
# max.dist - numeric inidicating the drive distance (in miles) to use as an 
# upper bound for determining which points are within range of hubs.   
# max.time - numeric inidicating the drive time (in hours) to use as an 
# upper bound for determining which points are within range of hubs. 

# RETURNS:
# list of lists in which first element is RID and second is
# character vector of county names within specified range of refinery

#-----------------------------------------------------------------------------#

# # TEMP: set wd for testing function locally in IDE
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
# this.dir <- dirname(csf())
# setwd(this.dir)
# rm(list=ls())

TLPointsInRange <- function(hubs.data, nodes.data, edges.data, constraint, 
                            max.dist = NULL, max.time = NULL) {
  
  #TODO: change roads.sldf to edges.data in rest of script
  roads.sldf <- edges.data
  
  
  # # TEMP: set sample params for local function testing
  # hubs.data <- readRDS("../clean_binary_data/biorefs.sptdf.RDS")
  # nodes.data <- readRDS("../output/ILLINOIS_wtd_cntroids.sptdf.RDS")
  # edges.data <- readRDS("../clean_binary_data/roads.sldf.RDS")
  # roads.sldf <- edges.data
  # constraint = c("distance")
  # max.dist = 50
  # 
  ###### LOAD LIBRARIES #######
  packages <- c("ggmap", "raster", "sp", "ggplot2", "rgeos",
                "spatialEco", "geosphere")
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
  
  ###### SOURCE LOCAL FUNCTIONS ######
  source("TLRoute_fun.R")
  
  ###### SET GLOBAL VARS ######
  aea.crs <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23.0
                 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 
                 +units=m +no_defs")
  
  wgs84.crs <- crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  
  ###### PERFORM NET ANALYSIS #######
  
  # init output list of FIPS vectors
  output.fips.ls <- vector("list", length = nrow(hubs.data))
  
  # init output vector to store route distances
  distances.v <- c(NULL)
  
  # iterate over all bioref locations
  #for (RID in seq_along(hubs.data$RID)){
  
  # TEMP: subset for iowa refs
  #illinois.ref.RIDS <- (hubs.data[hubs.data$STATE == "IL", ])$RID
  
  # TEMP: constrain RID sequence
  for (RID in hubs.data$RID) {
  
    print(paste("Working on refinery with RID: ", RID), echo = T)
    # TEMP: 
    #RID = 22
    
    # subset for particular bioref
    par.bioref <- hubs.data[hubs.data$RID == RID,]
    
    # establish search space of counties based on buffer radius
    buff.rad <- 60 # set radius in miles
    buff.rad <- 1609.34*buff.rad # convert to meters
    
    # create buffer around focal refinery
    buff <- gBuffer(par.bioref, byid = F, 
                    width = buff.rad,         
                    quadsegs = 100)
    
    # subset county nodes.data for those in search space
    cand.counties <- crop(nodes.data, buff)
    
    # generate basemap to pass to TLRoute function
    # clip road network to search radius of hub
    bmap <- crop(roads.sldf, buff)
    
    # set buffer CRS
    #refinery_buff.sp <- spTransform(refinery_buff.sp, aea.crs)
    
    if (constraint == "time"){
      # set threshold travel time (in hours)
      max.hrs <- max.hrs
    }
    
    if (constraint == "distance"){
      # set threshold travel dist in miles
      max.dist <- max.dist
    }
    
    # init vector for counties in range
    in.range <- c(NULL)
    
    # iterate over candidate counties
    for (fips in cand.counties$FIPS){
      
      print(paste("Checking county with FIPS code: ", fips), echo = T)
      
      # TEMP:
      #fips = 39001
      
      # get start point coords in WGS 84 CRS
      par.bioref.wgs <- spTransform(par.bioref, wgs84.crs)
      start <- round(as.numeric(par.bioref.wgs@coords[1,]), 6)
      
      # get end point
      cand.counties.wgs <- spTransform(cand.counties, wgs84.crs)
      end <- cand.counties.wgs[cand.counties.wgs$FIPS == fips, ]
      end <- round(as.numeric(end@coords[1,]), 6)
      
      # calculate route
      rt <- TLRoute(bmap, start, end)
      
      # calculate total distance of route in meters
      dist <- sum(rt$length)
      
      # convert to miles
      tmiles <- dist*0.00062
      
      # add to distances vector
      distances.v <- c(distances.v, tmiles)
      
      # print dist
      print(paste("Drive distance (miles): ", tmiles))
      
      # if travel time is less than thresh, save county FIPS to in.range
      if (constraint == "time"){
        if (thrs <= max.hrs){
          in.range <- c(in.range, fips)
        }
      }
      if (constraint == "distance"){
        if (tmiles <= max.dist){
          in.range <- c(in.range, fips)
          print("County in range")
        } else {
          print("County not in range")
        }
      }
    }
    
    print (paste("FIPs of counties in range: ", in.range))
    
    # create subset of county centroids for counties in range
    in.range.centroids <- cand.counties[cand.counties$FIPS %in% in.range, ]
    
    # if no counties in range assign NA to FIPS_IN_RANGE col
    if (is.null(in.range)){
      output.fips.ls[[RID]] <- NA
    } else {
      
      # add in.range FIPS list to output list
      output.fips.ls[[RID]] <- in.range 
    }
  }
  output <- data.frame(I(output.fips.ls))
  output$RID <- hubs.data$RID
  output <- data.frame(output)
  
  #return(output)
  
  #TEMP
  return(distances.v)
  
}

