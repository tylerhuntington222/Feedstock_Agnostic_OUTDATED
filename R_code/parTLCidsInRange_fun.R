
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
# 
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
  
  ###### LOAD LIBRARIES #######
  packages <- c("ggmap", "raster", "sp", "ggplot2", "rgeos",
                "spatialEco", "geosphere", "doParallel", "iterators",
                "foreach", "rgdal", "doSNOW", "snow")
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
  library(doParallel)
  library(iterators)
  library(foreach)
  library(rgdal)
  library(doSNOW)
  library(snow)
  
  ###### SOURCE LOCAL FUNCTIONS ######
  source("TLRoute_fun.R")
  
  ###### SET GLOBAL VARS ######
  aea.crs <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23.0
                 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 
                 +units=m +no_defs")
  
  wgs84.crs <- crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  
  ###### PERFORM NET ANALYSIS #######
  
  # init output list of cid vectors
  output.cids.ls <- vector("list", length = nrow(hubs.data))
  
  
  # initiate parallel cluster
  no_cores <- detectCores()
  cl <- makeCluster(no_cores, outfile = "")
  registerDoSNOW(cl)
  
  # TEMP: constrain RID sequence
  rid.res <- foreach (RID = hubs.data$RID,
             .export = c("TLRoute"),
             .packages = c('doSNOW', 'foreach', 'raster', 'sp', 'rgdal',
                           'rgeos', 'geosphere'))  %dopar% {
                                      
    cat(paste0("\nWorking on refinery with RID: ", RID), echo = T)
    # TEMP: 
    #RID = 22
    
    # subset for particular bioref
    par.bioref <- hubs.data[hubs.data$RID == RID,]
    
    # establish search space of counties based on buffer radius
    buff.rad <- max.dist # set radius in miles
    buff.rad <- 1609.34*buff.rad # convert to meters
    
    # create buffer around focal refinery
    buff <- gBuffer(par.bioref, byid = F, 
                    width = buff.rad,         
                    quadsegs = 100)
    
    # subset county nodes.data for those in search space
    cand.counties <- crop(nodes.data, buff)
    
    # generate basemap to pass to TLRoute function
    # clip road network to search radius of hub
    # buff <- spTransform(buff, wgs84.crs)
    # bmap <- crop(edges.data, buff)
    
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
    
    # define function to parallelize routing computations
    CheckIfInRange <- function(cid, par.bioref, cand.counties, edges.data) {
      
      cat(sprintf("\nChecking refinery with RID: %s and centroid with CID: %s", 
                  RID, cid), echo = T)
      
      # init in.range vector to store results
      in.range <- c(NULL)
      
      # TEMP:
      #cid = 39001
      
      # get start point coords in WGS 84 CRS
      par.bioref.wgs <- spTransform(par.bioref, wgs84.crs)
      start <- par.bioref.wgs@coords[1,]
      
      # get end point
      cand.counties.wgs <- spTransform(cand.counties, wgs84.crs)
      end <- cand.counties.wgs[cand.counties.wgs$cid == cid, ]
      end <- end@coords[1,]
      names(end) <- c("long", "lat")
      
      # calculate route
      rt <- TLRoute(edges.data, start, end)
      
      # calculate total distance of route in meters
      dist.df <- rt[[1]]
      
      # convert to miles
      tmiles <- dist.df$DIST.MI[1]
      
      #TEMP: check the distances being returned by routing function
      #return(tmiles)
      
      # print dist
      cat(paste0("\nDrive distance (miles): ", tmiles))
      
      # if travel time is less than thresh, save county CIDs to in.range
      # if (constraint == "time"){
      #   if (thrs <= max.hrs){
      #     return(cid)
      #   }
      # }
      if (constraint == "distance"){
        if (tmiles <= max.dist) {
          cat("\nCentroid in range")
          return(cid)
        } else {
          cat("\nCentroid not in range")
          return()
        }
      }
    }
    
    # iterate over candidate counties
    cids.in.range <- 
      foreach (f = cand.counties$cid[1:length(cand.counties$cid)],
               .export = c("TLRoute"),
               .packages = c('raster', 'sp', 'rgdal',
                             'rgeos', 'geosphere')) %do% {
        CheckIfInRange(f, par.bioref, cand.counties, 
                    edges.data)
      }
    
    # init vec to store results
    in.range <- c(NULL) 
    
    # collect results in vector
    for (i in seq(1, length(cids.in.range))) {
      elem <- as.character(cids.in.range[[i]][1])
      if (!is.null(cids.in.range[i])) {
        in.range <- c(in.range, elem)
      }
    }
    
    # create subset of county centroids for counties in range
    in.range.counties <- cand.counties[cand.counties$cid %in% in.range, ]
    
    # if no counties in range assign NA to CIDS_IN_RANGE col
    if (is.null(in.range)){
      output.cids.ls[[RID]] <- NA
    } else {
      # add in.range.counties list to output list
      output.cids.ls[[RID]] <- in.range
    }
    
    saveRDS(in.range, paste0("../output/feedsheds/kmeans_clusters/RID_", 
                             RID,  "_feedshed_", max.dist, "mi.RDS"))
    cat(sprintf("Finished with refinery RID: %s", RID))
    
    RID
                           }
  
  # stop cluster
  stopCluster(cl)
  
  # generate output df
  output <- data.frame(I(output.cids.ls))
  output$RID <- hubs.data$RID
  output <- data.frame(output)
  
  # clean up output df
  n <- nrow(output)
  output <- output[((n/2)+1):n,]
  rownames(output) <- seq(1, nrow(output))
  output <- data.frame(output[,2], output[,1])
  names(output) <- c("RID", "CIDS_IN_RANGE")
  
  return(output)
  
}


# ###### REFINERY BY REFINERY I/O ######
# 
# ###### LOAD LIBRARIES #######
# packages <- c("ggmap", "raster", "sp", "ggplot2", "rgeos",
#               "spatialEco", "geosphere", "doParallel", "iterators",
#               "foreach", "doSNOW", "snow")
# if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
#   install.packages(setdiff(packages, rownames(installed.packages())))
# }
# 
# library(ggplot2)
# library(ggmap)
# library(raster)
# library(sp)
# library(rgeos)
# library(spatialEco)
# library(geosphere)
# library(doParallel)
# library(iterators)
# library(foreach)
# library(doSNOW)
# library(snow)
# 
# ###### LOAD DATA ######
# hubs.data <- readRDS("../clean_binary_data/biorefs.sptdf.RDS")
# nodes.data <- readRDS("../output/US.cluster.cents.sp.RDS")
# edges.data <- readRDS("../clean_binary_data/roads.sldf.RDS")
# roads.sldf <- edges.data
# constraint <- c("distance")
# max.dist <- 70
# 
# # TODO: 
# # run with max.dist <- 30, 60
# # for 30m run: rids.to.do <- c(22:213)
# 
# # # iterate over all refs
# # rids.to.do <- hubs.data$RID
# 
# # iterate over particular ref
# rids.to.do <- c(1:213)
# 
# # still to.do
# #c(192, 183, 184, 147, 148, 149, 120, 166)
# 
# 
# refs.to.do <- (hubs.data[hubs.data$RID %in% rids.to.do, ])
# 
# 
# 
# # calc feedsheds by state
# st.counties.in.range <- TLPointsInRange(refs.to.do, nodes.data, edges.data,
#                                         constraint = constraint,
#                                         max.dist = max.dist)
# 





# ###### NOT RUN: FUNTION TESTING ######
# 
# ###### LOAD LIBRARIES #######
# packages <- c("ggmap", "raster", "sp", "ggplot2", "rgeos",
#               "spatialEco", "geosphere", "doParallel", "iterators",
#               "foreach")
# if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
#   install.packages(setdiff(packages, rownames(installed.packages())))  
# }
# 
# library(ggplot2)
# library(ggmap)
# library(raster)
# library(sp)
# library(rgeos)
# library(spatialEco)
# library(geosphere)
# library(doParallel)
# library(iterators)
# library(foreach)
# 
# ###### LOAD DATA ######
# hubs.data <- readRDS("../clean_binary_data/biorefs.sptdf.RDS")
# nodes.data <- readRDS("../output/US.wtd.cents.RDS")
# edges.data <- readRDS("../clean_binary_data/roads.sldf.RDS")
# roads.sldf <- edges.data
# constraint = c("distance")
# max.dist = 50
# 
#   
# # subset refineries for particular state
# hubs.data <- (hubs.data[hubs.data$RID == "131", ])
# 
# # calc feedsheds by state
# st.counties.in.range <- TLPointsInRange(hubs.data, nodes.data, edges.data, 
#                                         constraint = constraint, 
#                                         max.dist = max.dist)




# RUN LOG:

# 6/22 4:00 - started run over all RIDs - stopped at 4:40 bc stalled on RID 23
# stamp
