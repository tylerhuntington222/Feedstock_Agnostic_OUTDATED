
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
  
  ###### LOAD LIBRARIES #######
  packages <- c("ggmap", "raster", "sp", "ggplot2", "rgeos",
                "spatialEco", "geosphere", "doParallel", "iterators",
                "foreach", "rgdal")
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
  
  ###### SOURCE LOCAL FUNCTIONS ######
  source("TLRoute_clus_fun.R")
  
  ###### SET GLOBAL VARS ######
  aea.crs <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23.0
                 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 
                 +units=m +no_defs")
  
  wgs84.crs <- crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  
  ###### PERFORM NET ANALYSIS #######
  
  # init output list of FIPS vectors
  output.fips.ls <- vector("list", length = nrow(hubs.data))
  
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
    CheckIfInRange <- function(fips, par.bioref, cand.counties, edgest.data) {
      
      print(paste("Checking county with FIPS code: ", fips), echo = T)
      
      # init in.range vector to store results
      in.range <- c(NULL)
      
      # TEMP:
      #fips = 39001
      
      # get start point coords in WGS 84 CRS
      par.bioref.wgs <- spTransform(par.bioref, wgs84.crs)
      start <- par.bioref.wgs@coords[1,]
      
      # get end point
      cand.counties.wgs <- spTransform(cand.counties, wgs84.crs)
      end <- cand.counties.wgs[cand.counties.wgs$FIPS == fips, ]
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
      print(paste("Drive distance (miles): ", tmiles))
      
      # if travel time is less than thresh, save county FIPS to in.range
      # if (constraint == "time"){
      #   if (thrs <= max.hrs){
      #     return(fips)
      #   }
      # }
      if (constraint == "distance"){
        if (tmiles <= max.dist) {
          print("County in range")
          return(fips)
        } else {
          print("County not in range")
          return()
        }
      }
    }
    
    # init vector for counties in range
    in.range <- c(NULL)
    
    # initiate parallel cluster
    no_cores <- detectCores() - 1
    cl <- makeCluster(no_cores)
    registerDoParallel(cl)
    
    # iterate over candidate counties
    fips.in.range <- 
      foreach (f = cand.counties$FIPS[1:length(cand.counties$FIPS)],
               .export = c("TLRoute"),
               .packages = c('raster', 'sp', 'rgdal',
                             'rgeos', 'geosphere')) %dopar% {
                               CheckIfInRange(f, par.bioref, 
                                              cand.counties, edges.data)
                             }
    
    # stop cluster
    stopCluster(cl)
    
    # init vec to store results
    in.range <- c(NULL)
    
    # collect results in vector
    for (i in seq(1, length(fips.in.range))) {
      elem <- as.character(fips.in.range[[i]][1])
      if (!is.null(fips.in.range[i])) {
        in.range <- c(in.range, elem)
      }
    }
    
    # create subset of county centroids for counties in range
    in.range.counties <- cand.counties[cand.counties$FIPS %in% in.range, ]
    
    # if no counties in range assign NA to FIPS_IN_RANGE col
    if (is.null(in.range)){
      output.fips.ls[[RID]] <- NA
    } else {
      # add in.range.counties list to output list
      output.fips.ls[[RID]] <- in.range
    }
    
    saveRDS(in.range, paste0("../output/feedsheds/clus/RID_", RID,  
                             "_feedshed_50mi.RDS"))
  }
  
  # generate output df
  output <- data.frame(I(output.fips.ls))
  output$RID <- hubs.data$RID
  output <- data.frame(output)
  
  # clean up output df
  n <- nrow(output)
  output <- output[((n/2)+1):n,]
  rownames(output) <- seq(1, nrow(output))
  output <- data.frame(output[,2], output[,1])
  names(output) <- c("RID", "FIPS_IN_RANGE")
  
  return(output)
  
}


###### STATE BY STATE I/O ######

###### LOAD LIBRARIES #######
packages <- c("ggmap", "raster", "sp", "ggplot2", "rgeos",
              "spatialEco", "geosphere", "doParallel", "iterators",
              "foreach")
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

###### LOAD DATA ######
hubs.data <- readRDS("../clean_binary_data/biorefs.sptdf.RDS")
nodes.data <- readRDS("../output/US.wtd.cents.RDS")
edges.data <- readRDS("../clean_binary_data/roads.sldf.RDS")
roads.sldf <- edges.data
constraint = c("distance")
max.dist = 50


  
# subset refineries for particular state
# to.do <-   c(seq(10,12), seq(14,18),seq(22,35), seq(39,40), seq(46,50), seq(54,55), seq(57,58),
#              seq(65,67), seq(69,70), seq(72,73), seq(75,78), seq(82,85), seq(99,101),
#              seq(103,106), 108, seq(110,125), 127, seq(129,134), seq(137, 138),
#              seq(141,148), 150, seq(153,183), seq(187,188), seq(190,191), seq(196,201),
#              seq(201,213))

to.do <- c(23, 196)

# calc biosheds for refineries in to.do list
par.ref <- (hubs.data[hubs.data$RID %in% to.do, ])


# calc feedsheds by state
st.counties.in.range <- TLPointsInRange(par.ref, nodes.data, 
                                        edges.data,
                                        constraint = constraint,
                                        max.dist = max.dist)



# check which refs are missing from a par state
unique(hubs.data$STATE)

state.refs <- hubs.data[hubs.data$STATE == "TN",]

# TODO:
c(23, 26, 30, 39, 40, seq(46, 49), 65, 97, 114, 117, 
  130, 131, 142, 143, 144, 147, 148, 153, 154, 156, 157,
  166, 167, 168, seq(173, 175), 179, 183, 187, 196, 198,
  200, 204, 205, 207, 211, 212)

state.refs$RID

hubs.data@data



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










