
#-----------------------------------------------------------------------------#
# CountiesInRange_fun.R

# Type: R function script

# AUTHOR:
# Tyler Huntington, 2017
# JBEI Sustainability Research Group
# Project: Feedstock Agnostic Biorefinery Study

# PURPOSE:
# A function to perform a network analysis to determine which counties are 
# within a specified range (either drive time or drive distance) of a focal 
# point. 

# PARAMETERS:
# hubs.data
# points.data
# constraint
# max.dist = NULL 
# max.time = NULL

# RETURNS:
# list of lists in which first element is RID and second is
# character vector of county names within specified range of refinery

#-----------------------------------------------------------------------------#

###### SET WD ######

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

this.dir <- dirname(csf())
setwd(this.dir)
rm(list=ls())

PointsInRange <- function(hubs.data, points.data, constraint,
                            max.dist = NULL, max.time = NULL){
  
  ###### LOAD LIBRARIES #######
  packages <- c("ggmap", "raster", "sp", "ggplot2", "rgeos")
  if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
    install.packages(setdiff(packages, rownames(installed.packages())))  
  }
  
  library(ggplot2)
  library(ggmap)
  library(raster)
  library(sp)
  library(rgeos)
  
  ###### LOAD DATA ######
  
  biorefs.sptdf <- hubs.data
  
  centroids <- points.data
  
  ###### SET GLOBAL VARS ######
  wgs84.crs <- crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  
  
  ###### PERFORM NET ANALYSIS #######
  
  # init output list
  output.ls <- list(NULL)
  
  # iterate over bioref locations
  for (RID in seq_along(biorefs.sptdf$RID)){
    
    # TEMP: 
    #RID = 22
    
    
    # subset for particular bioref
    par.bioref <- biorefs.sptdf[biorefs.sptdf$RID == RID,]
    
  
    
    # establish search space of counties based on buffer radius
    buff.rad <- 100 # set radius in miles
    buff.rad <- 1609.34*buff.rad # convert to meters
    
    # create buffer around focal refinery
    buff <- gBuffer(par.bioref, byid = F, 
                                width = buff.rad,         
                                quadsegs = 100) 
    
    # set buffer CRS
    #refinery_buff.sp <- spTransform(refinery_buff.sp, aea.crs)
    
    # subset county centroids for those in search space
    cand.counties <- raster::intersect(centroids, buff)
    
    if (constraint == "time"){
      # set threshold travel time (in hours)
      max.hrs <- max.hrs
    }
    
    if (constraint = "distance"){
      # set threshold travel dist in miles
      max.dist <- max.dist
    }
    
    # init vector for counties in range
    in.range <- c(NULL)
    
    # iterate over candidate counties
    for (fips in cand.counties$FIPS){
      
      # TEMP:
      #fips = 39001
      
      # get start point coords in WGS 84 CRS
      par.bioref.wgs <- spTransform(par.bioref, wgs84.crs)
      start <- par.bioref.wgs@coords[1,]
      
      
      # get end point
      cand.counties.wgs <- spTransform(cand.counties, wgs84.crs)
      end <- cand.counties.wgs[cand.counties.wgs$FIPS == fips, ]
      end <- end@coords[1,]
      
      # calculate route
      rt <- route(start, end, 
                  mode = c("driving"),
                  structure = c("route"), output = c("simple", "all"),
                  alternatives = FALSE, messaging = FALSE, sensor = FALSE,
                  override_limit = T)
      
      routeQueryCheck()
      
      # calculate total distance of route
      tmiles <- sum(na.omit(rt$miles))
      
      # calculate total travel time of route in hrs
      thrs <- sum(na.omit(rt$hours))
      print(paste("Travel time (hrs): ", thrs))
      
      # if travel time is less than thresh, save county FIPS to in.range
      if (constraint == "time"){
        if (thrs <= max.hrs){
        in.range <- c(in.range, fips)
        }
      }
      if (constraint == "dist"){
        if (tmiles <= max.dist){
          in.range <- c(in.range, fips)
        }
      }
    }
    
    # create subset of county centroids for counties in range
    in.range.centroids <- cand.counties[cand.counties$FIPS %in% in.range, ]
      
    # add in.range list to output list
    output.ls[[RID]][1] <- RID
    output.ls[[RID]][2] <- list(in.range)
  }
  
  return(output.ls)
}

  
    
      
    
      
  # output: list of lists in which first element is RID and second is
  # character vector of county names within specified range of refinery
    
    
    
# # plots for development and testing
# plot(counties.spdf)
# plot(cand.counties, add = T)
# plot(buff, add = F)
# plot(buff, add = T, col = 'red')
# plot(par.bioref, add = T, col = 'blue', pch = 16)
# plot(in.range.centroids, add = T, col = "orange")



    