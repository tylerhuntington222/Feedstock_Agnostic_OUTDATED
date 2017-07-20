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

# define function to parallelize routing computations
CheckIfInRange <- function(cid, par.bioref, cand.counties, edges.data) {
  
  cat(sprintf("Checking refinery with RID: %s and centroid with CID: %s", 
              RID, cid), echo = T)
  
  # init in.range vector to store results
  in.range <- c(NULL)
  
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