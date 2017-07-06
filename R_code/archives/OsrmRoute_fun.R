
#-----------------------------------------------------------------------------#
# MqRoute_fun.R 

# Type: R function script

# AUTHOR:
# Tyler Huntington, 2017
# JBEI Sustainability Research Group
# Project: Feedstock Agnostic Biorefinery Study

# PURPOSE:
# A function to determine drive distances and drive times between two locations
# by querying the MapQuest routing API under their open community use policy.
# As of June 2017, there is no query limit for usingthis API.


# PARAMETERS:
# start - the starting lcoation of the route as a character with no whitespaces.
# pluss (+) signs may be used in place of spaces. i.e. 53+Hollis+St. 
# if given as coordinates, they must be geographic in WGS84 datum and provided
# in the following format: "lat,long" e.g. "42.486217,-94.176046".
# E.g. county centroids.
# end - the ending location provided as a character with no whitespaces.

# RETURNS:
# a data frame with one entry (for the optimal driving route) 
# and two columns (drive_time and drive_dist).

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


OsrmRoute <- function(start, end) {
  
  
  # get start and end point lat long vals
  s.lat <- as.numeric(start[2])
  s.long <- as.numeric(start[1])
  
  e.lat <- as.numeric(end[2])
  e.long <- as.numeric(end[1])
  
  # TEMP: sample vals
  # s.lat <- 41.944472
  # s.long <- -94.343238
  # 
  # e.lat <- 42.486217
  # e.long <- -94.176046
  
  
  # load libraries
  packages <- c("curl", "jsonlite", "httr")
  if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
    install.packages(setdiff(packages, rownames(installed.packages())))  
  }
  library(curl)
  library(jsonlite)
  library(httr)
  
  # set server
  server <- "http://router.project-osrm.org/"
  
  # coords
  coords <- paste(s.long, ",", s.lat, ";", e.long, ",", e.lat, sep = "")
  
  # construct the routing JSON request
  route.req <- paste("route/v1/driving/", coords, 
                     "?overview=false", sep = "")
  
  # Build the query
  query <- paste(server, route.req, sep = "")
  
  
  # initialize connection to webpage
  con <- curl(url = query)
  
  # open connection for reading JSON file
  open(con, "r")
  print("Opening connection to OSRM Open Route API...", echo = T)
  
  # fetch response from webpage
  response <- fromJSON(con)
  print("Fetching route info...")
  
  # close connection
  close(con)
  print ("Closing connection to OSRM Open Route API...", echo = T)
  
  # extract travel time and distance from response
  # distance given in meters
  drive_dist <- response$route$distance 
  
  # convert to miles
  drive_dist <- (drive_dist*0.00062) 
  
  # time given in seconds
  drive_time <- response$route$duration
  
  # convert time to hours
  drive_time <- as.numeric(drive_time)/3600
  
  # format distance and time into dataframe
  route.df <- data.frame(drive_dist, drive_time)
  
  return(route.df)
  
}








