# comp_routing_funs_analysis.R

# PURPOSE: an analysis script to compare different network routing functions.


###### BASIC FUNCTIONS ######

# a basic function to get the filepath of the current script
csf <- function() {
  # install packages
  #install.packages("rstudioapi")
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


###### LOAD LIBRARIES ######
packages <- c("raster", "sp", "ggplot2", "rgeos", "spatialEco")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

library(ggplot2)
library(raster)
library(sp)
library(rgeos)
library(spatialEco)

###### SOURCE FUNCTIONS ######
source("MqPointsInRange_fun.R")
source("TLPointsInRange_fun.R")
source("OsrmPointsInRange_fun.R")
source("GmapsPointsInRange_fun.R")
source("TLroute_fun.R")
list.files()

####### LOAD DATA ######
biorefs <- readRDS("../clean_binary_data/biorefs.sptdf.RDS")
county.cents <- readRDS("../output/US.wtd.cents.RDS")
roads.data <- readRDS("../clean_binary_data/roads.sldf.RDS")
counties <- readRDS("../clean_binary_data/counties.spdf.RDS")
cents <- readRDS("../output/US.wtd.cents.RDS")


###### SET PARAMS ######
constraint = c("distance")
max.dist = 50

###### PREP DATA ######
midwest.states <- c("IL", "IN", "IA", "KS", "MI",
                    "MN", "MS", "NE", "ND",
                    "OH", "SD", "WI")


# biorefs <- biorefs[biorefs$STATE %in% midwest.states, ]

# TEMP: constrain to single state
biorefs <- biorefs[biorefs$STATE %in% c("ND"), ]

# Determine points in range using the TIGER/Line routing function 
# tl.res <- TLPointsInRange(biorefs, county.cents, roads.data, 
#                           constraint = constraint,
#                           max.dist = max.dist)



# # Determine points in range for midwest states using the osrm Open Routing API
# osrm.res <- OsrmPointsInRange(biorefs, county.cents, constraint = constraint, 
#              max.dist = max.dist) 
# 
# 
# # Determine points in range for midwest states using the osrm Open Routing API
# osrm.res <- (biorefs, county.cents, constraint = constraint, 
#              max.dist = max.dist) 


gmaps.res <- GmapsPointsInRange(biorefs, cents, constraint = constraint,
                                max.dist = max.dist)

(gmaps.res[,1])


saveRDS(gmaps.res, "gmaps.nd.pir.RDS")


# compare results from Tigerline and Google maps approaches

gmaps <- readRDS("gmaps.iowa.pir.RDS")
tl <- readRDS("tl.iowa.pir.RDS")


tl.res
length(tl.res) == length(gmaps.res)

plot(tl.res, gmaps.res)

abline(0,1, add = T)

model <- lm( ~ gmaps.res)

summary(model)

biorefs@data


# TODO: set up for loop to route via TL and Gmaps methods using same start/end
# points in identical order
# Need to pass cropped road network to TL function in each iteration
start <- c(-97.26349, 46.89957)
end <- c(-96.88698, 46.07561)

tl.test.rt <- TLRoute(basemap = roads.data, 
                      start = start, 
                      end = end) 

states <- readRDS("../clean_binary_data/states.spdf.RDS")
iowa <- states[states$NAME == "Iowa",]


iowa.counties <- counties[counties$STATEABBREV == "IOWA",]

iowa.roads <- raster::crop(roads.data, iowa)

iowa.cents <- cents[cents$STATENAME == "IOWA",]

iowa.biorefs <- biorefs[biorefs$STATE == "IA",]


plot(iowa)
plot(iowa.roads, add = T, col = 'green')
plot(iowa.cents, add = T, col = 'blue', pch = 16)
plot(iowa.biorefs, add = T, col = 'red', pch = 8)

biorefs


plot(roads.data)








