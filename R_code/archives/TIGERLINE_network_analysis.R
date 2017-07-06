
#-----------------------------------------------------------------------------#
# network analysis.R
# Type: R analysis script

# AUTHOR:
# Tyler Huntington, 2017
# JBEI Sustainability Research Group
# Project: Feedstock Agnostic Biorefinery Study

# PURPOSE:
# A script to perform preliminary feedstock supply analysis.


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

###### SET STANDARDIZED CRS)
aea.crs <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23.0
                   +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 
                   +units=m +no_defs")

###### LOAD LIBRARIES ######
library(raster)
library(spdep)
library(maptools)
library(rgdal)
library(plyr)
library(geosphere)
library(rgeos)
library(raster)
library(spdep)
library(maptools)
library(rgdal)
library(plyr)
library(igraph)
library(shp2graph)

###### LOAD CLEANED DATA ######

#biomass.df <- readRDS("../clean_binary_data/bt_biomass_18_30_40.df.RDS")

# load biorefineries data
biorefs.sptdf <- readRDS("../clean_binary_data/biorefs.sptdf.RDS")

# re-project biorefs to standard CRS
biorefs.sptdf <- spTransform(biorefs.sptdf, aea.crs)

counties.spdf <- readRDS("../clean_binary_data/counties.spdf.RDS")

states.spdf <- readRDS("../clean_binary_data/states.spdf.RDS")

# load roads
roads.sldf <- readRDS("../clean_binary_data/roads.sldf.RDS")

# load centroids
centroids.spdf <- 
  readRDS("../output/iowa_county_cntroids.sptdf.RDS")


# subset states for Iowa
states.spdf <- subset(states.spdf, states.spdf$NAME == "Iowa")

# subset counties for Iowa
counties.spdf <- subset(counties.spdf, counties.spdf$STATENAME == "IOWA")

# clip road network to Iowa
#roads.sldf <- gIntersection(roads.sldf, states.spdf)

# crop extent of roads to iowa
roads.sldf <- crop(roads.sldf, states.spdf)

# subset biorefs to only those within Iowa
biorefs.sptdf <- raster::intersect(biorefs.sptdf, states.spdf)


# convert road layer to network
roads.net <- readshpnw(roads.sldf, ELComputed=T, longlat=T, Detailed=F, ea.prop=NULL)

# get nodes of road network
road.nodes <- (roads.net[[2]])

# get coords of road net nodes
Nodes.coordinates(nodes)

# get coords of bioref locations
bioref.coords <- biorefs.sptdf@coords

# generate zero vector as placeholder for road line attributes
cols <- c(0)
for (i in seq(1, length(names(roads.sldf))-1)){
  cols <- c(cols, 0)
}

# integrate bioref points into net
# creates new node on net for each bioref at closest point on nearest edge
net <- points2network(roads.sldf, bioref.coords, mapping.method=2, 
                      ELComputed=T, longlat=T, Detailed=F, ea.prop=cols)

# visualize
ptsinnt.view(roads.sldf, net[[1]], bioref.coords, bioref.node.key)

# get key for bioref node IDs
bioref.node.key <- net[[3]]

# convert key to df
RIDs <- seq_along(bioref.node.key)
nodeIDs <- c(NULL)
for (i in seq_along(bioref.node.key)){
  nodeIDs <- c(nodeIDs, bioref.node.key[[i]])
}

bioref.node.key.df <- data.frame(RIDs, nodeIDs)
names(bioref.node.key.df) <- c("RID", "nodeID")


# merge node IDs to biorefs 
biorefs.sptdf <- sp::merge(biorefs.sptdf, bioref.node.key.df, by = "RID")

names(biorefs.sptdf)

# i

# integrate

ptsinnt.view(roads.sldf, nodelist, pointsxy, CoorespondIDs, VElist=NULL)


# rbind nodes for roadnodes, biorefs and centroids

# produce an igraph object for roads, county centroids and bioref locations
nel2igraph(nodelist, edgelist, weight = NULL, eadf = T, Directed = FALSE)

head(roads.net[[3]])

plot(states.spdf)
plot(counties.spdf, add = T)
plot(roads.sldf, add =F)
plot(centroids.spdf, add = T , col = 'red')
plot(biorefs.sptdf, add = T, col = "blue")


# NOTES:







plot(roads.net)



