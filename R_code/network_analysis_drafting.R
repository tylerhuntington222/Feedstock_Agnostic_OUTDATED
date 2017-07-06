
#-----------------------------------------------------------------------------#
# network analysis.R
# Type: R cleaning script

# AUTHOR:
# Tyler Huntington, 2017
# JBEI Sustainability Research Group
# Project: Feedstock Agnostic Biorefinery Study

# PURPOSE:
# A script to perform preliminary feedstock supply analysis


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

# load counties data
counties.spdf <- readRDS("../clean_binary_data/counties.spdf.RDS")

# load state boundaries data
states.spdf <- readRDS("../clean_binary_data/states.spdf.RDS")

# load roads
roads.sldf <- readRDS("../clean_binary_data/roads.sldf.RDS")

# load centroids
centroids.spdf <- readRDS("../output/ILLINOIS_wtd_cntroids.sptdf.RDS")


####### PREP DATA ######

wgs84.crs <- crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

# subset states for Illinois
states.spdf <- subset(states.spdf, states.spdf$NAME == "Illinois")

# subset counties for Illinois
counties.spdf <- subset(counties.spdf, counties.spdf$STATENAME == "ILLINOIS")


# subset for a particular bioref
bioref <- subset(biorefs.sptdf, biorefs.sptdf$RID == 58)

# susbet for a particular centroid
centroid <- centroids.spdf[43,]

# clip road network to 100 mile radius of hub
buff <- gBuffer(centroid, width = 160934, quadsegs = 100)
roads.sldf <- crop(roads.sldf, buff)

# reproject to wgs 84
bioref <- spTransform(bioref, wgs84.crs)
counties.spdf <- spTransform(counties.spdf, wgs84.crs)
states.spdf <- spTransform(states.spdf, wgs84.crs)
roads.sldf <- spTransform(roads.sldf, wgs84.crs)
centroid <- spTransform(centroid, wgs84.crs)



###### CONVERT TO NETWORK ######

# # convert road layer to network
# roads.net <- readshpnw(roads.sldf, ELComputed=T, 
#                        longlat=T, Detailed=T, ea.prop=NULL)
# 
# # get nodes of road network
# road.nodes <- (roads.net[[2]])

# get coords of road net nodes
# Nodes.coordinates(road.nodes)

# get coords of bioref locations
bioref.coords <- bioref@coords

# get coords of centroid
cent.coords <- centroid@coords

# generate zero vector as placeholder for road line attributes
cols <- c(0)
for (i in seq(1, length(names(roads.sldf))-1)){
  cols <- c(cols, 0)
}


# create matrix with start and end node coords
start.end.nodes <- matrix(nrow = 1, ncol = 2, as.numeric(bioref.coords))
start.end.nodes <- rbind(start.end.nodes, as.numeric(cent.coords))


# integrate bioref node into net
# creates new node on net for bioref at closest point on nearest edge
# appends node to end of nodelist 
net <- points2network(roads.sldf, start.end.nodes, mapping.method = 2, 
                      ELComputed=F, longlat=F, Detailed=F, ea.prop = cols)

# get nodes of road network
net.nodes <- (net[[1]])

# Nodes.coordinates(net.nodes)

start.node.id <- nrow(net.nodes)-1
end.node.id <- nrow(net.nodes)

start.node.coords <- net.nodes[length(net.nodes)-1]
end.node.coords <- net.nodes[length(net.nodes)]

# extract nodelist and edgelist from net object
nodelist <- net[[1]]
elist <- net[[2]]

sum(is.na(nodelist))

node.coords <- Nodes.coordinates(nodelist)

# create edgeID.df
edges.df <- data.frame(elist)

# init empty vector to accumulate edge length vals
elens <- c(NULL)

# compute edge lengths
for (e in seq(1, nrow(elist))) {
  ed <- elist[e,]
  id <- ed[1]
  hd <- ed[2]
  tl <- ed[3]
  
  # get coords of head and tail nodes for edge
  hd.coords <- node.coords[hd,]
  tl.coords <- node.coords[tl,]
  
  # calculate edge length
  elen <- distHaversine(hd.coords, tl.coords)
  
  # add edge length to elens
  elens <- c(elens, elen)
}

# bind edgelengths to edges.df
edges.df <- cbind(edges.df, elens)

# rename cols of edges df
names(edges.df) <- c("edge.id", "head.node", "tail.node", "length")

# produce an igraph object for roads, county centroids and bioref locations
graph <- nel2igraph(nodelist, elist, weight = NULL, eadf = edges.df, 
                    Directed = FALSE)

path <- shortest_paths(graph, start.node.id, end.node.id, mode = c("all"),
               weights = NULL, output=("both"))

# extract edge ids of edges in path
eids <- unlist(path$epath)

# generate edge list for edges in route
edges.df <- subset(edges.df, edges.df$edge.id %in% eids)

# calculate total distance of routh in meters
dist <- sum(edges.df$length)

# convert route dist to miles
dist <- dist*0.00062

return(dist)




