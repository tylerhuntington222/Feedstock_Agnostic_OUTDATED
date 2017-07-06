
#-----------------------------------------------------------------------------#
# TLRoute_fun.R
# Type: R function script

# AUTHOR:
# Tyler Huntington, 2017
# JBEI Sustainability Research Group
# Project: Feedstock Agnostic Biorefinery Study

# PURPOSE:
# A script to perform preliminary feedstock supply analysis

# PARAMETERS:
# basemap - an object of class SpatialLinesDataFrame to use as road network
# basemap for calculating distance of shortest path.
# start - a numeric vector in which the first element is the longitude and the 
# second element is the latitude of the starting point. Coordinates should
# be geographic (i.e. unprojected) and based on the WGS84 datum. 
# end - a numeric vector in which the first element is the longitude and the 
# second element is the latitude of the end point of the route. Coordinates 
# should be geographic (i.e. unprojected) and based on the WGS84 datum. 


# RETURNS:
# A list of the following objects:
# 1. A data frame containing the total route distance in km and miles
# 2. A SpatialPoints object with two point features: the start and end points of 
# the route provided in the function call. 
# 3. A SpatialLinesDataFrame containing features for all segments of the route 
# and segment lengths (given in both km and miles) in the associated 
# dataframe

#-----------------------------------------------------------------------------#

##### TEMP: SET WD TO THIS SCRIPT LOCATION FOR FUNCTION TESTING ######

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


TLRoute <- function(basemap, start.coords, end.coords) {
  
  ###### LOAD LIBRARIES #######
  packages <- c("ggmap", "raster", "sp", "ggplot2", "rgeos", "rgdal",
                "spatialEco", "geosphere", "shp2graph", "igraph")
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
  library(igraph)
  library(shp2graph)
  library(rgdal)
  
  ###### LOAD COORD REF SYSTEMS ######
  aea.crs <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23.0
                 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 
                 +units=m +no_defs")
  
  wgs84.crs <- crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  
  ###### MANIP DATA ######
  
  orig.bmap <- basemap
  # set up start end point data for cropping basemap
  start <- matrix(start.coords, byrow = T, ncol = 2)
  end <- matrix(end.coords, byrow = T, ncol = 2)
  
  # aggregate start and end points into sptdf
  st.pt <- SpatialPoints(start, proj4string = wgs84.crs)
  end.pt <- SpatialPoints(end, proj4string = wgs84.crs)
  start.end.pts <- spRbind(st.pt, end.pt)
  
  # set initial margin scalar for increasing extent of road net basemap
  marg <- 0.005
  trim <- 0.00075
  
  # define func for cropping basemap
  CropBmap <- function(basemap, st.end.pts, marg) {
    box <- extent(start.end.pts)
    box.long.min <- box[1] + box[1]*marg
    box.long.max <- box[2] - box[2]*marg
    box.lat.min <- box[3] - box[3]*marg
    box.lat.max <- box[4] + box[4]*marg
    box <- c(box.long.min, box.long.max, box.lat.min, box.lat.max)
    box <- extent(box)
    res <- raster::crop(orig.bmap, box)
    return(res)
  }
  
  # crop road net basemap to area encompassing start and end points
  print("Cropping road network to focal area...")
  basemap <- CropBmap(orig.bmap, st.end.pts, marg)
  
  # plot road network + start and end points
  plot(basemap, col = 'grey')
  plot(start.end.pts, add = T, col = 'blue', pch = 16)
  
  # init bool flag for checking whether a valid route is calculated
  valid.rt <- FALSE
  
  # init counter for number of re-crops peformed to circumvent route calc error
  recrops <- 0
  
  # init counter for number of jitters performed
  n.jit <- 0
  
  # re-crop road network and recalculate route until valid route found
  while(valid.rt == F) {
    
      # generate zero vector as placeholder for road line attributes
      cols <- c(1)
      for (i in seq(1, length(names(basemap))-1)){
        cols <- c(cols, 1)
      }
      
      # create matrix with start and end node coords
      start.end.nodes <- matrix(nrow = 1, ncol = 2, as.numeric(start.coords))
      start.end.nodes <- rbind(start.end.nodes, as.numeric(end.coords))
      
      
      # if re-cropping hasn't fixed routing error, jitter start/end pts
      if (recrops >= 5) {
        cat("5 re-crops have been performed, jittering start/end nodes \n")
        
        # jitter the start and end nodes in case that route calc fails with orig
        jit.trim <- 0.01
        jit.val <- 0.03 + n.jit * jit.trim
        
        for (r in 1:2) {
          for (c in 1:2) {
            amt <- sample(c(-jit.val, jit.val), 1)
            start.end.nodes[r, c] <- start.end.nodes[r, c] + amt
          }
        }
        # increase couter for numebr of jitters performed
        n.jit <- n.jit + 1
        cat(sprintf("%s jitters have been performed \n", n.jit))
      }
      
      # integrate bioref node into net
      # creates new node on net for bioref at closest point on nearest edge
      # appends node to end of nodelist 
      net <- points2network(basemap, start.end.nodes, mapping.method = 2, 
                            ELComputed=T, longlat=T, Detailed=F, ea.prop = cols)
      
      # get nodes of road network
      net.nodes <- net[[1]]
      
      # get corresponding node ids of start and end nodes
      cor.ids <- net[[3]]
      
      # get node IDs of start and end points of route
      start.node.id <- cor.ids[1]
      end.node.id <- cor.ids[2]
      
      # check if start and end points are mapped to same node
      if (start.node.id == end.node.id) {
        # calculate crow-flies distance between start and end points
        DIST.KM <- distHaversine(start, end)/1000
        DIST.MI <- DIST.KM*0.621371
        
        # generate route summary table
        rt.summary <- data.frame(DIST.KM, DIST.MI)
        
        # indicates that no primary or secondary roads were in route
        roads.in.rt <- NULL
        
        # create list of results to return
        res.list <- list(rt.summary, start.end.pts, roads.in.rt)
        
        return(res.list)
      }
      
      # get node coords of start and end points of route
      start.node.coords <- net.nodes[length(net.nodes)-1]
      end.node.coords <- net.nodes[length(net.nodes)]
      
      # plot start and end nodes
      coords <- matrix(unlist(start.node.coords), nrow = 1, ncol = 2, byrow = T)
      coords <- rbind(coords, unlist(end.node.coords))
      plot(SpatialPoints(coords, proj4string = wgs84.crs), add = T, col = 'red')
    
    # extract nodelist and edgelist from net object
    nodelist <- net[[1]]
    elist <- net[[2]]
    
    # extract edgeID/roadID key
    road.edge.key <- net[[6]]
    road.edge.key <- road.edge.key[,c("EdgeID", "ID")]
    
    # fetch coords of nodes
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
    graph <- nel2igraph(nodelist, elist, weight = elens, eadf = edges.df, 
                        Directed = F)
    
    # calculate shortest path
    path <- shortest_paths(graph, start.node.id, end.node.id,
                           weights = elens, output = "both")
    
    # extract edge ids of edges in path
    eids <- unlist(path$epath)
    
    # init e.rt.coords.df
    e.rt.coords.df <- data.frame( "eid" = numeric(), 
                                  "e.head" = numeric(), "e.tail" = numeric())
    
    # get coords of start and end nodes of each edge in rt
    for (e in eids) {
      ed <- elist[e,]
      id <- ed[1]
      hd <- ed[2]
      tl <- ed[3]
      
      # get coords of head and tail nodes for edge
      hd.coords <- node.coords[hd,]
      tl.coords <- node.coords[tl,]
      
      ed.row <- c(e, hd.coords, tl.coords)
      
      # bind to df
      e.rt.coords.df <- rbind(e.rt.coords.df, ed.row)
    }
    
    # check whether valid route was calculated
    if (length(names(e.rt.coords.df)) == 5) {
      valid.rt <- TRUE
      
    } else {
      print("No valid route found. Re-cropping roadmap to larger extent...")
      marg <- marg + trim
      print(paste0("Current value of margin: ", marg))
      basemap <- CropBmap(orig.bmap, st.end.pts, marg)
      recrops <- recrops + 1
      cat(sprintf("Re-crops performed: %s \n", recrops))
      
      # plot
      plot(basemap, col = 'grey')
      plot(start.end.pts, add = T, col = 'blue')
    }
  }
  
  # change col names
  names(e.rt.coords.df) <- c("eid", "e.head.long", "e.head.lat", 
                             "e.tail.long", "e.tail.lat")
  
  # init vector to store r.IDs in route
  r.IDs.in.rt <- c(NULL)
  
  # find road SpatialLines in calculated shortest path
  for (r in seq(1, nrow(basemap))) {
    
    # get ID of road
    r.ID <- basemap@lines[[r]]@ID
    
    # calc number of points in polyline
    n.pts <- nrow(basemap@lines[[r]]@Lines[[1]]@coords)
    
    
    # get coords of points that form road polyline
    r.all.pt.coords <- matrix(basemap@lines[[r]]@Lines[[1]]@coords[1:n.pts,],
                              ncol = 2, byrow = F)
    
    if (is.null(r.all.pt.coords)) {
      r.all.pt.coords <- "Null"
    }
    
    # check if road endpoints corresponds with endpoints of an edge in rt
    if (nrow(e.rt.coords.df) == 1) {
      start.int <- 1
      end.int <- 1
    }
    if (nrow(e.rt.coords.df) == 2) {
      start.int <- 1
      end.int <- 2
    }
    if (nrow(e.rt.coords.df) >= 3) {
      start.int <- 2
      end.int <- (nrow(e.rt.coords.df)-1)
    }
    
    for (i in seq(start.int, end.int)) {
      e.coords.v <- unlist(e.rt.coords.df[i, 2:5])
      
      if (is.null(e.coords.v)) {
        e.coords.v <- "None"
      }
      # check if all coords in e.coords.v match points in par road polyline
      check <- TRUE
      
      for (j in seq(1, length(e.coords.v))) {
        if (!(e.coords.v[j] %in% r.all.pt.coords)) {
          check <- FALSE
        }
      }
      if (check == TRUE) {
        r.IDs.in.rt <- c(r.IDs.in.rt, r.ID) 
      }
    }
  }
  
  # subset road network basemap for roads in route
  roads.in.rt <- basemap[r.IDs.in.rt, ]
  
  # subset edges df for first and last edges in route
  start.end.edges <- edges.df[c(eids[1],eids[length(eids)]),]
  
  # create lines for start and end edges
  st.end.edge.nodes <- unlist(c(start.end.edges[1,2:3], start.end.edges[2,2:3]))
  
  # get coords of start/end edge nodes 
  nodes.in.path.coords <- node.coords[st.end.edge.nodes,]
  
  # init list to store lines
  line.ls <- c(NULL)
  
  # generate spatial lines objects from edges 
  for (row in seq(1, nrow(nodes.in.path.coords)-1,2)) {
    mx <- nodes.in.path.coords[(row:(row+1)), ]
    line <- Line(mx)
    line <- Lines(line, ID = as.character(row))
    line.ls <- c(line.ls, line)
  }
  
  # create SpatialLines object with start.end.roads
  start.end.roads <- SpatialLines(line.ls, proj4string = wgs84.crs)
  start.end.roads <- spChFIDs(start.end.roads, c("start", "end"))
  
  # change line FIDs, if necessary to allow rbinding
  if (nrow(roads.in.rt) >= 1) {
    roads.in.rt <- spChFIDs(roads.in.rt, 
                            as.character(seq(1,nrow(roads.in.rt))))
  } 
  
  # bind start.end.roads to roads in route object
  roads.in.rt <- spRbind(roads.in.rt, start.end.roads)
  
  # change spatial ID slots of lines
  new.IDs <- seq(1, length(roads.in.rt))
  roads.in.rt <- spChFIDs(roads.in.rt, as.character(new.IDs))
  
  # calculate road segment lengths in km and miles
  LEG <- seq(1, length(roads.in.rt))
  KM <- SpatialLinesLengths(roads.in.rt, longlat = T)
  MI <- (KM * 0.62)
  
  # create distances df for making SpatialLinesDataFrame
  roads.in.rt.df <- data.frame(LEG, KM, MI)
  roads.in.rt <- SpatialLinesDataFrame(roads.in.rt, data = roads.in.rt.df)
  
  # plot
  plot(roads.in.rt, add = T)
  
  # calculate total distance of route
  DIST.KM <- sum(roads.in.rt$KM, (start.end.edges$length/1000))
  DIST.MI <- sum(roads.in.rt$MI, (start.end.edges$length*0.000621371))
  
  # generate route summary table
  rt.summary <- data.frame(DIST.KM, DIST.MI)
  
  # create list of results to return
  res.list <- list(rt.summary, start.end.pts, roads.in.rt)
  
  return(res.list)
}

###### NOT RUN: FUNCTION TESTING ######

###### LOAD LIBRARIES #######
packages <- c("ggmap", "raster", "sp", "ggplot2", "rgeos",
              "spatialEco", "geosphere", "shp2graph", "igraph")
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
library(igraph)
library(shp2graph)

###### LOAD COORD REF SYSTEMS ######
aea.crs <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23.0
               +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83
               +units=m +no_defs")

wgs84.crs <- crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

####### LOAD DATA ######
biorefs <- readRDS("../clean_binary_data/biorefs.sptdf.RDS")
county.cents <- readRDS("../output/US.wtd.cents.RDS")
roads.data <- readRDS("../clean_binary_data/roads.sldf.RDS")
counties <- readRDS("../clean_binary_data/counties.spdf.RDS")
cents <- readRDS("../output/US.cluster.cents.sp.RDS")
states <-  readRDS("../clean_binary_data/states.spdf.RDS")
state <- states[states$NAME == "North Dakota",]

# crop roads to smaller area
# state.wgs <- spTransform(state, wgs84.crs)
# state.roads <- roads.data

# set start/end points of route
# state.cents <- cents[cents$STATENAME == "MINNESOTA",]
# state.biorefs <- biorefs[biorefs$STATE == "MN",]

# subseet for particular refinery
par.ref <- biorefs[biorefs$RID == "21", ]

# subset centroids for those within 50 mile radius of ref
buff <- gBuffer(par.ref, width = 80468, quadsegs = 100)
cents <- crop(cents, buff)

# set consistent CRS
cents <- spTransform(cents, wgs84.crs)
par.ref <- spTransform(par.ref, wgs84.crs)

# test function
for (c in cents$cid[1:length(cents$cid)]) {
  
  # # TEMP: 
  # c <- "19013.19"
  
  print(paste0("Working on cluster centroid with CID: ", c))
  par.cnty = cents[cents$cid == c, ]
  
  # set params
  basemap <- roads.data
  start.coords <- par.ref@coords
  end.coords <- cents[cents$cid == c, ]@coords
  
  # test function
  test.route <- TLRoute(basemap, start.coords, end.coords)
}

route.summary <- test.route[[1]]
start.end.pts <- test.route[[2]]
route.legs <- test.route[[3]]

# # plotting
# basemap <- crop(basemap, start.end.pts)
# plot(start.end.pts, add = T, col = 'blue', pch = 16)
# plot(route.legs, add =T , col = 'red')
# 
# plot(box, add = T)


# ###### TEST CASE FOR A PARTICULAR REFINERY-CENTROID ROUTING ######
# 
# ####### LOAD DATA ######
# biorefs <- readRDS("../clean_binary_data/biorefs.sptdf.RDS")
# county.cents <- readRDS("../output/US.wtd.cents.RDS")
# roads.data <- readRDS("../clean_binary_data/roads.sldf.RDS")
# counties <- readRDS("../clean_binary_data/counties.spdf.RDS")
# cents <- readRDS("../output/US.wtd.cents.RDS")
# states <-  readRDS("../clean_binary_data/states.spdf.RDS")
# state <- states[states$NAME == "North Dakota",]
# 
# 
# # crop roads to smaller area
# state.wgs <- spTransform(state, wgs84.crs)
# state.roads <- roads.data
# 
# # subset for refinery of interest
# par.ref <- state.biorefs[state.biorefs$RID == "67",]
# 
# # transform to consistent CRS
# cents <- spTransform(cents, wgs84.crs)
# par.ref <- spTransform(par.ref, wgs84.crs)
# 
# 
# # test function
# #for (c in state.cents$NAME) {
# 
# f <- 18179
# 
# print(paste0("working on county: ", f))
# par.cnty = cents[cents$FIPS == f,]
# 
# # set params
# basemap <- roads.data
# start.coords <- par.ref@coords
# end.coords <- par.cnty@coords
# 
# # test function
# test.route <- TLRoute(basemap, start.coords, end.coords)
# #}
# 
# route.summary <- test.route[[1]]
# start.end.pts <- test.route[[2]]
# route.legs <- test.route[[3]]
# 
# # plotting
# basemap <- crop(basemap, start.end.pts)
# plot(start.end.pts, add = T, col = 'blue', pch = 16)
# plot(route.legs, add =T , col = 'red')
# 
# plot(box, add = T)
