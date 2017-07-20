#------------------------------------------------------------------------------#
# calc_us_cluster_cents_IO.R

# AUTHOR:
# Tyler Huntington, 2017

# JBEI Sustainability Team
# Feedstock Agnostic Study
# PI: Corinne Scown PhD

# PURPOSE:
# An analysis script to find the geographic centroids of all US counties 
# weighted by the spatial distribution of crop and pasture land within each 
# county. Weighted centroids will serve as the nodes in a network analysis for
# calculating potential feedstock supplies within range of existing US
# biorefinery locations. 

# OUTPUTS:
# An object of class SpatialPointsDataFrame in which each point 
# represents the weighted centroid of a county in the contiguous US.

#------------------------------------------------------------------------------#

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
library(broom)
library(dplyr)
library(ggplot2)
library(colorspace)
library(sp)
library(maptools)
library(raster)
library(rgdal)
library(spatial)
library(foreach)
library(iterators)
library(doParallel)
library(snow)
library(doSNOW)
library(rgeos)

aea.crs <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23.0 
               +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 
               +units=m +no_defs")

###### LOAD DATA ######

# load county boundaries data
counties <- readRDS("../clean_binary_data/counties.spdf.RDS")

# load NLCD raster
raster.path <- (paste0("../../Desktop/very_large_files/", 
                       "nlcd_2011_landcover_2011_edition_2014_10_10.img"))

# fips <- "06075"

# define vector of fips codes to iterate over
fips.codes <- counties$FIPS

# non-parallel iteration
# for (fips in fips.codes[1:length(fips.codes)]) {  

# parallel comp
# init cluster
no.cores <- detectCores()
cl <- makeCluster(no.cores, type = "SOCK", outfile="")
registerDoSNOW(cl)

US.cluster.cents <-
  foreach(fips = fips.codes[1:length(fips.codes)],
                            .combine = "rbind",
                            .packages = c("broom", "dplyr", "sp",
                                          "raster", "maptools", 
                                          "rgeos")) %dopar% {
                                            
                             
                                            
  cat(sprintf("Working on FIPS: %s", fips))       
                                           
  # load point representation of ag raster for county
  county.pts <- readRDS(paste0("../../Desktop/lfs_temp/",
                               "raster_points/FIPS_", 
                               fips, "_ras_pts.RDS"))
  
  if (nrow(county.pts) < 20) {
    if (nrow(county.pts) == 0) {
      county.poly <- counties[counties$FIPS == fips, ]
      cent <- gCentroid(county.poly)
      x1 <- cent@coords[1]
      x2 <- cent@coords[2]
    } else {
      x1 <- mean(county.pts[1])
      x2 <- mean(county.pts[2])
    }
    
    size <- nrow(county.pts)
    withinss <- 0
    cluster <- 1
    cid <- paste0(fips, ".1")
    
    clusters <- data.frame(x1, x2, size, withinss, cluster, fips, cid)
    
    clusters
  
  } else {
  
  # elim layer val column from points matrix
  county.pts <- county.pts[,1:2]
  
  cat("\n Performing k-means cluster analysis...") 
  # perform K-Means cluster analysis
  cluster.info <- kmeans(county.pts, 20)
  
  cat("\n cbinding cluster groups to points data")
  # cbind cluster groups to points data
  county.pts <- broom::augment(cluster.info, county.pts)
  
  cat("\n getting cluster summary stats")
  # get cluster summary stats
  clusters <- tidy(cluster.info)
  
  cat("\n cbinding fips and cid columns")
  # cbind fips and cid (unqique cluster id) columns
  clusters$fips <- fips
  clusters$cid <- lapply(clusters$cluster, function(x) paste0(fips, ".", x))
  
  clusters
  }
}

# stop cluster
stopCluster(cl)

# problematic fips
# 41019

# convert cluster centers in spatial points
US.cluster.cents.coords <- US.cluster.cents[ ,1:2]
US.cluster.cents.sp <- SpatialPoints(US.cluster.cents.coords, 
                                  proj4string = aea.crs)
cluster.data <- US.cluster.cents[ ,3:(ncol(US.cluster.cents))]
US.cluster.cents.sp <- sp::SpatialPointsDataFrame(US.cluster.cents.sp,  
                                                  cluster.data,
                                                  proj4string = aea.crs)

# export all US cluster cents in binary data file
saveRDS(US.cluster.cents.sp, 
        paste0("../output/US.cluster.cents.sp.RDS"))







# ###### PLOTTING ######
# plot(US.cluster.cents[,1], US.cluster.cents[,2], pch = 16)
# 
# plot(cluster.cents.sp, cex = 1.5, add = T)
# 
# SpatialPoints(county.pts[,1:2])









