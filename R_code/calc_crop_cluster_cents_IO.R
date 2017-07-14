#------------------------------------------------------------------------------#
# calc_crop_cluster_cents_IO.R

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

# # a basic function to get the filepath of the current script
# csf <- function() {
#   # install packages
#   #install.packages("rstudioapi")
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
# ###### SET WORKING DIRECTORY ######
# this.dir <- dirname(csf())
# setwd(this.dir)
# rm(list=ls())



###### LOAD LIBRARIES ######
require(broom)
require(dplyr)
require(ggplot2)
require(colorspace)
require(sp)
require(maptools)
require(raster)
require(rgdal)
require(spatial)
require(foreach)
require(iterators)
require(parallel) 
require(parallel)
require(snow)
require(doSNOW)
require(rgeos)

aea.crs <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23.0 
               +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 
               +units=m +no_defs")

###### LOAD DATA ######


# Set params
crop <- "Corn"
raster.path <- "../../Desktop/very_large_files//cdl/cdl_2016_30m.img"
counties <- readRDS("../clean_binary_data/counties.spdf.RDS")
fips.codes <- as.character(counties$FIPS)

# TEMP: determine which fips havent been done
done <- list.files(paste0("../output/county_kmeans_",crop))
done <- sapply(done, function(x) {substr(x, 6, stop = 10)}, simplify = T)
done <- unname(done, force= F)
not.done.fips <- fips.codes[!(fips.codes %in% done)]


# load raster data
raster.data <- raster(raster.path)

###### ORG DATA ######

# get df of cell value codes
ras.vals.df <- raster.data@data@attributes[[1]]

  
# find raster vals for cells with dual production of target crop + other
all.names <- grep(crop, ras.vals.df$Class_Names, value = T)

# include abbreviations for wheat
if (crop == "Wheat") {
  all.names <- c(all.names, grep("Wht", ras.vals.df$Class_Names, value = T))
}
dbl.names <- grep("Dbl", all.names, value = T)
dbl.vals <- ras.vals.df[ras.vals.df$Class_Names %in% dbl.names, "ID"]

# determine raster vals corresponding to land dedicated to selected crop 
ded.names <- all.names[!(all.names %in% dbl.names)]
ded.vals <- ras.vals.df[ras.vals.df$Class_Names %in% ded.names, "ID"]
  

# define vector of fips codes to iterate over
fips.codes <- as.character(counties$FIPS)

# non-parallel iteration
# for (fips in fips.codes[1:length(fips.codes)]) {  

# parallel comp

# init cluster
no.cores <- detectCores()
cl <- makeCluster(no.cores, type = "SOCK", outfile="log.txt")
registerDoSNOW(cl)

US.cluster.cents <-
  foreach(fips = not.done.fips,
          .combine = "rbind",
          .packages = c("broom", "dplyr", "sp",
                        "raster", "maptools", 
                        "rgeos")) %dopar% {
                          
  cat(sprintf("Working on FIPS: %s", fips))       
  
  # load point representation of ag raster for county
  county.pts <- readRDS(paste0("../../Desktop/lfs_temp/",
                               "raster_points_non_sp/FIPS_", 
                               fips, "_ras_pts.RDS"))
  
  
  ## TODO make this a function
  # a function to update raster vals of cdl points layer
  RecodeVals <- function(val, ded.vals, dbl.vals) {
    
    if (val %in% ded.vals) {
      return(2)
    } else if (val %in% dbl.vals) {
      return(1)
    } else {
      return(0)
    }
  }
  RecodeVals <- Vectorize(RecodeVals, vectorize.args = c("val"))
  county.pts[,3] <- RecodeVals(county.pts[,3], ded.vals, dbl.vals)
  
  # drop points with val of zero
  county.pts <- county.pts[which(county.pts[,3]!=0),]
  county.pts <- matrix(county.pts, ncol = 3)
  
  # account for weights by duplicating dedicated crop production points
  DuplicateDedPoints <- function(county.pts) {
    ded.rows <- county.pts[which(county.pts[,3] == 2),]
    county.pts <- rbind(county.pts, ded.rows)
    return(county.pts)
  }
  county.pts <- DuplicateDedPoints(county.pts)
  
  if (nrow(unique(county.pts)) <= 20) {
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
    clusters$cluster <- as.character(clusters$cluster)
    clusters$cid <- lapply(clusters$cluster, 
                           function(x) {paste0(fips, ".", x)})
    
  }
  
  # export county cluster centroids
  crop <- gsub(" ", "_", crop)
  saveRDS(clusters, 
          paste0("../output/county_kmeans_", crop, "/FIPS_",
                 fips, "_", crop, "_cluster.cents.RDS"))
  
  print(paste0("Exported RDS data file for FIPS: ", fips))
  
  # return cluster df
  clusters
}


# # convert cluster centers in spatial points
# US.cluster.cents.coords <- US.cluster.cents[ ,1:2]
# US.cluster.cents.sp <- SpatialPoints(US.cluster.cents.coords, 
#                                      proj4string = aea.crs)
# cluster.data <- US.cluster.cents[ ,3:(ncol(US.cluster.cents))]
# US.cluster.cents.sp <- sp::SpatialPointsDataFrame(US.cluster.cents.sp,  
#                                                   cluster.data,
#                                                   proj4string = aea.crs)
# 
# # export all US cluster cents in binary data file
# crop <- gsub(" ", "_", crop)
# saveRDS(US.cluster.cents.sp, 
#         paste0("../output/US", crop, 
#                "cluster.cents.sp.RDS"))


# stop parallel cluster
stopCluster(cl) 










