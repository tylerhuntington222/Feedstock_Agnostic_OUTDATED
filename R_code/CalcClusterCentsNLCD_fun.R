#------------------------------------------------------------------------------#
# CalcClusterCents_fun.R

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

# ###### BASIC FUNCTIONS ######
# 
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

CalcCLusterCentsNLCD <- function (counties.data, fips.codes) {

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
    require(doParallel)
    require(snow)
    require(doSNOW)
    require(rgeos)
    
    aea.crs <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23.0 
                 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 
                 +units=m +no_defs")
    
    
    # non-parallel iteration
    # for (fips in fips.codes) {  
    no.cores <- detectCores() - 1
    cl <- makeCluster(no.cores, type = "SOCK", outfile = "")
    registerDoSNOW(cl)
    
    US.cluster.cents <-
    foreach(fips = fips.codes,
            .combine = "rbind",
            .packages = c("broom", "dplyr", "sp",
                          "raster", "maptools", 
                          "rgeos")) %dopar% {
    
    
    
    cat(sprintf("Working on FIPS: %s", fips))       
    
    # load point representation of ag raster for county
    county.pts <- readRDS(paste0("../output/bin/FIPS_", 
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
      
      
      # export county cluster centroids
      crop <- gsub(" ", "_", crop)
      saveRDS(clusters, 
              paste0("../output/bin/FIPS_",
                     fips, "_EnergyCrops_cluster.cents.RDS"))
      
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
      
      
      # export county cluster centroids
      crop <- gsub(" ", "_", crop)
      saveRDS(clusters, 
              paste0("../output/bin/FIPS_",
                     fips, "_EnergyCrops_cluster.cents.RDS"))
      
      clusters
      
    }
  }
}

# cancel parallel backend
stopCluster(cl)










