#------------------------------------------------------------------------------#
# weighted_centroids_analysis.R

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

###### SOURCE LOCAL FUNCTIONS #######
source("CalcWeightedCentroids_fun.R")


# ###### INSTALL PACKAGES IF NECCESSARY ######
# install.packages("raster")
# install.packages("spdep")
# install.packages("maptools")
# install.packages("rgdal")
# install.packages("plyr")
# install.packages("geosphere")
# install.packages("rgeos")
# install.packages("raster")
# install.packages("spdep")
# install.packages("maptools")
# install.packages("rgdal")
# install.packages("plyr")
# 

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
library(foreach)
library(parallel)
library(doParallel)
library(rgeos)
  
###### LOAD DATA ######

# load county boundaries data
counties.spdf <- readRDS("../clean_binary_data/counties.spdf.RDS")

# load NLCD raster
nlcd <- 
  raster("../../../../../../Desktop/very_large_files/nlcd_2011_landcover_2011_edition_2014_10_10.img")

# # calculate weighted centroids per county, iterating over states
# 
# states <- c("NEW JERSEY", "ALABAMA", "ARIZONA", "ARKANSAS", "CALIFORNIA", "COLORADO", "CONNECTICUT",
#             "GEORGIA", "IDAHO", "IOWA", "LOUISIANA", "MONTANA", "NEVADA",
#             "NEW MEXICO", "NEW YORK", "NORTH CAROLINA",
#             "OREGON", "SOUTH CAROLINA", "TEXAS", "UTAH", "WASHINGTON", "WYOMING")
# 
# states <- "CONNECTICUT"


# states <- c( "IDAHO", "IOWA", "LOUISIANA", "NEW YORK", "NORTH CAROLINA",
#              "OREGON", "SOUTH CAROLINA", "UTAH", "WASHINGTON", "WYOMING",
#              "TEXAS", "CALIFORNIA",  "MONTANA", "ARIZONA", "NEW MEXICO", 
#              "NEVADA", "COLORADO")
# 
# counties.spdf <- subset(counties.spdf, counties.spdf$STATENAME %in% states)
# 
# 
# # big.states <- c("TEXAS", "CALIFORNIA",  "MONTANA", "ARIZONA", "NEW MEXICO", "NEVADA", "COLORADO")
# 
# #states <- "NEW JERSEY"
# 
# # create vector of counties to iterate over
# all.fips <- unique(counties.spdf$FIPS)
# all.fips <- as.character(all.fips)
# 
# # determine which all.fips have already been done
# done.files <- list.files("../output/counties")
# 
# # define function to get fips code of county out of filename
# GetFips <- function(x) {
#   scores <- which(strsplit(x, "")[[1]] == "_")
#   s1 <- scores[1]
#   s2 <- scores[2]
#   fips <- substr(x, start = s1+1, stop = s2-1)
#   return(fips)
# }
# 
# done.fips <- lapply(done.files, FUN = GetFips)
# done.fips <- unlist(done.fips)
# done.fips <- done.fips[!is.na(done.fips)]
# 
# # find not.done.counties fips codes
# key <- !(all.fips %in% done.fips)
# not.done.fips <- all.fips[key]
# 
# issue.fips <- c("32013", "32007")

# TEMP
# not.done.fips <- "36061"
#not.done.fips <- subset(not.done.fips, !(not.done.fips %in% issue.fips))

not.done.fips <- counties.spdf[counties.spdf$STATENAME == "VIRGINIA",]$FIPS

# #Initiate cluster for parallel comp
# no_cores <- detectCores() - 1
# cl <- makeCluster(no_cores)
# registerDoParallel(cl)
# # parallel
# foreach (fips = not.done.fips[1:length(not.done.fips)],
#          .packages = 'raster', 'rgeos') %dopar% {


# non parallel        
for (fips in not.done.fips) {
  
  
  #fips <- "22071"
  
  print(paste("Calculating weighted centroids for", fips, sep = ": "))
  # # rop counties layer to particular state
  s.counties.spdf <- subset(counties.spdf, counties.spdf$FIPS == fips)
  # 
  # ###### PREP DATA #######
  # 
  # print("cropping extent of NLCD layer to county...")
  # # crop extent of nlcd RasterLayer to extent of US counties layer
  tempfile <- paste("../../../../../../Desktop/lfs_temp/cropped_",
                    fips, "_raster_v2", sep = "")
  
  crop(nlcd, s.counties.spdf, filename = tempfile, overwrite = T)
 
  # re-load raster layer into workspace
  county.nlcd <- raster(tempfile)
  
  # convert to matrix
  mx <- as.matrix(county.nlcd)
  
  # update vals
  mx[mx == 81 | mx == 82] <- 1
  mx[mx != 1] <- 0
  
  # convert to raster
  county.nlcd <- raster(mx)
  
  # re-project mask raster to standardized projection
  proj4string(county.nlcd) <- crs(s.counties.spdf)
  
  # set extent of mask to extent of county polys
  extent(county.nlcd) <- extent(s.counties.spdf)
  
  # write out raster
  writeRaster(county.nlcd, tempfile, overwrite = T)
  
  # re-load raster
  ras <- raster(tempfile)
  
  # convert raster cells to pts
  ras.pts <- rasterToPoints(ras, fun=function(x){x>0})
  
  # check if any points are ag/pasture
  if (nrow(ras.pts) == 0) {
    
    cent <- gCentroid(s.counties.spdf)
    
    # extract vals of containing polys 
    cent.data <- extract(s.counties.spdf, cent)
    
    # convert to SPTDF
    cent.sptdf <- SpatialPointsDataFrame(cent@coords, cent.data)
    
    # subset DF for relevant cols
    cent.sptdf <- 
      subset(cent.sptdf, select = c("NAME", "FIPS", "STATEABBREV", "STATENAME"))
    
    # re-project mask raster to standardized projection
    proj4string(cent.sptdf) <- crs(s.counties.spdf)
    
    # export binary data file containing centroid
    saveRDS(cent.sptdf, paste("../output/counties/fips_", 
                              fips, "_wtd_cntroid.sptdf.RDS", sep = ""))
    
  } else {
    ras.pts <- SpatialPoints(ras.pts)
    
    # re-project mask raster to standardized projection
    proj4string(ras.pts) <- crs(s.counties.spdf)
    
    # determine points in county poly
    in.pts <- raster::intersect(ras.pts, s.counties.spdf)
      
    # in case of no points in county
    if (nrow(in.pts) == 0) {
      cent <- gCentroid(s.counties.spdf)
      
      # extract vals of containing polys 
      cent.data <- extract(s.counties.spdf, cent)
      
      # convert to SPTDF
      cent.sptdf <- SpatialPointsDataFrame(cent@coords, cent.data)
      
      # subset DF for relevant cols
      cent.sptdf <- 
        subset(cent.sptdf, select = c("NAME", "FIPS", "STATEABBREV", "STATENAME"))
      
      # re-project mask raster to standardized projection
      proj4string(cent.sptdf) <- crs(s.counties.spdf)
      
      # export binary data file containing centroid
      saveRDS(cent.sptdf, paste("../output/counties/fips_", 
                                fips, "_wtd_cntroid.sptdf.RDS", sep = ""))
    } else {
    
      # create data frame of points in county poly
      df <- data.frame(in.pts@coords)
      
      # calculate mean xy coords of points
      mean.x <- mean(na.omit(df$x))
      mean.y <- mean(na.omit(df$y))
      cent.coords <- c(mean.x, mean.y)
      cent.mx <- matrix(cent.coords, ncol = 2, nrow = 1)
      
      # create SpatialPoints object for weighted centroid
      cent.spt <- SpatialPoints(cent.mx, proj4string = crs(s.counties.spdf))
      
      # extract vals of containing polys 
      cent.data <- s.counties.spdf@data
      
      # convert to SPTDF
      cent.sptdf <- SpatialPointsDataFrame(cent.mx, cent.data)
      
      # subset DF for relevant cols
      cent.sptdf <- 
        subset(cent.sptdf, select = c("NAME", "FIPS", "STATEABBREV", "STATENAME"))
      
      # re-project mask raster to standardized projection
      proj4string(cent.sptdf) <- crs(s.counties.spdf)
      
      # export binary data file containing centroid
      saveRDS(cent.sptdf, paste("../output/counties/fips_", 
                                fips, "_wtd_cntroid.sptdf.RDS", sep = ""))
    }
  }
  
}



# plot(s.counties.spdf, add = F)
# plot(cent, add = T)
# plot(sac.ex.cent, add = T, col = 'red')

