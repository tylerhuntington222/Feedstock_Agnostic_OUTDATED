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


# start timer
system.time({
  
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
  
  
  states <- c( "IDAHO", "IOWA", "LOUISIANA", "NEW YORK", "NORTH CAROLINA",
               "OREGON", "SOUTH CAROLINA", "UTAH", "WASHINGTON", "WYOMING",
               "TEXAS", "CALIFORNIA",  "MONTANA", "ARIZONA", "NEW MEXICO", 
               "NEVADA", "COLORADO")
  
  counties.spdf <- subset(counties.spdf, counties.spdf$STATENAME %in% states)
  
  
  # big.states <- c("TEXAS", "CALIFORNIA",  "MONTANA", "ARIZONA", "NEW MEXICO", "NEVADA", "COLORADO")
  
  #states <- "NEW JERSEY"
  
  # create vector of counties to iterate over
  all.fips <- unique(counties.spdf$FIPS)
  all.fips <- as.character(all.fips)
  
  # determine which all.fips have already been done
  done.files <- list.files("../output/counties")
  
  # define function to get fips code of county out of filename
  GetFips <- function(x) {
    scores <- which(strsplit(x, "")[[1]] == "_")
    s1 <- scores[1]
    s2 <- scores[2]
    fips <- substr(x, start = s1+1, stop = s2-1)
    return(fips)
  }
  
  done.fips <- lapply(done.files, FUN = GetFips)
  done.fips <- unlist(done.fips)
  done.fips <- done.fips[!is.na(done.fips)]
  
  # find not.done.counties fips codes
  key <- !(all.fips %in% done.fips)
  not.done.fips <- all.fips[key]
  
  issue.fips <- c("36061", "06075","32013")
  
  # TEMP
  # not.done.fips <- "36061"
  not.done.fips <- subset(not.done.fips, !(not.done.fips %in% issue.fips))

  # Initiate cluster for parallel comp
  no_cores <- detectCores() - 1
  cl <- makeCluster(no_cores)
  registerDoParallel(cl)
  
  #for (county in counties[1:length(counties)]){
  foreach (fips = not.done.fips[1:length(not.done.fips)], .packages = "raster", "rgeos", "sp") %dopar% {
    
    print(paste("Calculating weighted centroids for", fips, sep = ": "))
    # rop counties layer to particular state
    s.counties.spdf <- subset(counties.spdf, counties.spdf$FIPS == fips)
    
    ###### PREP DATA #######
    
    print("cropping extent of NLCD layer to county...")
    # crop extent of nlcd RasterLayer to extent of US counties layer
    tempfile <- paste("../../../../../../Desktop/lfs_temp/cropped_", 
                      fips, "_raster", sep = "")
    crop(nlcd, s.counties.spdf, filename = tempfile, overwrite = T)
    
    # re-load raster layer into workspace
    county.nlcd <- raster(tempfile)
    
    # re-project mask raster to standardized projection
    proj4string(county.nlcd) <- crs(s.counties.spdf)
    
    # set extent of mask to extent of county polys
    extent(county.nlcd) <- extent(s.counties.spdf)
    
    if (is.null(county.nlcd[county.nlcd == 81 | county.nlcd == 82])) {
      # ###### COMPUTE TRUE CENTROIDS ######
      cntrs.sptdf <- rgeos::gCentroid(s.counties.spdf, byid = T)
    
      } else {
      
      print("updating vals of raster layer...")
      # make mask raster with binary vals based on crop/pasture (1) or other (0)
      UpdateVals <- function(x){
        x[x==81 | x == 82] <- 1
        x[x != 1] <- 0
        return(x)
      }
      
      calc(county.nlcd, UpdateVals, filename = tempfile, overwrite = T)
      
      ###### COMPUTE WEIGHTED CENTROIDS ######
      cntrs.sptdf <- CalcWeightedCentroids(s.counties.spdf, tempfile)
      
      # # export result as binary data file in output directory
      # saveRDS(cntrs.sptdf, "../output/wtd_cntroids.sptdf.RDS")
      
    }
    
    # saveRDS(cntrs.sptdf, "../output/wtd_county_cntroids.sptdf.RDS")
    saveRDS(cntrs.sptdf, paste("../../../../../../Desktop/lfs_temp/fips_", 
                               fips, "_wtd_cntroid.sptdf.RDS", sep = ""))  
  }
  
  stopCluster(cl)

  
}) # end timer


# # TEMP: Plots for testing and checking analysis
# # plot(s.counties.spdf)
# plot(mask)
# plot(s.counties.spdf, add = T)
# plot(cntrs.sptdf, add = T, col = 'red', cex = 1, pch = 16)
# plot(t.cents, add = T, col = 'blue', pch = 4, cex = 1)

#





