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
library(iterators)
library(rgeos)
library(doSNOW)


CropCountyRaster <- function(counties.data, raster.path, fips) {
  
  print(paste("Calculating weighted centroids for", fips, sep = ": "))
  
  # crop counties layer to particular state
  county <- subset(counties.data, counties.data$FIPS == fips)
  
  # load raster data
  raster.data <- raster(raster.path)
  
  ###### PREP DATA #######
  
  print("cropping extent of NLCD layer to county...")
  # crop extent of nlcd RasterLayer to extent of US counties layer
  tempfile <- paste("../../../../../../Desktop/lfs_temp/cropped_", 
                    fips, "_raster", sep = "")
  raster::crop(raster.data, county, filename = tempfile, overwrite = T)
  
  # convert to raster
  county.raster <- raster(tempfile)
  
  # re-project mask raster to standardized projection
  proj4string(county.raster) <- crs(county)
  
  # set extent of mask to extent of county polys
  extent(county.raster) <- extent(county)
  
  new.tempfile <- paste0("../../../../../../Desktop/lfs_temp/FIPS_", 
                       fips, "_raster")
  # write out raster
  writeRaster(county.raster, new.tempfile, overwrite = T)
  
  # re-load raster layer into workspace
  county.raster <- raster(new.tempfile)
  
  # convert to matrix
  mx <- as.matrix(county.raster)
  
  # update vals
  mx[mx == 81 | mx == 82] <- 1
  mx[mx != 1] <- 0
  
  # convert to raster
  county.raster <- raster(mx)
  
  # re-project mask raster to standardized projection
  proj4string(county.raster) <- crs(county)
  
  # set extent of mask to extent of county polys
  extent(county.raster) <- extent(county)
  
  # write out raster
  writeRaster(county.raster, new.tempfile, overwrite = T)
  
  # re-load raster
  ras <- raster(new.tempfile)
  
  # convert raster cells to pts
  ras.pts <- rasterToPoints(ras, fun=function(x){x>0})
  
  # export point representation of raster layer
  saveRDS(ras.pts, paste0("../../../../../../Desktop/lfs_temp/",
                          "raster_points/FIPS_", 
                          fips, "_ras_pts.RDS"))
}

###### USAGE: COUNTY BY COUNTY RASTER CROP IO ######
    
###### LOAD DATA ######

# load county boundaries data
counties <- readRDS("../clean_binary_data/counties.spdf.RDS")

# load NLCD raster
raster.path <- (paste0("../../../../../../Desktop/very_large_files/", 
                "nlcd_2011_landcover_2011_edition_2014_10_10.img"))

# Initiate cluster for parallel comp
no_cores <- detectCores() - 1
cl <- makeCluster(no_cores)
registerDoParallel(cl)

# define vector of fips codes to iterate over
fips.codes <- counties$FIPS

#for (county in counties[1:length(counties)]){
foreach (fips = fips.codes[1:length(fips.codes)], 
         .packages = c("raster", "rgeos", "sp")) %dopar% {
           
           
           CropCountyRaster(counties, raster.path, fips)
           
         }

stopCluster(cl)

###### END COUNTY BY COUNTY IO ######
           
           
           
           
           
           
      
        
        
        