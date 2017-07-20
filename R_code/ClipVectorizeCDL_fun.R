
#------------------------------------------------------------------------------#
# CropAndVectorizeRaster_fun.R

# AUTHOR:
# Tyler Huntington, 2017

# JBEI Sustainability Team
# Feedstock Agnostic Study
# PI: Corinne Scown PhD

# PURPOSE:
# An function to crop a raster to a smaller extent, vectorize it to a point 
# layer (in which the centroid of each cell is represented by a point) and 
# output the spatial points object as an R binary data file.

# OUTPUTS:
# An object of class SpatialPointsDataFrame in which each point 
# represents the centroid of a cell in the input raster layer

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
require(raster)
require(spdep)
require(maptools)
require(rgdal)
require(plyr)
require(geosphere)
require(raster)
require(spdep)
require(maptools)
require(rgdal)
require(plyr)
require(foreach)
require(parallel)
require(doParallel)
require(iterators)
require(doSNOW)


ClipVectorizeCDL <- function(counties.data, raster.path, fips) {
  
  print(paste("Calculating weighted centroids for", fips, sep = ": "))
  
  # crop counties layer to particular county
  county <- subset(counties.data, counties.data$FIPS == fips)
  
  # load raster data
  raster.data <- raster(raster.path)
  
  # get df of cell value codes
  ras.vals.df <- raster.data@data@attributes[[1]]
  
  ###### PREP DATA #######
  
  print("cropping extent of NLCD layer to county...")
  # crop extent of nlcd RasterLayer to extent of US counties layer
  tempfile <- paste0("../../Desktop/lfs_temp/cdl/cropped_", 
                     fips, "_cdl_raster")
  
  raster::crop(raster.data, county, filename = tempfile, overwrite = T)
  
  # convert to raster
  county.raster <- raster(tempfile)
  
  # re-project mask raster to standardized projection
  proj4string(county.raster) <- crs(county)
  
  # set extent of mask to extent of county polys
  extent(county.raster) <- extent(county)
  
  # convert to matrix
  mx <- as.matrix(county.raster)
  
  # convert to raster
  county.raster <- raster(mx)
  
  # re-project mask raster to standardized projection
  proj4string(county.raster) <- crs(county)
  
  # set extent of mask to extent of county polys
  extent(county.raster) <- extent(county)
  
  # convert raster cells to pts
  ras.pts <- rasterToPoints(county.raster, fun=function(x){x>0}, spatial = F)
  
  # export point representation of raster layer
  saveRDS(ras.pts, paste0("../../Desktop/lfs_temp/",
                          "raster_points_non_sp/FIPS_", 
                          fips, "_ras_pts.RDS"))
} 





###### USAGE: COUNTY BY COUNTY RASTER CROP IO ######

###### LOAD DATA ######

# load county boundaries data
counties <- readRDS("../clean_binary_data/counties.spdf.RDS")
counties$FIPS <- as.character(counties$FIPS)

# load NLCD raster
raster.path <- (paste0("../../Desktop/very_large_files/", 
                       "/cdl/cdl_2016_30m.img"))

# Initiate cluster for parallel comp
# init cluster
no.cores <- detectCores() - 1
cl <- makeCluster(no.cores, type="SOCK", outfile="../log.txt")
registerDoSNOW(cl)

# define vector of fips codes to iterate over
fips.codes <- counties$FIPS

#TEMP:
#fips.codes <- rem

#for (county in counties[1:length(counties)]){
foreach(fips = fips.codes[1:length(fips.codes)], 
        .packages = c("raster", "sp")) %dopar% {
          
          
          CropAndVectorizeRaster(counties, raster.path, fips)
          
        }

stopCluster(cl)

###### END COUNTY BY COUNTY IO ######

all <- as.character(unique(counties$FIPS))

done <- list.files("../../Desktop/lfs_temp/raster_points/")

get.fip <- function (x) {substr(x, 6, 10)}

done <- unlist(llply(done, get.fip))

rem <- which(!(all %in% done))

rem <- all[rem]

length(rem)

