#-----------------------------------------------------------------------------#
# main_analysis.R
# Type: R analysis script

# AUTHOR:
# Tyler Huntington, 2017
# JBEI Sustainability Research Group
# Project: Feedstock Agnostic Biorefinery Study

# PURPOSE:
# A script to collate the county-level kmeans cluster centroids for production
# of a particular crop from separate binary data files (one for each county).


# SIDE-EFFECTS:
# outputs a binary data file (.RDS) to the 'output' directory which contains an
# object of class SpatialPointsData frame containing k-means cluster centroids
# for the specified crop across the continental US. 

#-----------------------------------------------------------------------------#

# ##### BASIC FUNCTIONS ######
# 
# # a basic function to get the filepath of the current script
# csf <- function() {
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

###### LOAD LIBRARIES #######
source("install.required.packages.R")

require(raster)
require(sp)
require(rgeos)
require(rgdal)
require(plyr)
require(openxlsx)
require(doSNOW)
require(doParallel)
require(snow)
require(iterators)
require(foreach)
require(parallel)
require(stats)

# load required functions
source("CropAndVectorieRaster_fun.R")
source("ClipMaskVectorizeNLCD_fun.R")
source("GetCropList_fun.R")
source("ClipMaskVectorizeNLCD_fun.R")
source("CalcClusterCentsNLCD_fun.R")
source("CalcBiosheds_fun.R")
source("CollateCountyClusterCents_fun.R")
source("CollateBiosheds_fun.R")
source("ClipVectorizeCDL_fun.R")
source("CalcClusterCentsCDL_fun.R")
source("MakeBioshedDF_fun.R")


# load raw data
source("load.R")

# clean raw data
source("clean.R")

# load cleaned binary data
source("load.cleaned.binary.R")

#-----------------------------------------------------------------------------#
### Set model parameters

# choose feedstocks to include in analysis
# note: these must match exactly as named the in billion ton biomass datafile
feedstocks <- c("Corn stover", "Wheat straw", "Sorghum", 
                "Switchgrass", "Miscanthus", "Oats straw", "Sorghum stubble")

# set max drive distance constraints (in miles) for calculating biosheds
ranges.to.do <- c(40, 50, 60)

# define years to run analysis for
# note: these must correspond to years in Year column of
# billion ton biomass datafile
years <- c(2018, 2030, 2040)  

# set prices per dt of biomass to include in analysis
# note: these must correspond to price levels in Biomass.price column of
# billion ton biomass datafile 
prices <- c(40, 50, 60) 

# define Billion Ton scenarios to run 
# note: these must match exactly the names in scenario column of billion 
# ton biomass datafile
scenarios <- c("Basecase, all energy crops", 
               "2% yield inc.", "3% yield inc.", "4% yield inc.")


#-----------------------------------------------------------------------------#
### Initialize parallel backend
no.cores <- (detectCores() - 1)
cl <- makeCluster(no.cores, type = "SOCK", outfile="")
registerDoSNOW(cl)

#-----------------------------------------------------------------------------#
### Run analysis and generate intermediate data products

# determine crop list from feedstocks selected
crops.to.do <- GetCropList(feedstocks)

# generate list of all US county FIPs (or replace with list of FIPs to analyze)
fips.to.do <- unique(counties.spdf$FIPS)

# vectorize county level rasters of crop/pasture land from NLCD layer
ncld.raster.path <- ("../clean_binary_data/nlcd/nlcd_2011_30m.img")

foreach (fips = fips.to.do) %dopar% {
  ClipMaskVectorizeNLCD(counties.spdf, nlcd.raster.path, fips)
}

# find k-means clusters of crop/pasture land (potential energy crop land)
CalcClusterCentsNLCD(counties.spdf, fips.to.do)

# aggregate all US county cluster cents into single file
CollateCountyClusterCents(crop = "EnergyCrops", fips.to.do)


for (range in ranges.to.do) {
  # calculate dedicated energy crop biosheds based on crop/pasture kmeans cents
  CalcBiosheds(biorefs.sptdf, crop = "EnergyCrops", 
                     edges.data = roads.sldf, max.dist = range)
  
  # collate biosheds into single file
  CollateBiosheds(biorefs.sptdf, crop = "EnergyCrops", range)
}


### Crop residues
cdl.raster.path <- ("../clean_binary_data/nlcd/cdl_2011_30m.img")


for (crop in crops.to.do) {
  
  # vectorize county level rasters for select crops from CDL layer
  ClipVectorizeCDL(counties.spdf, cdl.raster.path, fips = fips.to.do)
  
  # calculate cluster centroids
  CalcClusterCentsCDL(counties.spdf, cdl.raster.path, crop, fips.codes)
  
  # collate clusters
  CollateCountyClusterCents(crop, fips.to.do)

}

# calculate residue feedstock biosheds
for (range in ranges.to.do) {
  
  CalcBiosheds(biorefs.sptdf, crop, edges.data = roads.sldf, max.dist = range)
  
  # collate biosheds into single file
  CollateBiosheds(biorefs.sptdf, crop, range)
}

#-----------------------------------------------------------------------------#
### Generate final results dataframe

results.df <- MakeBioshedDF(bt_all_crops.df, counties.spdf, biorefs.sptdf, 
                            feedstocks, years, scenarios, prices, ranges)



              
              


