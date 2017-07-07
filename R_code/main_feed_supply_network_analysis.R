
#-----------------------------------------------------------------------------#
# main_feed_supply_network_analysis.R

# Type: R analysis script

# AUTHOR:
# Tyler Huntington, 2017
# JBEI Sustainability Research Group
# Project: Feedstock Agnostic Biorefinery Study

# PURPOSE:
# A script to perform a network analysis of feedstock supplies for biorefineries
# given the list of counties within range of each. 


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


###### LOAD LIBRARIES #######
packages <- c("ggmap", "raster", "sp", "ggplot2", "rgeos", "rgdal")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

library(ggplot2)
library(ggmap)
library(raster)
library(sp)
library(rgeos)
library(rgdal)

###### LOAD CLEANED DATA ######

# load billion ton study biomass data
biomass.data <- readRDS("../clean_binary_data/bt_biomass_18_30_40.df.RDS")

# load bioref locations data
biorefs.sptdf <- readRDS("../clean_binary_data/biorefs.sptdf.RDS")

# load county polygons data
counties.spdf <- readRDS("../clean_binary_data/counties.spdf.RDS")

# TODO: load centroids data for all US counties
centroids.data <- readRDS("../output/US.cluster.cents.sp.RDS")


###### LOAD FUNCTIONS ######
source("BtDataSelect_fun.R")
source("SumBioshedFeeds_cids_fun.R")
source("parTLCidsInRange_fun.R")


for (yr in c(2018, 2030, 2040)) {
  for (run.range in c(40, 50, 60)) {
    # define parameters for BT data to use
    year <- yr
    scenario <- "Basecase, all energy crops"
    feeds <- c("herb", "woody", "residues")
    price <- 60  # note: this is delivered price per dt 
    
    # subset BT data
    biomass.df <- BtDataSelect(biomass.data, year, scenario, feeds, price)
    
    # # calc bioshed per refinery from scratch
    # constraint <- "distance"
    # max.dist <- 50
    # catchments <- TLPointsInRange(biorefs.sptdf, centroids.data, 
    #                             constraint = constraint, max.dist = max.dist)
    
    
    # load pre-calculated bioshed data 
    range <- run.range
    range.units <- "mi"
    rid.bioshed.key <- readRDS(paste0("../output/curr_ref_biosheds_",
                                       range, range.units, ".RDS"))
    
    
    # calculate total biomass available to each bioref, given counties in range
    bioref.supplies.sptdf <- SumBioshedFeeds(counties.spdf, 
                                             biorefs.sptdf, 
                                             rid.bioshed.key,
                                             centroids.data,
                                             biomass.df)
    
    # set name for export files
    outfile.name <- paste0("bioref_biosheds_", run.range, "mi_", year)
    
    # export attribute table as .csv
    write.csv(bioref.supplies.sptdf@data, 
              paste0("../output/model_runs/", outfile.name, ".csv"), 
              fileEncoding = "UTF-16LE")
    
    
    # export spdf as shapefile
    writeOGR(bioref.supplies.sptdf,
             dsn = paste0(outfile.name, ".shp"),
             layer = paste0("../output/model_runs/", outfile.name), 
             driver = "ESRI Shapefile")
  }
}













