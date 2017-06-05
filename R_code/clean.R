#-----------------------------------------------------------------------------#
# clean.R
# Type: R cleaning script

# AUTHOR:
# Tyler Huntington, 2017
# JBEI Sustainability Research Group
# Project: Feedstock Agnostic Biorefinery Study

# PURPOSE:
# A script to clean all the raw binary data for the Feedstock Agnostic analysis


# SIDE-EFFECTS:
# exports clean binary data files to the clean_binary_data projec directory

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


###### LOAD LIBRARIES ######

library(ggmap)
library(sp)
library(raster)
library(ggmap)
library(maptools)
library(rgdal)

###### SET WORKING DIRECTORY ######
this.dir <- dirname(csf())
setwd(this.dir)
rm(list=ls())


###### DATA FORMATTING SETTINGS ######

# define standardized CRS for spatial data
  aea.crs <- CRS(paste("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23.0", 
               "+lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"))


###### CLEAN BIOREFINERIES DATA ######

  # load raw binary data
  biorefs.df <- readRDS("../raw_binary_data/raw_biorefs.df.RDS")
  
  # import geocoding func
  source("geocode_refineries_func.R")
  
  # geocode bioref locations 
  biorefs.sptdf <- GeocodeLocations(biorefs.df)
  
  saveRDS(biorefs.sptdf, "../clean_binary_data/biorefs.sptdf.RDS")


###### US COUNTY BOUNDARIES DATA ######
  
  # load raw binary data for counties
  counties.spdf <- readRDS("../raw_binary_data/raw_county_bounds.spdf.RDS")
  
  # set counties to standardized CRS
  counties.spdf <- spTransform(counties.spdf, aea.crs)
  
  # load state fips key
  fips.df <- readRDS("../raw_binary_data/raw_state_fips_key.df.RDS")
  
  # add state abbrevs to counties df
  counties.spdf <- sp::merge(counties.spdf, fips.df, by = "STATEFP")

  # subset for continental US counties only
  non_cont_states <- c("02", "15", "72") 
  counties.spdf <- subset(counties.spdf, 
                               !(paste(counties.spdf$STATEFP) 
                                 %in% non_cont_states))
  
  # change GEOID column to FIPS code to match BT dataset
  names(counties.spdf@data)[which(names(counties.spdf@data) == 
          "GEOID")] <- "FIPS"

  # export clean binary data
  saveRDS(counties.spdf, "../clean_binary_data/counties.spdf.RDS")


###### US STATES BOUNDARIES ######

  # load raw binary data
  states.spdf <- readRDS("../raw_binary_data/raw_county_bounds.spdf.RDS")
  
  # set counties to standardized CRS
  states.spdf <- spTransform(states.spdf, aea.crs)
  
  # export clean binary data
  saveRDS(states.spdf, "../clean_binary_data/states.spdf.RDS")


###### US ROAD NETWORK DATA ######
  
  # load raw binary data
  roads.sldf <- readRDS("../raw_binary_data/raw_roads.sldf.RDS")
  
  # export clean binary data
  saveRDS(roads.sldf, "../clean_binary_data/roads.sldf.RDS")


###### BILLION TON STUDY DATA ######

  # load raw binary data
  bt_all_crops.df <-
    readRDS("../raw_binary_data/raw_bt_all_crops_18_30_40.df.RDS")

  # change fips column name to FIPS to match counties baselayer
  names(bt_all_crops.df)[which(names(bt_all_crops.df) == "fips")] <-  "FIPS"

  # export cleaned bt data as binary
  saveRDS(bt_all_crops.df, "../clean_binary_data/bt_biomass_18_30_40.df.RDS")
  
  
###### NLCD DATA ######
  
  # load raw binary data
  nlcd.ras <-
    readRDS("../raw_binary_data/raw_nlcd.ras.RDS")
  
  # export cleaned bt data as binary
  saveRDS(nlcd.ras, "../clean_binary_data/nlcd.ras.RDS")


