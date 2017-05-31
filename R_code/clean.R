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

###### SET WORKING DIRECTORY ######
this.dir <- dirname(csf())
setwd(this.dir)
rm(list=ls())

###### LOAD RAW BINARY DATA WORKSPACE IMAGE ######
load("../raw_binary_data/loaded_data_workspace.RData")
print("Raw data objects loaded into workspace:")
print (ls())


###### DATA FORMATTING SETTINGS ######

# define standardized CRS for spatial data
aea.crs <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 
               +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 
               +units=m +no_defs")

###### CLEAN BIOREFINERIES DATA ######

#TODO:
# get coords of bioref locations 

# import geocoding func

# merge bioref locations and attribute data

# set CRS of biorefs locations SPDF
#biorefs.spdf <- sp::spTransform(biorefs.spdf, aea.crs)

# export clean binary data 
#saveRDS(biorefs.spdf, "../clean_binary_data/biorefs.spdf.RDS")

# TEMP:
biorefs.spdf <- rgdal::readOGR(dsn = "../../raw_data_files/biorefineries.shp",
                        layer = "biorefineries")

biorefs.spdf <- sp::spTransform(biorefs.spdf, aea.crs)

saveRDS(biorefs.spdf, "../clean_binary_data/biorefs.spdf.RDS")


###### US COUNTY BOUNDARIES DATA ######

  # subset for continental US counties only
  non_cont_states <- c("02", "15", "72") 
  county_bounds.spdf <- subset(county_bounds.spdf, 
                               !(paste(county_bounds.spdf$STATEFP) 
                                 %in% non_cont_states))
  
  # change GEOID column to FIPS code to match BT dataset
  names(county_bounds.spdf@data)[which(names(county_bounds.spdf@data) == 
          "GEOID")] <- "FIPS"
  
  # rename var for simplicity in subsequent analysis
  counties.spdf <- county_bounds.spdf
  
  # set CRS
  counties.spdf <- spTransform(counties.spdf, aea.crs)
  
  # export clean binary data
  saveRDS(counties.spdf, "../clean_binary_data/counties.spdf.RDS")

###### US STATES BOUNDARIES ######

  # export clean binary data
  saveRDS(state_bounds.spdf, "../clean_binary_data/states.spdf.RDS")
  
###### BILLION TON STUDY DATA ######
  
  # change fips column name to FIPS to match counties baselayer
  names(bt_all_crops.df)[which(names(bt_all_crops.df) == "fips")] <-  "FIPS"
  
  # export cleaned bt data as binary 
  saveRDS(bt_all_crops.df, "../clean_binary_data/bt_biomass_18_30_40.df.RDS")

###### US ROADS NETWORK ######

  # export cleaned bt data as binary 
  saveRDS(roads.sldf, "../clean_binary_data/roads.sldf.RDS")


