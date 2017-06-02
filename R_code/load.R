#-----------------------------------------------------------------------------#
# load.R
# Type: R loading script

# AUTHOR:
# Tyler Huntington, 2017
# JBEI Sustainability Research Group
# Project: Feedstock Agnostic Biorefinery Study

# PURPOSE:
# A script to load all the raw data for the Feedstock Agnostic analysis


# SIDE-EFFECTS:

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
#-----------------------------------------------------------------------------#

# set working directory
this.dir <- dirname(csf())
setwd(this.dir)
rm(list=ls())

# load current US biorefinery profiles (Source: Renewable Fuels Association)
bioref_profiles.df <- 
  read.csv("../../raw_data_files/current_US_biorefineries.csv", 
           header = T,
           colClasses = "character")

# export as binary data file
saveRDS(bioref_profiles.df, "../raw_binary_data/raw_biorefs.df.RDS")

# load US county boundary shapefile (Source: US Census, 20m resolution)
county_bounds.spdf <- 
  rgdal::readOGR(dsn = "../../raw_data_files/cb_2016_us_county_20m.shp",
                 layer = "cb_2016_us_county_20m")

# export as binary data file
saveRDS(county_bounds.spdf, "../raw_binary_data/raw_county_bounds.spdf.RDS")

# load US state boundary shapefile (Source: US Census, 20m resolution)
state_bounds.spdf <- 
  rgdal::readOGR(dsn = "../../raw_data_files/cb_2016_us_state_20m.shp",
                 layer = "cb_2016_us_state_20m")

# export as binary data file
saveRDS(state_bounds.spdf, "../raw_binary_data/raw_state_bounds.spdf.RDS")

# load US national road network (primary and secondary) shapefile (Source: USDT)
roads.sldf <- 
  rgdal::readOGR(dsn = "../../raw_data_files/faf3_network.shp",
                 layer = "faf3_network")

# export as binary data file
saveRDS(roads.sldf, "../raw_binary_data/raw_roads.sldf.RDS")

# load billion ton study biomass data for all crops for 2018, 2030, and 2040
bt_all_crops.df <- 
  read.csv("../../raw_data_files/bt_biomass_yrs_18_30_40.csv", 
           header = T,
           colClasses = "character")

# export as binary data file
saveRDS(bt_all_crops.df, "../raw_binary_data/raw_bt_all_crops_18_30_40.df.RDS")





