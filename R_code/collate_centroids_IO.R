#-----------------------------------------------------------------------------#
# collate_centroids_IO.R
# Type: R I/O script

# AUTHOR:
# Tyler Huntington, 2017
# JBEI Sustainability Research Group
# Project: Feedstock Agnostic Biorefinery Study

# PURPOSE:
# A script to collate the weighted county centroids from all US states from 
# separate binary data files (one for each state).


# SIDE-EFFECTS:
# outputs a binary data file (.RDS) to the 'output' directory which contains an
# object of class SpatialPointsData frame containing weighted centroids of 
# all US counties.

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
packages <- c("ggmap", "raster", "sp", "ggplot2", "rgeos", "maptools", "dplyr")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

library(ggplot2)
library(ggmap)
library(raster)
library(sp)
library(rgeos)
library(maptools)
library(dplyr)

###### LOAD CLEANED DATA ######

counties.spdf <- readRDS("../clean_binary_data/counties.spdf.RDS")


# # load midwest state county centroids data
# illinois <- readRDS("../output/ILLINOIS_wtd_cntroids.sptdf.RDS")
# indiana <- readRDS("../output/INDIANA_wtd_cntroids.sptdf.RDS")
# # iowa <- readRDS("../output/iowa_county_cntroids.sptdf.RDS")
# kansas <- readRDS("../output/KANSAS_wtd_cntroids.sptdf.RDS") 
# michigan <- readRDS("../output/MICHIGAN_wtd_cntroids.sptdf.RDS")
# minnesota <- readRDS("../output/MINNESOTA_wtd_cntroids.sptdf.RDS")
# #missouri <- readRDS("../output/MISSOURI_wtd_cntroids.sptdf.RDS") 
# nebraska <- readRDS("../output/NEBRASKA_wtd_cntroids.sptdf.RDS") 
# #n.dakota <- readRDS("../output/NORTH DAKOTA_wtd_cntroids.sptdf.RDS")
# ohio <- readRDS("../output/OHIO_wtd_cntroids.sptdf.RDS")
# s.dakota <- readRDS("../output/SOUTH_DAKOTA_wtd_cntroids.sptdf.RDS")
# wisconsin <- readRDS("../output/WISCONSIN_wtd_cntroids.sptdf.RDS")

# define proj4string for common CRS
aea.crs <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23.0
                 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 
               +units=m +no_defs")

# set states for reading in state level centroid data files
state.lev.states <- c("ALABAMA", "MAINE", "CONNECTICUT", "DELAWARE", "VERMONT",
                      "MASSACHUSETTS", "MARYLAND", "NEW JERSEY", 
                      "NEW HAMPSHIRE",
                      "RHODE ISLAND", "WEST VIRGINIA", "TENNESSEE",
                      "ARKANSAS", "KENTUCKY", "PENNSYLVANIA",
                      "ILLINOIS", "GEORGIA", "MINNESOTA", "NORTH DAKOTA",
                      "MISSOURI", "KANSAS", "INDIANA", "MICHIGAN",
                      "NEBRASKA", "FLORIDA", "MISSISSIPPI", "OKLAHOMA",
                      "WISCONSIN", "SOUTH DAKOTA", "OHIO")


# init sptdf to store all US county centroids
US.wtd.cents <- readRDS("../output/ALABAMA_wtd_cntroids.sptdf.RDS")

# iterate over state level centroid data files 
for (i in state.lev.states[2:length(state.lev.states)]) {
  state.cents <- readRDS(paste("../output/", i, 
                               "_wtd_cntroids.sptdf.RDS", sep = ""))
  US.wtd.cents <- spRbind(US.wtd.cents, state.cents)
}

# init vector to store all fips codes
all.fips <- unique(as.character(counties.spdf$FIPS))

# iterate over all counties by fips code
for (j in seq(1, length(all.fips))) {
  
  file <- paste0("../output/counties/fips_",
                 all.fips[j], "_wtd_cntroid.sptdf.RDS")
  
  print(paste0("Checking if file exists for FIPS: ", all.fips[j]))
  
  if (file.exists(file)) {
    cnty.cent <- readRDS(file)
    if (!(is.na(proj4string(cnty.cent)))) {
      if (proj4string(cnty.cent) == as.character(aea.crs)) {
        if (nrow(cnty.cent) == 1) {
          if (!(cnty.cent$FIPS %in% US.wtd.cents$FIPS)) {
            row.names(cnty.cent) <- as.character(nrow(US.wtd.cents)+1)
            US.wtd.cents <- spRbind(US.wtd.cents, cnty.cent)
            print("County added!")
          } else {
            print("County already added.")
          }
        } else {
          print("No features found in SpatialPoints object")
        }
      } else {
        print("Non-matching CRS")
      }
    } else {
      print("CRS for centroid was invalid")
    }
  } else {
    print(paste0("No file found for FIPS: ", all.fips[j]))
  }
}

# determine which counties are still missing
all.fips <- data.frame(all.fips)
names(all.fips) <- c("fips")

done.fips <- data.frame(unique(as.character(US.wtd.cents$FIPS)))
names(done.fips) <- c("fips")

not.done.fips <- anti_join(all.fips, done.fips)

not.done.fips <- as.character(not.done.fips$fips)

saveRDS(not.done.fips, "not.done.fips.RDS")

nrow(counties.spdf)

# Clean up duplicate entries



# export all US county centroids
saveRDS(US.wtd.cents, "../output/US.wtd.cents.RDS")

check <- readRDS("../output/counties/fips_08111_wtd_cntroid.sptdf.RDS")


length(unique((US.wtd.cents$FIPS)))
which(table(US.wtd.cents$FIPS) > 1)


plot(US.wtd.cents[US.wtd.cents$FIPS == "51790",], add = T)
plot(counties.spdf[counties.spdf$FIPS == 51790 ,])






