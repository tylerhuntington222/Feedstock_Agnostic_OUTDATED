#-----------------------------------------------------------------------------#
# analysis.R
# Type: R cleaning script

# AUTHOR:
# Tyler Huntington, 2017
# JBEI Sustainability Research Group
# Project: Feedstock Agnostic Biorefinery Study

# PURPOSE:
# A script to perform preliminary feedstock supply analysis


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

###### LOAD CLEANED DATA ######

biomass.df <- readRDS("../clean_binary_data/bt_biomass_18_30_40.df.RDS")

biorefs.spdf <- readRDS("../clean_binary_data/biorefs.spdf.RDS")

counties.spdf <- readRDS("../clean_binary_data/counties.spdf.RDS")

states.spdf <- readRDS("../clean_binary_data/states.spdf.RDS")


# Test out basic_biomass_catchment_calc.R function
source("basic_biomass_catchment_calc.R")

# set feedstocks to include in models
feeds <- c("residues", "herb", "woody") 

# init year
y <- 2018

# init scenarios
s <- "Basecase, all energy crops"


# init price per dry ton
price_per_dt <- 60

datasets <- list(biomass.df, counties.spdf, biorefs.spdf)

result <- 
  BasicBiomassCatchmentCalc(data = datasets,
                            year = y, 
                            scenario = s, 
                            feedstocks = feeds,
                            price = price_per_dt,
                            radius = 60)



