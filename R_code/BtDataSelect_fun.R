#-----------------------------------------------------------------------------#
# BtDataSelect_fun.R
# Type: R function

# AUTHOR:
# Tyler Huntington, 2017
# JBEI Sustainability Research Group
# Project: Feedstock Agnostic Biorefinery Study

# PURPOSE:
# calculate total available biomass within defined buffer radius of biorefinery

# PARAMATERS:

# data - dataframe of billion ton study data

# year - an int (2018, 2030 or 2040) specifying the BT scenario year to use

# scenario - a char indicating the billion ton model scenario to use
# choices:
# "Basecase, all energy crops"            
# "Basecase, single energy crops"         
# "Baseline"                             
# "2% yield inc."                         
# "3% yield inc."                         
# "4% yield inc."                         
# "High housing, high energy demands"     
# "High housing, low energy demands"      
# "High housing, medium energy demands"   
# "Medium housing, high energy demands"   
# "Medium housing, low energy demands"    
# "Medium housing, medium energy demands"
# "Wastes and other residues"

# feedstocks - a char or vector of chars indicating feedstocks to include
# may be any combination of "herb", "woody", and "residues"
# or individually specified feedstocks from this list:
# "Barley straw" 
# "Biomass sorghum"
# "CD waste"
# "Citrus residues" 
# "Corn stover"
# "Cotton gin trash" 
# "Cotton residue" 
# "Energy cane" 
# "Eucalyptus"
# "Food waste"
# "Hardwood, lowland, residue"
# "Hardwood, upland, residue"
# "Miscanthus"
# "Mixedwood, residue"
# "Mixedwood, tree"
# "MSW wood"
# "Noncitrus residues"
# "Oats straw"
# "Other forest residue"
# "Other forest thinnings"
# "Poplar"
# "Pine"
# "Primary mill residue"
# "Rice hulls"
# "Rice straw"
# "Secondary mill residue"
# "Softwood, natural, residue"
# "Softwood, planted, residue"
# "Sorghum stubble"
# "Sugarcane bagasse"
# "Sugarcane trash" 
# "Switchgrass"
# "Wheat straw"
# "Willow"
# "Yard trimmings"

# price - an int specifying the price per dry ton of biomass (in US $) to model
# choices:
# 30   
# 40   
# 50   
# 60   
# 70   
# 80   
# 90
# 100

# RETURNS:
# a subsetted version of the input dataframe as specified by other parameter
# values in the function call. 

#-----------------------------------------------------------------------------#

# ###### TEMP: SET WORKING DIRECTORY FOR IDE FUNCTION TESTING ######
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
# this.dir <- dirname(csf())
# setwd(this.dir)
# rm(list=ls())

BtDataSelect <-
  
  function(biomass.data, year, scenario, feedstocks, price) {
    
    # #TEMP: define sample parameter values to test function in global enviro
    # biomass.data <- readRDS("../clean_binary_data/bt_biomass_18_30_40.df.RDS")
    # year <- 2030
    # scenario <- "Basecase, all energy crops"
    # feedstocks <- c("residues", "herb", "woody")
    # price <- 80
    
    ###### INSTALL PACKAGES (IF NECCESSARY) ######
    packages <- c("kimisc", "raster", "sp", "ggplot2", "rgeos", "knitr",
                  "R.utils", "spatial", "GISTools", "gdata", "sp", "rgdal",
                  "rgeos", "maptools", "plyr", "dplyr", "raster", "ggplot2")
    
    if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
      install.packages(setdiff(packages, rownames(installed.packages())))  
    }
    
    library(ggplot2)
    library(ggmap)
    library(raster)
    library(sp)
    library(rgeos)
    library(kimisc)
    library(knitr)
    library(R.utils)
    library(spatial)
    library(GISTools)
    library(gdata)
    library(rgdal)
    library(rgeos)
    library(maptools)
    library(plyr)
    library(dplyr)
    library(raster)

    # define standardized CRS for spatial data
    aea.crs <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23.0 
                   +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 
                   +units=m +no_defs")
    
    ###### DEFINE PARAMS OF BT DATA TO ANALYZE ###### 
    # define year of analysis
    year_choice <- as.character(year)
    
    # subset for chosen year
    biomass.data <- subset(biomass.data,
                         (paste(biomass.data$Year) == year_choice))
    
    # define scenario
    scenario_choice <- as.character(scenario)
    
    # subset for chosen scenario
    biomass.data <- subset(biomass.data,
                         (paste(biomass.data$Scenario) == scenario_choice))
    
    # define price point (units: US dollars per dry ton) 
    price_choice <- as.character(price)
    
    # subset for price point
    biomass.data <- subset(biomass.data,
                         (paste(biomass.data$Biomass.Price) == price_choice))
    
    ### Define biomass feedstocks of interest
    
    # group major categories of biomass
    herb <- c("Switchgrass", "Miscanthus", "Energy cane", "Biomass sorghum")
    
    woody <- c("Willow", "Eucalyptus", "Poplar", "Pine")
    
    residues <- c("Wheat straw", "Oat straw", "Corn stover", 
                  "Barley straw", "Sorghum stubble")
    
    # init feed_choices var
    feed_choices <- NULL
    
    # constuct feed_choices string based on feedstocks arg in function call
    if ("herb" %in% feedstocks){
      feed_choices <- herb
    }
    
    if ("woody" %in% feedstocks){
      feed_choices <- c(feed_choices, woody)
    }
    
    if ("residues" %in% feedstocks){
      feed_choices <- c(feed_choices, residues)
    }
    
    if (!("residues" %in% feedstocks) & !("woody" %in% feedstocks) & 
        !("residues" %in% feedstocks)) {
      feed_choices <- feedstocks
    }
    
    
    # make sure there are no duplicates in feed_choices vector
    feed_choices <- unique(feed_choices)
    
    biomass.data <- subset(biomass.data,
                         (paste(biomass.data$Feedstock) %in% feed_choices))
    
    saveRDS(biomass.data, "../output/bt_select_sample.RDS")
    
    return(biomass.data)
  }



