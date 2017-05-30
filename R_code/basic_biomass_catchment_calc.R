#-----------------------------------------------------------------------------#
# basic_biomass_catchment_calc.R
# Type: R function

# AUTHOR:
# Tyler Huntington, 2017
# JBEI Sustainability Research Group
# Project: Feedstock Agnostic Biorefinery Study

# PURPOSE:
# calculate total available biomass within defined buffer radius of biorefinery

# PARAMATERS:
# year - an int (2018, 2030 or 20140) specifying the BT scenario year to use
 
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

# radius - int value (in miles) specifying radius of buffer to define 
  # catchment area in which available biomass will be calculated

# RETURNS:
# ESRI shapefile containing vector point locations of US biorefinerie in 
# specified region w/ biomass availability by feedstock type in attribute table.

#-----------------------------------------------------------------------------#

BasicBiomassCatchmentCalc <- 
  function(year, scenario, feedstocks, price, radius) {
  
  # # TEMP: define sample parameter values for testing function in global enviro
  # year = 2018
  # scenario = "Basecase, all energy crops"
  # feedstocks = c("residues", "herb", "woody")
  # price = 80
  # radius = 60
  
  ###### INSTALL PACKAGES (IF NECCESSARY) ######
  # install.packages("kimisc")
  # install.packages("knitr")
  # install.packages("R.utils")
  # install.packages("spatial")
  # install.packages("GISTools")
  # install.packages("gdata")
  # install.packages("sp")
  # install.packages("rgdal")
  # install.packages("rgeos")
  # install.packages("maptools")
  # install.packages("ggplot2")
  # install.packages("plyr")
  # install.packages("dplyr")
  # install.packages("raster")
  
  ###### LOAD LIBRARIES ######
  library(kimisc)
  library(knitr)
  library(R.utils)
  library(spatial)
  library(GISTools)
  library(gdata)
  library(sp)
  library(rgdal)
  library(rgeos)
  library(maptools)
  library(plyr)
  library(dplyr)
  library(raster)
  library(ggplot2)
  
  
  ###### FUNCTION SETTINGS ######
  # set wd
  setwd("/Users/tyler/Google Drive/JBEI_Sustainability_Team/Feedstock_Agnostic/Analysis/feedstock_agnostic_R")
  # define standardized CRS for spatial data
  aea.crs <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 
                 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 
                 +units=m +no_defs")
  
  
  ###### LOAD DATA #####
  
  # load US biorefinery locations shapefile (Source: ???)
  biorefineries.sptdf <- readOGR(dsn = "../data/biorefineries.shp", 
                                 layer = "biorefineries")
  # set CRS
  biorefineries.sptdf <- spTransform(biorefineries.sptdf, aea.crs)
  
  # load US county boundary shapefile (Source: US Census, 20m resolution)
  county_bounds.spdf <- readOGR(dsn = "../data/cb_2016_us_county_20m.shp",
                                layer = "cb_2016_us_county_20m")
  # set CRS
  county_bounds.spdf <- spTransform(county_bounds.spdf, aea.crs)
  
  # # load 2016 billion ton study biomass data for food, feed, fiber (FFF) crops
  # bt_all_crops.df <- read.csv("bt_biomass_yrs_18_30_40.csv", header = T,
  #                              colClasses = 'character')
  
  # # save as RDS for quicker loading in subsequent script runs
  # saveRDS(bt_all_crops.df, "bt_biomass_yrs_18_30_40.rds")
  
  # load from RDS instead (much faster than csv)
  bt_all_crops.df <- readRDS("../data/bt_biomass_yrs_18_30_40.rds")
  
  
  ###### SET UP COUNTY LEVEL SPATIAL POLYGONS LAYER ######
  
  # subset for continental US counties only
  non_cont_states <- c("02", "15", "72") 
  county_bounds.spdf <- subset(county_bounds.spdf,
                               !(paste(county_bounds.spdf$STATEFP) 
                                 %in% non_cont_states))
  
  # change GEOID column to FIPS code to match BT dataset
  names(county_bounds.spdf@data)[which(names(county_bounds.spdf@data) 
                                       == "GEOID")] <- "FIPS"
  
  # rename var for simplicity in subsequent analysis
  counties.spdf <- county_bounds.spdf
  counties.spdf <- spTransform(counties.spdf, aea.crs)
  
  
  ###### SET UP BILLION TON DATA ######
  
  # change fips column name to FIPS to match counties baselayer
  names(bt_all_crops.df)[which(names(bt_all_crops.df) 
                               == "fips")] <-  "FIPS"
  
  
  ###### DEFINE PARAMS OF BT DATA TO ANALYZE ###### 
  
  # define year of analysis
  year_choice <- as.character(year)
  # choose from:
  # "2018"
  # "2030"
  # "2040"
  
  biomass.df <- subset(bt_all_crops.df,
                       (paste(bt_all_crops.df$Year) == year_choice))
  
  # define scenario
  scenario_choice <- as.character(scenario)
  
  # subset for chosen scenario
  biomass.df <- subset(biomass.df,
                       (paste(biomass.df$Scenario) == scenario_choice))
  
  # define price point (units: US dollars per dry ton) 
  price_choice <- as.character(price)
  
  # subset for price point
  biomass.df <- subset(biomass.df,
                       (paste(biomass.df$Biomass.Price) == price_choice))
  
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
  
  # make sure there are no duplicates in feed_choices vector
  feed_choices <- unique(feed_choices)
  
  # check that all feedstock choices will be in BT data, given other params
  # elim from feed_choices vector if not
  # for (choice in feed_choices) {
  #   if (!(choice %in% unique(biomass.df$Feedstock))) {
  #     feed_choices <- feed_choices[feed_choices != choice]
  #   }
  # }
  
  biomass.df <- subset(biomass.df,
                       (paste(biomass.df$Feedstock) %in% feed_choices))
  
  
  ###### MERGE BIOMASS FEEDSTOCK AVAILABILITIES WITH COUNTY POLYGONS ######
  
  for (feedstock_type in feed_choices) {
    # extract particular feedstocks production data at county level
    feedstock.df <- subset(biomass.df,
                           (paste(biomass.df$Feedstock) 
                            == eval(feedstock_type)))
    
    # reformat feedstock string to elim whitespaces
    feedstock_type <- gsub(" ", "_", feedstock_type)
    
    # subset for columns of interest
    feedstock.df <- subset(feedstock.df,
                           select = c("FIPS", 
                                      "Yield", 
                                      "Production", 
                                      "Land.Area"))
    
    # sum production per county of particular feedstock over all orig land uses
    vars = c("FIPS")
    feedstock.df <- ddply((feedstock.df), 
                          .(FIPS),
                          summarise,
                          Yield = mean(na.omit(as.numeric(Yield))),
                          Production = sum(na.omit(as.numeric(Production))),
                          Land.Area = sum(na.omit(as.numeric(Land.Area))))
    
    # change names
    names(feedstock.df) <- c("FIPS", 
                             paste(feedstock_type, "YIELD", sep = "_"),
                             paste(feedstock_type, "PRODUCTION", sep = "_"), 
                             paste(feedstock_type, "LAND_AREA", sep = "_"))
    
    # merge feedstock data with counties 
    counties.spdf <- sp::merge(counties.spdf, 
                               feedstock.df, 
                               by = "FIPS")
  }
  
  ####### SUM AVAILABLE BIOMASS WITHIN DEFINIED RADIUS OF BIOREFINERY #######
  
  ### Calculate total area of each US county and bind to counties.df
  
  # change FIDS of polygons to be county FIPS codes
  counties.spdf <- spChFIDs(counties.spdf, 
                            as.character(counties.spdf@data$FIPS))
  
  # extract areas from polygons
  tot_county_areas.v <- sapply(slot(counties.spdf, 'polygons'), 
                               function(i) slot(i, 'area')) 
  county_FIDs.v <- sapply(slot(counties.spdf, "polygons"), 
                          function(i) slot(i, 'ID'))
  
  tot_areas_key.df <- data.frame(county_FIDs.v, tot_county_areas.v) 
  names(tot_areas_key.df) <- c("FIPS", "TOTAL_CNTY_AREA")
  
  # merge total areas with counties.df
  counties.spdf <- sp::merge(counties.spdf, tot_areas_key.df, by = "FIPS")
  
  ### Set buffer to define biomass catchment area
  
  # set buffer radius in miles
  buffer_rad <- radius
  
  # convert buffer radius to meters (units of CRS)
  buffer_rad <- (buffer_rad * 1609.34)
  
  # create unique identifier for each biorefinery (RID)
  biorefineries.sptdf@data$RID <- seq(1, nrow(biorefineries.sptdf@data))
  
  # initialize empty list for biomass availability summaries for all refineries
  refinery_sums.ls <- list(NULL)
  
  # initialize empty list for refinery catchment SPDFs
  refinery_catchments.ls <- list(NULL)
  
  # initialize empty df for refinery summary dataframe
  refinery_summaries.df <- data.frame(NULL)
  
  # toggle on for analyzing biorefineries
  for (RID in biorefineries.sptdf@data$RID) {
    
    # TEMP: try for-loop with first 5 biorefineries
    # for (RID in biorefineries.sptdf@data$RID[1:3]) {
    
    # subset for a particular refinery
    focal_refinery.sptdf <- biorefineries.sptdf[biorefineries.sptdf$RID == RID,]
    
    # create buffer around focal refinery
    refinery_buff.sp <- gBuffer(focal_refinery.sptdf, byid = F, 
                                width = buffer_rad,         
                                quadsegs = 100) 
    # set buffer CRS
    proj4string(refinery_buff.sp) <- aea.crs
    
    # intersect buffer with county baselayer to define catchment area 
    catchment.spdf <- raster::intersect(counties.spdf, refinery_buff.sp)
    
    ### Calculate areas of county polygons that fall within catchment area
    # change FIDS of polygons to match FIPS codes 
    catchment.spdf <- spChFIDs(catchment.spdf, 
                               as.character(catchment.spdf@data$FIPS))
    
    # extract area vals from polygons
    catch_county_areas.v <- sapply(slot(catchment.spdf, 'polygons'), 
                                   function(i) slot(i, 'area')) 
    
    # extract FIDS from polygons
    catch_county_FIDs.v <- sapply(slot(catchment.spdf, "polygons"), 
                                  function(i) slot(i, 'ID'))
    
    # create df to store polygon areas and FIDS
    catch_areas_key.df <- data.frame(catch_county_FIDs.v, catch_county_areas.v) 
    names(catch_areas_key.df) <- c("FIPS", "CNTY_AREA_IN_CATCH")
    
    # merge extracted polygon areas to df portion of catchment SPDF
    catchment.spdf <- sp::merge(catchment.spdf, catch_areas_key.df, by = "FIPS")
    
    # calculate proportion of original area of each county caught in catchment
    catchment.spdf@data$PROP_AREA_IN_CATCH <- 
      (catchment.spdf@data$CNTY_AREA_IN_CATCH
       /catchment.spdf@data$TOTAL_CNTY_AREA)
    
    catchment.spdf@data$PROP_AREA_IN_CATCH <- 
      round(catchment.spdf@data$PROP_AREA_IN_CATCH, 5)
    
    #### Update feedstock production based on prop each cnty within catchment
    
    # initialize empty df for storing feedstock sum vals for catchment
    feed_sums.df <- data.frame(NULL) 
    
    # iterate over all feedstocks
    for (feed_type in feed_choices) {
      
      # reformat feedstock string to elim whitespaces
      feed_type <- gsub(" ", "_", feed_type)
      
      # calculate new feedstock production vals
      catchment.spdf[[paste(feed_type, "PRODUCTION", sep = "_")]] <- 
        (as.numeric(catchment.spdf[[paste(feed_type, "PRODUCTION", sep = "_")]])  
         * as.numeric(catchment.spdf[["PROP_AREA_IN_CATCH"]]))
      
      # calculate sum of feedstock type for entire catchment
      assign(paste(feed_type, "_CATCH_SUM", sep = ""),
             sum(na.omit(as.numeric(catchment.spdf[[paste(feed_type,
                                                          "PRODUCTION", 
                                                          sep = "_")]]))))
      
      # store feestock sum in df
      feed_sums.df <- rbind(feed_sums.df, 
                            eval(as.name(paste(feed_type, "_CATCH_SUM", 
                                               sep = ""))))
    }
    
    # add catchment SPDF to list of biorefinery catchment SPDFs
    refinery_catchments.ls[[RID]] <- catchment.spdf
    
    # re-format feedstock sums df
    feed_sums.df$FEEDSTOCK <- gsub(" ", "_", feed_choices)
    names(feed_sums.df) <- c("AVAIL_BIOMASS", "FEEDSTOCK")
    feed_sums.df <- subset(feed_sums.df, 
                           select = c("FEEDSTOCK", "AVAIL_BIOMASS"))
    feed_sums.df$AVAIL_BIOMASS <- as.character(feed_sums.df$AVAIL_BIOMASS)
    
    
    # add total_biomass row to feed_sums.df
    total <- sum(na.omit(as.numeric(feed_sums.df$AVAIL_BIOMASS)))
    feed_sums.df <- rbind(feed_sums.df, c("All_Feedstocks", total))
    feed_sums.df$AVAIL_BIOMASS <- as.character(feed_sums.df$AVAIL_BIOMASS)
    
    # add refinery's biomass availability list of all refiniery summary stats
    # note: indices of dataframes in list will match refinery RIDs
    refinery_sums.ls[[RID]] <- feed_sums.df
   
    # create new row for this biorefinery
    row.entry <- c(as.character(RID), feed_sums.df[,2])
    
    # bind row to biorefinery summary table
    refinery_summaries.df <- rbind(refinery_summaries.df, row.entry)
    
    # change class of colums to char for subsequent rbinds
    for (col in seq(1, ncol(refinery_summaries.df),1)){
      refinery_summaries.df[,col] <- as.character(refinery_summaries.df[,col])
    }
    
  }
  
  # name columns
  names(refinery_summaries.df) <- c("RID", feed_sums.df[,1])
  
  # redefine classes of columns appropriately
  for (col in seq(1, ncol(refinery_summaries.df),1)){
    refinery_summaries.df[,col] <- as.numeric(refinery_summaries.df[,col])
  }
 
  # merge with biorefineries SPTDF
  biorefineries.sptdf <- sp::merge(biorefineries.sptdf, refinery_summaries.df,
                                   by = "RID")
  
  # export refinery catchments as shapefiles
  # for (i in seq(1, length(refinery_catchments.ls), 1)) {
  #   
  #   writeOGR(refinery_catchments.ls[[i]], 
  #            dsn = paste("biorefinery_", i, "_60mi_catchment", sep = ""),
  #            layer = paste("biorefinery_", i, "_60mi_catchment", sep = ""),
  #            driver = "ESRI Shapefile")
  # }
  
  
  return(biorefineries.sptdf)
}
  
  