
#-----------------------------------------------------------------------------#
# SumBioshedFeeds_fun.R

# Type: R analysis script

# AUTHOR:
# Tyler Huntington, 2017
# JBEI Sustainability Research Group
# Project: Feedstock Agnostic Biorefinery Study

# PURPOSE:
# Calculates the total of biomass availability for biorefineries
# given supplied lists of centroids of crop/pasture land previously calculated
# to be within range of each refinery. Centroids inherit yield vals from the
# county in which they are located. 

# PARAMETERS:
# counties.data - an object of class SpatialPolygonsDataFrame in which features 
  # represent US county boundaries. 
# biorefs.data - an object of class SpatialPointsDataFrame in which points
  # represent the locations of biorefineries.
# catchments.data - a data frame in which the rownames correspond to RIDs of 
  # biorefineries and the only column is a list of numeric vectors containing 
  # the FIPS codes of counties within range of the corresponding refinery.
# centroids.data - a SpatialPointsDataFrame containing the k-means cluster 
  # centroids and details about each in the associated df
# biomass.df - a data frame of projected biomass availability data organized
  # in BT format

# RETURNS:

# SIDE-EFFECTS:
# 

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


SumBioshedFeeds <- function(counties.data,
                            biorefs.data,
                            catchments.data,
                            centroids.data,
                            biomass.df) {
  
  # # TEMP: set params for local function testing 
  # counties.data <- readRDS("../clean_binary_data/counties.spdf.RDS")
  # biorefs.data <- readRDS("../clean_binary_data/biorefs.sptdf.RDS")
  # catchments.data <- readRDS("../output/sample_catchment_output.RDS")
  # biomass.df <- readRDS("../output/bt_select_sample.RDS")
  
  ###### LOAD LIBRARIES #######
  
  packages <- c("ggmap", "raster", "sp", "ggplot2", "rgeos", "plyr")
  if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
    install.packages(setdiff(packages, rownames(installed.packages())))  
  }
  
  library(ggplot2)
  library(ggmap)
  library(raster)
  library(sp)
  library(rgeos)
  library(plyr)
  
  ###### MERGE BIOMASS FEEDSTOCK AVAILABILITIES WITH COUNTY POLYGONS ######
  
  feed_choices <- unique(biomass.df$Feedstock)
  
  for (feedstock_type in feed_choices) {
    # extract particular feedstocks production data at county level
    feedstock.df <- 
      subset(biomass.df,
             (paste(biomass.df$Feedstock) == eval(feedstock_type)))
    
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
    
    # change col names
    names(feedstock.df) <- c("FIPS", 
                             paste(feedstock_type, "YIELD", sep = "_"),
                             paste(feedstock_type, "PRODUCTION", sep = "_"), 
                             paste(feedstock_type, "LAND_AREA", sep = "_"))
    
    # merge feedstock data with counties 
    counties.data <- sp::merge(counties.data, 
                               feedstock.df, 
                               by = "FIPS")
  }
  
  ###### SUM BIOMASS PER BIOSHED AREA ######
  
  # init empty list for biomass availability summaries for all refineries
  refinery_sums.ls <- list(NULL)
  
  # initialize empty list for refinery catchment SPDFs
  refinery_catchments.ls <- list(NULL)
  
  # initialize empty df for refinery summary dataframe
  refinery_summaries.df <- data.frame(NULL)
  
  # iterate over refineries (by RID)
  for (RID in seq(1, nrow(biorefs.data))){
    
    # get cids of counties in range of refinery
    in.range.cids <- (catchments.data[RID, 1])[[1]]
    
    in.range.cids <- unlist(in.range.cids)
    
    in.range.fips <- lapply(in.range.cids, function(x) {substr(x, 1, 5)})
    in.range.fips <- unique(unlist(in.range.fips))
    
    # subset counties layer for FIPS in range
    catchment.spdf <- counties.data[counties.data$FIPS %in% in.range.fips, ]
    
    # determine proportion of biomass in each catchment county falls in bioshed
    for (c in 1:nrow(catchment.spdf)) {
      
      # subset all counties for particular county
      cnty.sp <- catchment.spdf[c, ]
      
      # get fips code of this county
      cnty.fips <- cnty.sp[1]$FIPS
      
      # subset centroids for those in county
      all.cnty.cents <- centroids.data[centroids.data$fips == cnty.fips, ]
      
      # determine how many agpoints county has in total
      total.agpoints <- sum(all.cnty.cents$size)

      # susbet for agcents that fall in bioshed
      shed.cnty.cents <- all.cnty.cents[all.cnty.cents$cid %in% in.range.cids, ]
      
      # find sum of agpoints of county in bioshed
      shed.agpoints <- sum(shed.cnty.cents$size)
      
      # calculate proportion of all county agpoints fall in bioshed
      prop.in.shed <- as.numeric(shed.agpoints)/as.numeric(total.agpoints)
      
      ## update biomass production vals of county based on fractional
      ## contribution to this bioshed
      
      # find columns that contain production
      IsProductionCol <- function(colname) {
        start <- (nchar(colname) - 9)
        end <- nchar(colname)
        last.chars <- substr(colname, start, end)
        
        if (last.chars == "PRODUCTION") {
          return (TRUE)
        } else {
          return(FALSE)
        }
      }
      cols <- names(cnty.sp)
      prod.cols <- which(unlist(lapply(cols, IsProductionCol)))
      
      # update vals
      cnty.sp@data[,prod.cols] <- (prop.in.shed * cnty.sp@data[,prod.cols])
  
      # slot updated production vals into catchment spdf
      catchment.spdf@data[c,] <- cnty.sp@data
    }
    
    # initialize empty df for storing feedstock sum vals for catchment
    feed_sums.df <- data.frame(NULL) 
    
    # iterate over feedstock types
    for (feed_type in feed_choices) {
      
      # reformat feedstock string to elim whitespaces
      feed_type <- gsub(" ", "_", feed_type)
      
      # calculate sum of feedstock type for entire catchment
      assign(paste(feed_type, "_CATCH_SUM", sep = ""),
             sum(na.omit(as.numeric(catchment.spdf[[paste(feed_type,
                                                          "PRODUCTION", 
                                                          sep = "_")]]))))
      
      # store feedstock sum in df
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
      refinery_summaries.df[,col] <- 
        as.character(refinery_summaries.df[,col])
    }
  }
  
  # name columns
  names(refinery_summaries.df) <- c("RID", feed_sums.df[,1])
  
  # redefine classes of columns appropriately
  for (col in seq(1, ncol(refinery_summaries.df),1)){
    refinery_summaries.df[,col] <- as.numeric(refinery_summaries.df[,col])
  }
  
  # merge with biorefineries SPTDF
  biorefs.sptdf <- sp::merge(biorefs.data, refinery_summaries.df,
                             by = "RID")
  
  return (biorefs.sptdf)
  
}


# ###### FUNCTION TESTING ######
# 
# ###### LOAD LIBRARIES #######
# packages <- c("ggmap", "raster", "sp", "ggplot2", "rgeos", "rgdal")
# if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
#   install.packages(setdiff(packages, rownames(installed.packages())))  
# }
# 
# library(ggplot2)
# library(ggmap)
# library(raster)
# library(sp)
# library(rgeos)
# library(rgdal)
# 
# ###### LOAD CLEANED DATA ######
# 
# # load billion ton study biomass data
# biomass.df <- readRDS("../clean_binary_data/bt_biomass_18_30_40.df.RDS")
# 
# # TODO: load NREL biomass data
# 
# # load bioref locations data
# biorefs.sptdf <- readRDS("../clean_binary_data/biorefs.sptdf.RDS")
# 
# # load county polygons data
# counties.spdf <- readRDS("../clean_binary_data/counties.spdf.RDS")
# 
# # load kmeans cluster centroids
# centroids.data <- readRDS("../output/US.cluster.cents.sp.RDS")
# 
# # load pre-calculated bioshed data 
# range <- 50
# range.units <- "mi"
# rid.bioshed.key <- readRDS( paste0("../output/curr_ref_biosheds_",
#                                    range, range.units, ".RDS"))
# 
# 
# # test run function
# test <- SumBioshedFeeds(counties.spdf,
#                         biorefs.spdf,
#                         catchments.data,
#                         centroids.data,
#                         biomass.df)
# 


