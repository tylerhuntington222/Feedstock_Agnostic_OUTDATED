
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

# ###### BASIC FUNCTIONS ######
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


MakeBioshedsDataFrame <- function(feeds, years, scenarios, prices, ranges
                                  
###### LOAD LIBRARIES #######
packages <- c("ggmap", "raster", "sp", "ggplot2", "rgeos", "rgdal")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

require(ggplot2)
require(ggmap)
require(raster)
require(sp)
require(rgeos)
require(rgdal)

###### LOAD LIBRARIES #######
packages <- c("ggmap", "raster", "sp", "ggplot2", "rgeos", "rgdal")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

require(ggplot2)
require(ggmap)
require(raster)
require(sp)
require(rgeos)
require(rgdal)

###### LOAD CLEANED DATA ######

# load billion ton study biomass data
biomass.data <- readRDS("../clean_binary_data/bt_biomass_18_30_40.df.RDS")

# load bioref locations data
biorefs.sptdf <- readRDS("../clean_binary_data/biorefs.sptdf.RDS")

# load county polygons data
counties.spdf <- readRDS("../clean_binary_data/counties.spdf.RDS")



###### LOAD FUNCTIONS ######
source("BtDataSelect_fun.R")
source("SumBioshedFeeds_cids_fun.R")
source("parTLCidsInRange_fun.R")


scenarios <- c("Basecase, all energy crops", "2% yield inc.", "3% yield inc.", 
               "4% yield inc.")
feeds <- c("Corn stover", "Wheat straw")
prices <- c(40, 50, 60)  # note: this is delivered price per dt 
years <- c (2018, 2030, 2040)
ranges <- c(40, 50, 60)


no.cores <- (detectCores() - 1)
cl <- makeCluster(no.cores, type = "SOCK", outfile="")
registerDoSNOW(cl)

res.1 <- foreach (scenario = scenarios, .combine = "rbind") %dopar% {
  
  print(paste0("Feedstock: ", feed))
  
  if (feed == "Corn stover") {
    crop <- "Oats"
  } else if (feed == "Wheat straw") {
    crop <- "Wheat"
  }
  
  # read in kmeans cluster cents for the crop that produces this residue
  centroids.file <- paste0("../output/US.cluster.cents.", crop, ".RDS")
  centroids.data <- readRDS(centroids.file)
  n <- 1
  for (year in years) {
    print(paste0("Year: ", year))
    for (range in c(40, 50, 60)) {
      print(paste0("Range: ", range))
      for (feed in feeds) {
        print(paste0("Scenario: ", scenario))
        for (price in prices) {
          print(paste0("Price: ", price))
          
          # define parameters for BT data to use
          yr <- substr(as.character(year), 3,4)
          
          
          # subset BT data
          biomass.df <- BtDataSelect(biomass.data, year, 
                                     scenario, feed, price)
          
          
          # load pre-calculated bioshed data 
          range.units <- "mi"
          rid.bioshed.key <- 
            readRDS(paste0("../output/feedsheds/curr_ref_biosheds_", crop, "_",
                           range, range.units, ".RDS"))
          
          
          # calculate total biomass available to each bioref,
          res <- SumBioshedFeeds(counties.spdf, biorefs.sptdf,
                                 catchments.data = rid.bioshed.key,
                                 centroids.data = centroids.data,
                                 biomass.df = biomass.df,
                                 feed_choices = feed)
          
          drop.cols <- c("All_Feedstocks")
          res <- res[,!(names(res) %in% drop.cols)]
          
          res$Feedstock <- gsub(" ", "_", feed)
          res$Year <- year
          res$Drive.range <- range
          res$Price <- price
          res$Scenario <- scenario
          
          names(res)[names(res) == (gsub(" ", "_", feed))] <- "Dt.biomass"
          
          res
        }
      }
    }
  }
}


### Add oat straw and sorghum stubble
feeds <- c("Oats straw", "Sorghum stubble")
prices <- c(40, 50, 60)  # note: this is delivered price per dt 


res.2 <- foreach (scenario = scenarios, .combine = "rbind") %dopar% {
  print(paste0("Feedstock: ", feed))
  
  if (feed == "Oats straw") {
    crop <- "Oats"
  } else if (feed == "Sorghum stubble") {
    crop <- "Sorghum"
  }
  
  # read in kmeans cluster cents for the crop that produces this residue
  centroids.file <- paste0("../output/US.cluster.cents.", crop, ".RDS")
  centroids.data <- readRDS(centroids.file)
  n <- 2
  for (year in c(2018, 2030, 2040)) {
    print(paste0("Year: ", year))
    for (range in c(50)) {
      print(paste0("Range: ", range))
      for (feed in feeds) {
        print(paste0("Scenario: ", scenario))
        for (price in prices) {
          print(paste0("Price: ", price))
          
          # define parameters for BT data to use
          yr <- substr(as.character(year), 3,4)
          
          
          # subset BT data
          biomass.df <- BtDataSelect(biomass.data, year, 
                                     scenario, feed, price)
          
          
          # load pre-calculated bioshed data 
          range.units <- "mi"
          rid.bioshed.key <- 
            readRDS(paste0("../output/feedsheds/curr_ref_biosheds_", crop, "_",
                           range, range.units, ".RDS"))
          
          
          # calculate total biomass available to each bioref,
          res <- SumBioshedFeeds(counties.spdf, biorefs.sptdf,
                                 catchments.data = rid.bioshed.key,
                                 centroids.data = centroids.data,
                                 biomass.df = biomass.df,
                                 feed_choices = feed)
          
          drop.cols <- c("All_Feedstocks")
          res <- res[,!(names(res) %in% drop.cols)]
          
          res$Feedstock <- gsub(" ", "_", feed)
          res$Year <- year
          res$Drive.range <- range
          res$Price <- price
          res$Scenario <- scenario
          
          names(res)[names(res) == (gsub(" ", "_", feed))] <- "Dt.biomass"
          
          res
        }
      }
    }
  }
}

# add energy crops
feeds <- c("Sorghum", "Switchgrass", "Miscanthus")


res.3 <- foreach(scenario = scenarios, .combine = "rbind") %dopar% {
  
  # read in ag/pasture kmean cluster cents
  centroids.file <- paste0("../output/US.cluster.cents.sp.RDS")
  centroids.data <- readRDS(centroids.file)
  
  for (year in years) {
    print(paste0("Year: ", year))
    
    for (range in ranges) {
      print(paste0("Range: ", range))
      
      for (feed in feeds) {
        print(paste0("Scenario: ", scenario))
        
        for (price in prices) {
          print(paste0("Price: ", price))
          
          # define parameters for BT data to use
          yr <- substr(as.character(year), 3,4)
          
          
          # subset BT data
          biomass.df <- BtDataSelect(biomass.data, year, 
                                     scenario, feed, price)
          
          # load pre-calculated bioshed data 
          range.units <- "mi"
          rid.bioshed.key <- 
            readRDS(paste0("../output/feedsheds/curr_ref_biosheds_EC_",
                           range, range.units, ".RDS"))
          
          
          # calculate total biomass in each bioref's bioshed
          res <- SumBioshedFeeds(counties.spdf, biorefs.sptdf,
                                 catchments.data = rid.bioshed.key,
                                 centroids.data = centroids.data,
                                 biomass.df = biomass.df,
                                 feed_choices = feed)
          
          drop.cols <- c("All_Feedstocks")
          res <- res[,!(names(res) %in% drop.cols)]
          
          res$Feedstock <- gsub(" ", "_", feed)
          res$Year <- year
          res$Drive.range <- range
          res$Price <- price
          res$Scenario <- scenario
          
          names(res)[names(res) == (gsub(" ", "_", feed))] <- "Dt.biomass"
          
          res
        }
      }
    }
  }
}

full.res <- rbind(res.1, res.2, res.3)
saveRDS(full.res, "master.dataset.RDS")

stopCluster(cl)

}











