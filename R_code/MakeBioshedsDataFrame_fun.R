
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


MakeBioshedsDataFrame <- function(biomass.data, biorefs.data, counties.data,
                                  feeds, years, scenarios, prices, ranges) {
                                  
  # ###### LOAD LIBRARIES #######
  # packages <- c("ggmap", "raster", "sp", "ggplot2", "rgeos", "rgdal")
  # if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  #   install.packages(setdiff(packages, rownames(installed.packages())))  
  # }
  
  require(ggplot2)
  require(ggmap)
  require(raster)
  require(sp)
  require(rgeos)
  require(rgdal)

  
  ###### LOAD FUNCTIONS ######
  source("BtDataSelect_fun.R")
  source("SumBioshedFeeds_cids_fun.R")
  
  no.cores <- (detectCores() - 1)
  cl <- makeCluster(no.cores, type = "SOCK", outfile="")
  registerDoSNOW(cl)
  
  res.1 <- foreach (feed = feeds, .combine = "rbind") %dopar% {
    
    print(paste0("Feedstock: ", feed))
    
    if (feed == "Corn stover") {
      crop <- "Corn"
    } else if (feed == "Wheat straw") {
      crop <- "Wheat"
    } else if (feed == "Oats straw") {
      crop <- "Oats"
    } else if (feed == "Sorghum stubble") {
      crop <- "Sorghum"
      
    } else if  (feed %in% c("Biomass sorghum", "Switchgrass", "Miscanthus")) {
      crop <- "EnergyCrops"
    }
    
    # read in kmeans cluster cents for the crop that produces this residue
    centroids.file <- paste0("../output/data_products/US.cluster.cents.", 
                             crop, ".RDS")
    centroids.data <- readRDS(centroids.file)
    n <- 1
    for (year in years) {
      print(paste0("Year: ", year))
      for (range in ranges) {
        print(paste0("Range: ", range))
        for (scenario in scenarios) {
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
              readRDS(paste0("../output/data_products/curr_ref_biosheds_", 
                             crop, "_", range, range.units, ".RDS"))
            
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
  
  # export as binary data file
  filepath <- "../output/data_products/master.dataset"
  saveRDS(res, paste0(filepath, ".RDS"))
  
  # export as excel spreasheet
  file <- paste0(filepath, ".csv")
  write.csv(results.df, csv.filepath, fileEncoding = "UTF-16LE", row.names = F)
  
  stopCluster(cl)

}











