#-----------------------------------------------------------------------------#
# exploratory_GIS_analysis.R
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

biorefs.sptdf <- readRDS("../clean_binary_data/biorefs.sptdf.RDS")

counties.spdf <- readRDS("../clean_binary_data/counties.spdf.RDS")

# source basic_biomass_catchment_calc.R function
source("biomass_supply_buffer_func.R")

# set data needed for buffer biomass function
datasets <- list(biomass.df, counties.spdf, biorefs.sptdf)


# set feedstocks to include in models
feeds <- c("residues", "herb", "woody") 

# init years
years <- c(2018, 2030, 2040)

# init scenarios
scenarios <- c("Basecase, all energy crops",
               "2% yield inc.", 
               "3% yield inc.",
               "4% yield inc."
               )
               

price_per_dt <- 80

# init empty list to store resulting dfs
result.lst <- list(NULL)

# init index counter var for slotting dfs in result list
i = 1

# iterate over years 2018, 2030, 2040
for (s in scenarios) {
  
  # iterate over scenarios
  for (y in years){
    run.name <- paste(substr(s, 1, 2), y, sep = "_")
    assign(run.name, 
           (BasicBiomassCatchmentCalc(data = datasets,
                                      year = y, 
                                      scenario = s, 
                                      feedstocks = feeds,
                                      price = price_per_dt,
                                      radius = 60)))
    result.lst[[i]] <- eval(as.name(run.name))
    i = i + 1
  }
}

# #TEMP: join all SPTDFS into list
# j = 1
# for (y in years) {
#   # iterate over scenarios
#   for (s in scenarios){
#   data.set <- paste(substr(s, 1, 2), y, sep = "_")
#   result.lst[[j]] <- eval(as.name(data.set))
#   j = j+1
#   }
# }

  

# set up plotting matrix template
plot.new()
par(mar=c(5,5,3,4))
par(mfrow=c(4,3))

# plot matrix of histograms of total avail biomass for US refinery locations
# under diff scenarios in years 2018, 2030 and 2040
for (index in seq_along(result.lst)) {
  hist(result.lst[[index]]@data$All_Feedstocks/1000000,
       breaks = 30, 
       main = " ",
       col = "light blue",
       ylim = c(0,34),
       #xlim = c(0,25),
       xlab = "Total Available Biomass (M dt)",
       ylab = "Count")
  #abline(v = median(result.lst[[index]]@data$All_Feedstocks)/1000000, 
         #col = "red" )
  med <- (median(result.lst[[index]]@data$All_Feedstocks)/1000000)
  segments(x0=med, # Value from x (initial)
           x1=med, # Value to x (final)
           y0=0, # Value from y (initial)
           y1=29, # Value to y (final)
           col='red')

  text(x = med, y = 33, as.character(round(med, 2), adj = c(0.2), cex = 0.1)) 
}


#TODO
# create function than reads in all necessary datasets and elim from calc fx
#

rm(list=ls())
this.dir <- dirname(parent.frame(2)$ofile)

