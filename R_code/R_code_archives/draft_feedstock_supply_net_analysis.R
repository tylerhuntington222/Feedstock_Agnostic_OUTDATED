
#-----------------------------------------------------------------------------#
# feedstock_supply_net_analysis.R

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






# calculate total feedstock supply from counties in range
#### Update feedstock production based on prop each cnty within catchment

# initialize empty df for storing feedstock sum vals for catchment
feed_sums.df <- data.frame(NULL) 

# iterate over all feedstocks
for (feed_type in feed_choices) {
  
  # reformat feedstock string to elim whitespaces
  feed_type <- gsub(" ", "_", feed_type)
  
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
biorefineries.sptdf <- sp::merge(biorefineries.sptdf, refinery_summaries.df,
                                 by = "RID")



# check remaining gmaps queries
routeQueryCheck()






