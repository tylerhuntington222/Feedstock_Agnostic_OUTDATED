
#-----------------------------------------------------------------------------#
# GetCropList_fun.R
# Type: R function script

# AUTHOR:
# Tyler Huntington, 2017
# JBEI Sustainability Research Group
# PROJECT: Feedstock Agnostic Biorefinery Study

# PURPOSE:
# A script to collate the county-level kmeans cluster centroids for production
# of a particular crop from separate binary data files (one for each county).


# SIDE-EFFECTS:
# outputs a binary data file (.RDS) to the 'output' directory which contains an
# object of class SpatialPointsData frame containing k-means cluster centroids
# for the specified crop across the continental US. 

#-----------------------------------------------------------------------------#


GetCropList <- function(feedstocks) {
  
   crops <- c(NULL)
   
   ec1 <- "Biomass sorghum" %in% feedstocks
   ec2 <- "Switchgrass" %in% feedstocks
   ec3 <- "Biomass sorghum" %in% feedstocks
   
   if (ec1 | ec2 | ec3) {
     crops <- c("EnergyCrops")
   }
   
   if ("Corn stover" %in% feedstocks) {
     crops <- c(crops, "Corn")
   }
   
   if ("Wheat straw" %in% feedstocks) {
     crops <- c(crops, "Wheat")
   }
   
   if ("Sorghum stubble" %in% feedstocks) {
     crops <- c(crops, "Sorghum")
   }
   
   if ("Oats straw" %in% feedstocks) {
     crops <- c(crops, "Oats")
   }
   
   return(crops)
}

   
   
     
     
   
   