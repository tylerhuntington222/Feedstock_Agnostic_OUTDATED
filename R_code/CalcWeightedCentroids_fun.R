# weighted_centroids_fun.R

# Tyler Huntington, 2017

# A function to find the weighted centroids of a set of polygon features
# based on a raster layer that overplaps with the vector layer containing the 
# polygons of interest. 

# PARAMETERS:
  # poly.data - object of class SpatialPolygons or SpatialPolygonsDataFrame
                # of which to calculate weighted centroids. Must be in same 
                # CRS/projection and have idential extent as the 
                # raster.data argument.
  # raster.data - object of class RasterLayer whose cell values will provide
                # the weights for calculating weighted polygons. Must be in same 
                # CRS/projection and have idential extent as poly.data argument. 

# RETURNS:
  # object of class SpatialPointsDataFrame whose point features represent the 
  # centroids of the input polygon layer weighted by the values of the raster 
  # layer. Point feature IDs will match the feature IDs of the input polygon 
  # layers. 
#------------------------------------------------------------------------------#

CalcWeightedCentroids <- function (poly.data, raster.filepath) {
  
  
  #TEMP: set function param vals for testing purposes
  #raster.data <- mask  
  #poly.data <- counties.spdf
  
  ###### LOAD LIBRARIES ######
  library(raster)
  library(spdep)
  library(maptools)
  library(rgdal)
  library(plyr)
  library(geosphere)
  library(rgeos)
  library(raster)
  library(spdep)
  library(maptools)
  library(rgdal)
  library(plyr)
  
  # # TEMP:
  # counties.spdf <- readRDS("../clean_binary_data/counties.spdf.RDS")
  # 
  # poly.data <- subset(counties.spdf, counties.spdf$NAME == "Ida")
  # 
  #   
  # raster.filepath <- "../output/counties/cropped_Ida_raster"
  # 
   
  # load raster data for county
  raster.data <- raster(paste(raster.filepath, ".gri", sep = ""))
  
  # get all raster cells in each feature of poly layer
  print("Finding raster cells in county...")
  in.cells.ls <- raster::extract(raster.data, poly.data, cellnumbers = T)
  
  
  # add xy coords
  print("Extracting coordinates...")
  in.cells.xy.ls <- 
    llply(in.cells.ls, function(x) cbind(x, xyFromCell(raster.data, x[,"cell"])))
  
  # find weighted centroids
  print("Calculating weighted centroids...")
  centrs = laply(in.cells.xy.ls, 
                function(m){c(weighted.mean(m[,3],m[,2]), 
                              weighted.mean(m[,4],m[,2]))})
  
  # reformat into matrix 
  centrs.mx <- matrix(ncol = 2, nrow = 1)
  centrs.mx[1,1] <- centrs[1]
  centrs.mx[1,2] <- centrs[2]
  
  # convert to SpatialPoints object
  centrs.spt <- SpatialPoints(na.omit(centrs.mx))
  
  # project to CRS of polys layer
  proj4string(centrs.spt) <- crs(poly.data)
  
  # extract vals of containing polys 
  centrs.data <- extract(poly.data, centrs.spt)
  
  # convert to SPTDF
  centrs.sptdf <- SpatialPointsDataFrame(na.omit(centrs.mx), centrs.data)
  
  # subset DF for relevant cols
  centrs.sptdf <- 
    subset(centrs.sptdf, select = c("NAME", "FIPS", "STATEABBREV", "STATENAME"))
  
  # set CRS
  proj4string(centrs.sptdf) <- crs(poly.data)
  
  return(centrs.sptdf)
}
  
# # TEMP: plotting calls for function testing and development
# plot(states.spdf, col = "red", add = F)
#plot(counties.spdf, add = F, col = "blue")
#plot(centrs.sptdf, add = T, col = "black")
# plot(iowa.spdf, col = rgb(1,0,0,0.5), add = F)
# plot(mx.ras, add = F)
# plot(nlcd.ras, add = F)
# plot(lee.spdf, add = F)
# head(counties.spdf@data)
# plot(gcent, add = T, col = 'blue')
#t.cents
#plot(t.cents, add =T, col = "green", pch = 4)
# plot(mask)
# 



  
  
