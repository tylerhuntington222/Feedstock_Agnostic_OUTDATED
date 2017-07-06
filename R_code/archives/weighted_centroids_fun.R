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

WeightedCentroids <- function (poly.data, raster.data) {
  
  
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
  
  # get all raster cells in each feature of poly layer
  in.cells.ls <- raster::extract(raster.data, poly.data, cellnumbers = T)
  
  # add xy coords
  in.cells.xy.ls <- 
    llply(in.cells.ls, function(x) cbind(x, xyFromCell(raster.data, x[,"cell"])))
  
  # find weighted centroids
  centrs.mx = laply(in.cells.xy.ls, 
                function(m){c(weighted.mean(m[,3],m[,2]), 
                              weighted.mean(m[,4],m[,2]))})
  
  # convert to SpatialPoints object
  centrs.spt <- SpatialPoints(centrs.mx)
  
  # project to CRS of polys layer
  proj4string(centrs.spt) <- crs(poly.data)
  
  # extract vals of containing polys 
  centrs.data <- extract(poly.data, centrs.spt)
  
  # convert to SPTDF
  centrs.sptdf <- SpatialPointsDataFrame(centrs.mx, centrs.data)
  
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



  
  
