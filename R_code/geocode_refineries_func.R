#-----------------------------------------------------------------------------#
#' geocode_refineries_func.R
#'
#' A function that geocodes points of interest (POIs) in the US, given their
#' address. 
#' @param data a data frame in which each entry represents a POI. The first 
#' column contains the name of the POI and the second column contains the 
#' it's US postal address.
#' @export geocoded_pois.df an object of class SpatialPointsDataFrame in which 
#' the @data portion consists of a df identical to the input df. 
#'
#' @examples
#' GeocodeLocations(biorefineries.df)
#' 

# Adapted from a script authored by Shane Lynn 10/10/2013
#-----------------------------------------------------------------------------#

# # #TEMP: for testing func in this script
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

GeocodeLocations <- function(data){
  
  # define standardized CRS for spatial data
  aea.crs <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 
                 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 
                 +units=m +no_defs")
  
  # # TEMP: for testing function within this script
  #  data <- read.csv("../../raw_data_files/current_US_biorefineries.csv", 
  #                   header = T)
  
  # load libraries
  library(ggmap)
  library(sp)
  library(raster)
  library(ggmap)
  library(maptools)
  library(rgdal)
  
  # get the address list, and append "Ireland" to the end to increase accuracy 
  # (change or remove this if your address already include a country etc.)
  addresses = data$ADDRESS
  addresses = paste0(addresses, ", USA")
  
  #define a function that will process googles server responses
  getGeoDetails <- function(address){   
    # #TEMP:
    # address <- "27532 West Hwy 30 Sutherland Nebraska"
    
    #use the gecode function to query google servers
    geo_reply = geocode(address, output='all', messaging=TRUE, 
                        override_limit=TRUE)
    #now extract the bits that we need from the returned list
    answer <- data.frame(lat=NA, long=NA, accuracy=NA, 
                         formatted_address=NA, address_type=NA, status=NA)
    answer$status <- geo_reply$status
    
    #if we are over the query limit - want to pause for an hour
    while(geo_reply$status == "OVER_QUERY_LIMIT"){
      print("OVER QUERY LIMIT - Pausing for 1 hour at:") 
      time <- Sys.time()
      print(as.character(time))
      Sys.sleep(60*60)
      geo_reply = geocode(address, output='all', messaging=TRUE, 
                          override_limit=TRUE)
      answer$status <- geo_reply$status
    }
    
    #return Na's if we didn't get a match:
    if (geo_reply$status != "OK"){
      return(answer)
    }   
    #else, extract what we need from the Google server reply into a dataframe:
    answer$lat <- geo_reply$results[[1]]$geometry$location$lat
    answer$long <- geo_reply$results[[1]]$geometry$location$lng   
    if (length(geo_reply$results[[1]]$types) > 0){
      answer$accuracy <- geo_reply$results[[1]]$types[[1]]
    }
    answer$address_type <- paste(geo_reply$results[[1]]$types, collapse=',')
    answer$formatted_address <- geo_reply$results[[1]]$formatted_address
    
    return(answer)
  }
  
  #initialise a dataframe to hold the results
  geocoded <- data.frame()
  
  # find out where to start in the address list (if the script was interrupted before):
  startindex <- 1
  
  # Start the geocoding process - address by address. 
  # geocode() function takes care of query speed limit.
  for (ii in seq(startindex, length(addresses))){
    print(paste("Working on index", ii, "of", length(addresses)))
    #query the google geocoder - this will pause here if we are over the limit.
    result = getGeoDetails(addresses[ii]) 
    print(result$status)     
    result$index <- ii
    #append the answer to the results file.
    geocoded <- rbind(geocoded, result)
  }
  
  # now we add the latitude and longitude to the main data
  data$lat <- geocoded$lat
  data$long <- geocoded$long
  
  coords <- data[,c("long", "lat")]
  
  long_col <- which(names(data) == "long")
  lat_col <- which(names(data) == "lat")
  
  coords <- as.matrix(coords)
  
  pois.spdf <- 
    SpatialPointsDataFrame(coords, data, coords.nrs = c(long_col, lat_col), 
                           proj4string = CRS("+proj=longlat +datum=WGS84"), 
                           bbox = NULL)

  # set to aea.crs
  pois.spdf <- spTransform(pois.spdf, aea.crs)
  
  # # export shapefile
  # writeOGR(pois.sp, dsn="pois.shp", layer= "pois", driver="ESRI Shapefile")
  
  return(pois.spdf)
  
}
  

# #TEMP for plot testing
# plot(counties.spdf)
# plot(pois.spdf, add = T, col = 'blue')

  
  
  
  
