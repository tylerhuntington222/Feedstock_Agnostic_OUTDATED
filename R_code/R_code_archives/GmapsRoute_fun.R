




GmapsRoute <- function (start, end) {
  
  ###### LOAD LIBRARIES #######
  packages <- c("ggmap", "raster", "sp", "rgeos")
  
  if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
    install.packages(setdiff(packages, rownames(installed.packages())))  
  }
  
  library(ggmap)
  library(raster)
  library(sp)
  library(rgeos)

    # calculate route
    rt <- route(start, end, 
                mode = c("driving"),
                structure = c("route"), output = c("simple"),
                alternatives = FALSE, messaging = FALSE, sensor = FALSE,
                override_limit = T)
    
    # calculate total distance of route
    return(rt)