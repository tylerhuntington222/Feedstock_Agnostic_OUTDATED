# install required packages for all functions in this project directory

packages <- c("raster", "sp", "ggplot2", "rgeos", "rgdal", "plyr", "openxlsx",
              "doSNOW", "foreach", "parallel", "doParallel", "stats", 
              "maptools", "dplyr")

if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}