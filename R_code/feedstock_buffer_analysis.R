#-----------------------------------------------------------------------------#
# feedstock_buffer_analysis.R
# Type: R analysis script

# AUTHOR:
# Tyler Huntington, 2017
# JBEI Sustainability Research Group
# Project: Feedstock Agnostic Biorefinery Study
# PI: Corinne Scown

# PURPOSE:
# A script to perform preliminary feedstock supply analysis using a simple 
# buffering method. 

# SECTIONS
# 1. ANALYZE SUPPLY DISTRIBUTIONS FOR EXISTING US BIOREFS
# 2. FEEDSTOCK BLEND ANALYSIS

# SIDE-EFFECTS:
# plots matrix total feedstock supply distributions for existing US biorefs
# plots supply/co-supply matrix for a specified year, scenario and set of feeds

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

states.spdf <- readRDS("../clean_binary_data/states.spdf.RDS")


# load buffering function
source("biomass_supply_buffer_func.R")


###### 1. ANALYZE SUPPLY DISTRIBUTIONS FOR EXISTING US BIOREFS ######

# set data needed for buffering function
datasets <- list(biomass.df, counties.spdf, biorefs.sptdf)

# set feedstocks to model
feeds <- c("residues", "herb", "woody") 

# init years to model
years <- c(2018, 2030, 2040)

# init scenarios to model
scenarios <- c("Basecase, all energy crops",
               "2% yield inc.", 
               "3% yield inc.",
               "4% yield inc."
)

# init price per dry ton parameter
price_per_dt <- 80

# init empty list to store resulting dfs
result.lst <- list(NULL)

# init index counter var for slotting dfs in result list
i = 1

# iterate over scenarios selected
for (s in scenarios) {
  
  # iterate over years selected
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




###### 2. FEEDSTOCK BLEND ANALYSIS ###### 


# set feedstocks to include in blend analysis
#feeds <- c("residues", "herb", "woody") 

feeds <- c("Barley straw"
               , "Biomass sorghum"
               , "CD waste"
               , "Citrus residues"
               , "Corn stover"
               , "Cotton gin trash"
               , "Cotton residue"
               , "Energy cane"
               , "Eucalyptus"
               , "Food waste"
               , "Hardwood, lowland, residue"
               , "Hardwood, upland, residue"
               , "Miscanthus"
               , "Mixedwood, residue"
               , "Mixedwood, tree"
               , "MSW wood"
               , "Noncitrus residues"
               , "Oat straw"
               , "Other forest residue"
               , "Other forest thinnings"
               , "Poplar"
               , "Pine"
               , "Primary mill residue"
               , "Rice hulls"
               , "Rice straw"
               , "Secondary mill residue"
               , "Softwood, natural, residue"
               , "Softwood, planted, residue"
               , "Sorghum stubble"
               , "Sugarcane bagasse"
               , "Sugarcane trash"
               , "Switchgrass"
               , "Wheat straw"
               , "Willow"
               , "Yard trimmings")

#feeds <- all.feeds


# init year
y <- 2040

# init scenarios
s <- "Basecase, all energy crops"


# init price per dry ton
price_per_dt <- 80

datasets <- list(biomass.df, counties.spdf, biorefs.sptdf)

result <- 
  BasicBiomassCatchmentCalc(data = datasets,
                            year = y, 
                            scenario = s, 
                            feedstocks = feeds,
                            price = price_per_dt,
                            radius = 60)




# try for year 2018

# define residue categories
herb <- c("Switchgrass", "Miscanthus", "Energy cane", "Biomass sorghum")

woody <- c("Willow", "Eucalyptus", "Poplar", "Pine")

residues <- c("Wheat straw", "Oat straw", "Corn stover", 
              "Barley straw", "Sorghum stubble")

wastes <- c("MSW wood", "Yard trimmings")


# create vector of all possible feeds
#sel.feeds.v <- c(herb, woody, residues)

sel.feeds.v <- feeds



# replace spaces with underscores
sel.feeds.v <- gsub(" ", "_", sel.feeds.v)


# create blank df to store co-occurence data
fco.mx <- matrix(0, nrow= length(sel.feeds.v), ncol = length(sel.feeds.v))
colnames(fco.mx) <- sel.feeds.v
rownames(fco.mx) <- sel.feeds.v


# iterate over refineries
for (RID in seq_along(result$RID)){
  par.ref <- result[RID,]
  
  # iterate over rows
  for (rowfeed in sel.feeds.v){
    
    for (colfeed in sel.feeds.v){
      
      if (par.ref@data[1, colfeed] > 0 & par.ref@data[1, rowfeed] > 0){
        fco.mx[rowfeed,colfeed] <- fco.mx[rowfeed, colfeed] + 1
      }
    }
  }
}

# elim rows of fco.mx with all zeros
droprows <- c()
for (rowname in rownames(fco.mx)){
  if (all(fco.mx[rowname,] == 0)) {
    droprows <- c(droprows, rowname)
    print (rowname)
    
  }
}


# subset out zero vector rows
fco.mx <- fco.mx[!(rownames(fco.mx) %in% droprows),]


# elim rows of fco.mx with all zeros
dropcols <- c()
for (colname in colnames(fco.mx)){
  if (all(fco.mx[,colname] == 0)) {
    dropcols <- c(dropcols, colname)
    print (colname)
    
  }
}

# subset out zero vector cols
fco.mx <- fco.mx[,!(colnames(fco.mx) %in% dropcols)]

# normalize co-occurence vals based on total number of refineries
fco.mx <- fco.mx/nrow(biorefs.sptdf@data)


cormat <- fco.mx



###### PLOT ######
# adapted from: http://www.sthda.com/
#english/wiki/
#ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization

library(reshape2)
# round vals 
cormat <- round(cormat, 3)

# replace underscores with spaces 
rownames(cormat) <- gsub("_", " ", rownames(cormat))
colnames(cormat) <- gsub("_", " ", colnames(cormat))


melted_cormat <- melt(cormat)
head(melted_cormat)
melted_cormat$value <- round(melted_cormat$value, 3)

library(ggplot2)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()

# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

upper_tri <- get_upper_tri(cormat)
lower_tri <-get_lower_tri(cormat)

# Melt the correlation matrix
library(reshape2)
melted_cormat <- melt(upper_tri, na.rm = TRUE)

# Heatmap
library(ggplot2)
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "white", high = "red", mid = "orange", 
                       midpoint = 0.3, limit = c(0,1), space = "Lab", 
                       name="Co-Occurrence\nIndex") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()


# re - order
reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}

# Reorder the correlation matrix
cormat <- reorder_cormat(cormat)
upper_tri <- get_upper_tri(cormat)
# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Create a ggheatmap
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "white", high = "red", mid = "orange", 
                       midpoint = 0.3, limit = c(0,1), space = "Lab", 
                       name="Supply\nIndex\n") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 14, hjust = 1))+
  theme(axis.text.y = element_text(angle = 0, vjust = 1, 
                                   size = 14, hjust = 1))+
  coord_fixed()
  

# Print the heatmap
print(ggheatmap)

ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 2.5) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.position="left") +
  scale_y_discrete(position = "right") +
  guides(fill = guide_colorbar(barwidth = 0.9, barheight = 8,
                               title.position = "top", title.hjust = 0.5))
