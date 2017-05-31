
script.dir <- dirname(sys.frame(1)$ofile)

printDir <- function(x) print(script.dir)


# Test out basic_biomass_catchment_calc.R function
source("biomass_supply_buffer_func.R")

# set feedstocks to include in models
feeds <- c("residues", "herb", "woody") 

# init years
years <- c(2018)

# init scenarios
scenarios <- c("Basecase, all energy crops",
               "2% yield inc." 
               #"3% yield inc.",
               #"4% yield inc."
               )
               

price_per_dt <- 60

# init empty list to store resulting dfs
result.lst <- list(NULL)

# init index counter var for slotting dfs in result list
i = 1

# iterate over years 2018, 2030, 2040
for (s in scenarios) {
  
  # iterate over scenarios
  for (y in years){
    run.name <- paste(substr(s, 1, 2), y, sep = "_")
    assign(as.name(eval(run.name)), 
           (BasicBiomassCatchmentCalc(year = y, 
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
par(mfrow=c(4,3))

# plot matrix of histograms of total avail biomass for US refinery locations
# under diff scenarios in years 2018, 2030 and 2040
for (index in seq(1, length(result.lst), 1)) {
  hist(result.lst[[index]]@data$All_Feedstocks, breaks = 20, main = NULL)
  abline(v = median(result.lst[[index]]@data$All_Feedstocks), col = "red")
  print(median(result.lst[[index]]@data$All_Feedstocks))
}


#TODO
# create function than reads in all necessary datasets and elim from calc fx
#

rm(list=ls())
this.dir <- dirname(parent.frame(2)$ofile)

