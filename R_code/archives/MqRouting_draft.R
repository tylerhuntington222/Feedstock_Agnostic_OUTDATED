source(OsmRoute_fun.R)
# Load data
data("com")

# Travel path between SpatialPointsDataFrame
route <- OsmRoute(src = src[1,], 
                   dst = dst[1,],
                   sp = TRUE)
plot(route, lty = 2, lwd = 2)
plot(src[1,], pch = 20, col = "green", cex = 3, add = TRUE)             
plot(dst[1,], pch = 20, col = "red", cex = 3, add = TRUE)

library(curl)
library(jsonlite)
library(httr)

api.key <- "gcq5KA3kCkbHod2nIzaSoedTHlvJnWMF"

# Build the query

query <- paste("https://open.mapquestapi.com/directions/v2/route?key=", api.key,
               "&from=", start, "&to=", end)


con <- curl(url = "http://open.mapquestapi.com/directions/v2/route?key=gcq5KA3kCkbHod2nIzaSoedTHlvJnWMF&from=Clarendon%20Blvd,Arlington,VA&to=2400+S+Glebe+Rd,+Arlington,+VA")

open(con, "r")
 <- "http://open.mapquestapi.com/directions/v2/route?key=gcq5KA3kCkbHod2nIzaSoedTHlvJnWMF&from=Clarendon Blvd,Arlington,VA&to=2400+S+Glebe+Rd,+Arlington,+VA"

GET(con)

response = fromJSON(con)
readLines(con)

df <- fromJSON(con)


json <- do.call(rbind, 
                lapply(paste(readLines(con, warn=FALSE),
                             collapse=""), 
                       jsonlite::fromJSON))
this ish 

GET(http://google.com)


json[[1]]

http://open.mapquestapi.com/directions/v2/route?key=KEY&from=Clarendon Blvd,Arlington,VA&to=2400+S+Glebe+Rd,+Arlington,+VA

