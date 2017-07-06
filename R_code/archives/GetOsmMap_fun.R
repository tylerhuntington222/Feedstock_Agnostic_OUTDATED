
library(osmar)




library(httr)

# establish bounding box
center_lon <- -88.88722
center_lat <- 39.86531
width <- 160934 
height <- width

box <- center_bbox(center_lon, center_lat, width, height)


res <- httr::POST(overpass_base_url, body=query)

overpass_base_url <- "http://overpass-api.de/api/interpreter"

# build query
# bounding box given as (south, west, north, east)
query <- sprintf("way(%f,%f,%f,%f);out body;",  box[2],box[1],box[4],box[3])

# post query and fetch repsonse
res <- httr::POST(overpass_base_url, body=query)

# get content of repsonse
cont <- httr::content(res, as="text", encoding="UTF-8")

# generate XML doc from content of response
doc <- xml2::read_xml(httr::content(res, as="text", encoding="UTF-8"))

# parse XML
xml <- xmlParse(doc)

# create basemap
basemap <- as_osmar(xml)
