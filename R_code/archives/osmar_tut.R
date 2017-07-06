
# install packages

# load libraries 
library(osmar)
library(igraph)
library(geosphere)
library(sp)


# download osm planet files for US regions
regions <- c("midwest", "northeast", "south", "west")
url <- "http://download.geofabrik.de/north-america/"

for (region in regions[1:length(regions)]) {
  file <- paste("us-", region, "-latest.osm.bz2", sep = "")
  download.file(sprintf("%s%s", url, file), file)
}


# load osm data for each region into osmar objects
for (region in regions[1:length(regions)]) {
  file <- paste("us-", region, "../../raw_data_files", 
                region, "-latest.osm", sep = "")
  src <- osmsource_osmosis(file = "iowa-latest.osm", osmosis = "osmosis")
  max.box <- corner_bbox(left = -180, right = 180, top = 90, bottom = -90)
  assign(as.character(region), get_osm(max.box, src))
}


###### FUNCTION ######

# combine start and end nodes into SPTDF

sptdf <- 

# determine extent of start.end.sptdf
  
# get osm map for region within extent of start.end.sptdf
file <- paste("us-", region, "../../raw_data_files", 
                region, "-latest.osm", sep = "")
src <- osmsource_osmosis(file = "iowa-latest.osm", osmosis = "osmosis")
max.box <- corner_bbox(left = -180, right = 180, top = 90, bottom = -90)
assign(as.character(region), get_osm(max.box, src))
  
# add start and end nodes to region.osm
  
  
# calculate distance of shortest path between start and end nodes




# combine osmar objects for each region into single contiguous US osmar object
cont.us.osmar <- osmar::c(midwest, northeast, south, west)


# convert start point into osmar object

# 
# load biore
src <- osmsource_file(file = "../output/biorefs.osm")
max.box <- corner_bbox(left = -180, right = 180, top = 90, bottom = -90)
biorefs <- get_osm(bbox, src)

biorefs$nodes$tags

plot(dm)

muc

http://download.geofabrik.de/north-america/us/iowa-latest.osm.bz2
src <- osmsource_api()








url <- "http://osmar.r-forge.r-project.org/"
file <- "muenchen.osm.gz"
download.file(sprintf("%s%s", url, file), file)
system("gzip -d muenchen.osm.gz")

src <- osmsource_osmosis(file = "muenchen.osm", osmosis = "osmosis")
muc_bbox <- center_bbox(11.575278, 48.137222, 3000, 3000)
muc <- get_osm(muc_bbox, src)


# susbet for streets only
hways_muc <- subset(muc, way_ids = find(muc, way(tags(k == "highway"))))

# subest for streets with name
hways <- find(hways_muc, way(tags(k == "name")))

# find nodes of streets
hways <- find_down(muc, way(hways))

# subset full osmar object
hways_muc <- subset(muc, ids = hways)


# find start node
hway_start_node <- local({
  id <- find(muc, node(tags(v == "Sendlinger Tor")))[1]
  find_nearest_node(muc, id, way(tags(k == "highway")))
  })

hway_start <- subset(muc, node(hway_start_node))

# find end node
hway_end_node <- local({
  id <- find(muc, node(tags(v == "Lehel")))[1]
  find_nearest_node(muc, id, way(tags(k == "highway")))
  })

hway_end <- subset(muc, node(hway_end_node))


# convert highways to igraph object
gr_muc <- as_igraph(hways_muc)

summary(gr_muc)


# compute shortest path between start and end points
route <- get.shortest.paths(gr_muc,
                            from = as.character(hway_start_node),
                            to = as.character(hway_end_node))[[1]]

route_nodes <- as.numeric(V(gr_muc)[route[[1]]]$name)

# create new osmar object for route
route_ids <- find_up(hways_muc, node(route_nodes))
route_muc <- subset(hways_muc, ids = route_ids)
route_muc

####### CALCULATE DISTANCE OF SHORTEST PATH ######

# get node ids of nodes in route
node_ids <- route_muc$nodes$attrs$id

# extract way ids of ways in route
way_ids <- local({
  w <- match(node_ids, route_muc$ways$refs$ref)
  route_muc$ways$refs$id[w]
  })

# extract names of ways in correct order
way_names <- local({
  n <- subset(route_muc$ways$tags, k == "name")
  n[match(way_ids, n$id), "v"]
  })

# extract coords of nodes in route
node_coords <- route_muc$nodes$attrs[, c("lon", "lat")]

# compute the distances (meters) and the bearings (degrees) between 
# consecutive nodes using the package geosphere
node_dirs <- local({
  n <- nrow(node_coords)
  from <- 1:(n-1)
  to <- 2:n
  cbind(dist = c(0,
               distHaversine(node_coords[from, ], node_coords[to, ])),
      bear = c(0,
               bearing(node_coords[from, ],
                       node_coords[to, ])))
})

# compute summary stats of route
route_details <- data.frame(way_names, node_dirs)
route_details$cdist <- cumsum(route_details$dist)

# compute total distance of route in meters and convert to miles
tdist <- sum(route_details$dist)
tdist <- tdist/1609.34



# plot basemap
plot_nodes(muc, col = "gray", pch = 16)
plot_ways(hways_muc, add = TRUE)
plot_nodes(hways_muc, add = TRUE, col = "black")

# plot start and end nodes
plot_nodes(hway_start, add = TRUE, col = "green", pch = 16, cex = 2)
plot_nodes(hway_end, add = TRUE, col = "red", pch = 16, cex = 2)

# plot route
plot_nodes(route_muc, add = TRUE, col = "blue")
plot_ways(route_muc, add = TRUE, col = "blue")


# combine basemap, biorefs and centroids, osmar objects














# 
# TRY FOR IOWA

url <- "http://download.geofabrik.de/north-america/us/"
file <- "iowa-latest.osm.bz2"
download.file(sprintf("%s%s", url, file), file)
system("bzip2 -d iowa-latest.osm.bz2")


src <- osmsource_osmosis(file = "iowa-latest.osm", osmosis = "osmosis")
dm_bbox <- center_bbox(-93.6091, 41.6005, 3000, 3000)
dm <- get_osm(dm_bbox, src)

# try for biorefs.osm

src <- osmsource_file(file = "../output/biorefs.osm")
bbox <- corner_bbox(left = -180, right = 180, top = 90, bottom = -90)
biorefs <- get_osm(bbox, src)

biorefs$nodes$tags

plot(dm)

muc

http://download.geofabrik.de/north-america/us/iowa-latest.osm.bz2
src <- osmsource_api()



bb <- center_bbox(-1.53492, 53.81934, 1000, 1000)

?center_bbox
ctown <- get_osm(bb, source = src)
plot(ctown)
points(-1.53492, 53.81934, col = "red", lwd = 5)

str(ctown$ways)
str(ctown)



install.packages('osrm')
library(osrm)

biorefs.sptdf <- readRDS("../clean_binary_data/biorefs.sptdf.RDS")

samp.bioref1 <- (biorefs.sptdf[1,])@coords[1,]

samp.bioref2 <- (biorefs.sptdf[5,])@coords[1,]

rt <- osrmRoute(samp.bioref1, samp.bioref2, overview = F, sp = F)

rt

# route using google maps
library(ggmap)

g_rt <- route("2100 Mt. Diablo Scenic Blvd, Danville, CA", "38.106282, -122.681841", 
              mode = c("driving"),
      structure = c("route"), output = c("simple", "all"),
      alternatives = FALSE, messaging = FALSE, sensor = FALSE,
      override_limit = FALSE)

sum(na.omit(g_rt$miles))

routeQueryCheck()



biorefs.sptdf <- readRDS("../clean_binary_data/biorefs.sptdf.RDS")
biorefs.sptdf <- 
  spTransform( biorefs.sptdf, crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

samp.bioref1 <- (biorefs.sptdf[1,])@coords[1,]

#samp.bioref1 <- c(1, -198884, 2499378) 

samp.bioref2 <- (biorefs.sptdf[5,])@coords[1,]

#samp.bioref1 <- c(1, 916689, 2194244) 

rt <- osrm::osrmRoute(samp.bioref1, samp.bioref2, overview = F, sp = F)

library(sp)
library(raster)
