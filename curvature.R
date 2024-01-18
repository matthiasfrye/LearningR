# load libraries
if(!require(osmextract)) install.packages("osmextract")
if(!require(sf)) install.packages("sf")
if(!require(data.table)) install.packages("data.table")
if(!require(geosphere)) install.packages("geosphere")

library(osmextract)
library(sf)
library(data.table)
library(geosphere)

# Check whether accidents data is already loaded into memory 
# => run first part of TrafficAccidents.R otherweise
if (!exists("accidents") | !is.data.frame(get("accidents"))) {
  stop("Accidents data must be loaded in data frame 'accidents'.")
}

# query string for relevant roads in OSM
my_vectortranslate <- c(
  # SQL-like query where we select only the following fields
  "-select", "osm_id, highway, lanes, maxspeed, lit, oneway, sidewalk, cycleway " 
)

folder <- "TrafficAccidents/"
URL <- "https://download.geofabrik.de/europe/germany/"

# Check if the folder exists, if it doesn't exist, create the folder
if (!file.exists(folder)) {
  dir.create(folder)
}

# download germany if file does not yet exist
if (!file.exists(paste(folder,"bremen-latest.osm.pbf", sep = ""))) {
  download.file(paste(URL, "bremen-latest.osm.pbf", sep = ""),
                paste(folder, "bremen-latest.osm.pbf", sep = ""))
}

# convert germany pbf file to .gpkg file
osm_gpkg <- oe_vectortranslate(paste(folder, "bremen-latest.osm.pbf", sep = ""), 
                               extra_tags = c("lanes", "maxspeed", "lit", "oneway", "sidewalk", "cycleway"),
                               vectortranslate_options = my_vectortranslate,
                               layer="lines")

# read gpkg file and convert to sf
ger_sf <- sf::st_as_sf(st_read(osm_gpkg, layer="lines"))


for (i in 1:nrow(sf_ger)) {
  i <- 2
  geom <- st_geometry(ger_sf[i,])
  coords <- geom[[1]]
  curvature <- 1 / gcIntermediate(coords[,1], coords[,2], n=100)$d
  plot(curvature, type="l", xlab="Distance along road", ylab="Curvature")
}


library(geosphere)

# Example coordinates (latitude and longitude) - replace with your actual data
coords <- data.frame(lon = c(-122.4194, -122.4073, -122.3959), 
                     lat = c(37.7749, 37.7945, 37.7765))

# Calculate distances between points
distances <- distm(coords[, c("lon", "lat")])

# Calculate angles between points
angles <- abs(diff(atan2(coords$lat, coords$lon)))

# Calculate curvature
curvature <- angles / distances

# Visualize the curvature
plot(curvature, type="l", xlab="Distance along road", ylab="Curvature")
