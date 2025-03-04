
############################################################################
############################################################################
###                                                                      ###
###                                PART 2                                ###
###                         TRAVEL TIME MATRICES                         ###
###                                                                      ###
############################################################################
############################################################################

# Date updated: Feb 2025

# Set r5r and java pars ---------------------------------------------------

# This tutorial uses R5R V. 2.0.0

# Set memory to be used
options(java.parameters = "-Xmx4G")
library(r5r)

# R5R Directory
r5r_dir <- 'glasgow_router'
# Read network
r5r_core <- setup_r5(data_path = r5r_dir)
gc(reset = TRUE)

# Packages
library(sf)
library(tidyverse)
library(mapview)


# Origins: Download Data Zones and NHS health board geometries ------------

# Create directory for data zones
dir.create("data/datazones")
# Download data zones
download.file(
  "https://maps.gov.scot/ATOM/shapefiles/SG_DataZoneBdry_2011.zip",
  "data/datazones/datazones.zip"
)
# Unzip data zone files
unzip("data/datazones/datazones.zip", exdir = "data/datazones/")

# NHS health boards
# Directory
dir.create("data/healthboards")
# Download boundaries
download.file(
  "https://maps.gov.scot/ATOM/shapefiles/SG_NHS_HealthBoards_2019.zip",
  "data/healthboards/healthboards.zip"
)
# Unzip files
# Unzip data zone files
unzip("data/healthboards/healthboards.zip", exdir = "data/healthboards/")


# Read data zones ---------------------------------------------------------

# Read health board boundaries
healthboard <- st_read("data/healthboards/SG_NHS_HealthBoards_2019.shp")
# Keep Greater Glasgow and Clyde boundary
healthboard <- filter(healthboard, HBName == "Greater Glasgow and Clyde")

# Read data zones in Scotland
data_zones <- st_read("data/datazones/SG_DataZone_Bdry_2011.shp")
# Define geometric centroids within Glasgow
glasgow_centroids <- data_zones %>% 
  st_centroid() %>% 
  filter(st_intersects(., healthboard, sparse = FALSE)) %>% 
  st_transform(4326)
# Head
head(glasgow_centroids)

# Lastly, naming identification column as 'id'
glasgow_centroids <- glasgow_centroids %>% rename(id = DataZone)

# Plot centroids
ggplot() +
  geom_sf(data = healthboard) +
  geom_sf(data = glasgow_centroids, shape = 1) +
  theme_void()

# Routing time parameters --------------------------------------------------

# Mode
mode <- c("WALK", "TRANSIT")
# Time and date of departure
departure_datetime <- as.POSIXct("2022-11-09 08:00:00")

# Max trip duration, Def. 120 (min)
max_trip_duration <- 90
# Walk speed (Km/h), Def. to 3.6 Km/h 
walk_speed <- 4.8
# Max. walk time in minutes
max_walk_time <- 15


# Single travel time matrix -----------------------------------------------

# Single destination: Glasgow Royal Infirmary
royal_infirmary <- 
  data.frame(id = "roy_inf", lon = -4.234407, lat = 55.865749) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

# Estimate simplified travel time distance to a single destination
single_ttm <-
  travel_time_matrix(
    r5r_core = r5r_core, 
    origins = glasgow_centroids, 
    destinations = royal_infirmary, 
    mode = mode,
    departure_datetime = departure_datetime, 
    max_trip_duration = max_trip_duration, 
    walk_speed =  walk_speed, 
    max_walk_time = max_walk_time,
    verbose = FALSE
  )
# Head
dim(single_ttm)
head(single_ttm)

# Define location of Glasgow Royal Infirmary
infirmary_point <- 
  geom_sf(data = royal_infirmary, shape = 21, fill = 'white', size = 2)

# Visualize travel time to Royal Infirmary
data_zones %>% 
  left_join(single_ttm, by = c("DataZone" = "from_id")) %>% 
  drop_na() %>% 
  ggplot() +
  geom_sf(aes(fill = travel_time_p50), col = NA) +
  infirmary_point + 
  scale_fill_viridis_c(direction = -1) +
  theme_void()


# Detailed travel time matrix ---------------------------------------------

# Estimate detailed travel time distance to a single destination
breakdown_ttm <-
  expanded_travel_time_matrix(
    r5r_core = r5r_core,
    origins = glasgow_centroids,
    destinations = royal_infirmary,
    mode = mode,
    departure_datetime = departure_datetime,
    max_trip_duration = max_trip_duration,
    walk_speed =  walk_speed, 
    max_walk_time = max_walk_time, 
    breakdown = TRUE, 
    time_window = 1
  )


# Dimension
dim(breakdown_ttm)
# Head
head(breakdown_ttm)

# Visualize number of rides
data_zones %>% 
  left_join(breakdown_ttm, by = c("DataZone" = "from_id")) %>% 
  drop_na() %>% 
  mutate(n_rides = factor(n_rides)) %>%
  ggplot() +
  geom_sf(aes(fill = n_rides), col = NA) +
  infirmary_point +
  scale_fill_viridis_d(direction = -1) +
  labs(fill = "Rides \n(number)") +
  theme_void()

# Visualize wait time
data_zones %>% 
  left_join(breakdown_ttm, by = c("DataZone" = "from_id")) %>% 
  drop_na() %>% 
  ggplot() +
  geom_sf(aes(fill = wait_time), col = NA) +
  infirmary_point + 
  scale_fill_viridis_b(breaks = seq(0, 15, 5), direction = -1) +
  labs(fill = "Wait time\n(minutes)") +
  theme_void()

# Visualize access/egress time
data_zones %>% 
  left_join(breakdown_ttm, by = c("DataZone" = "from_id")) %>% 
  drop_na() %>% 
  pivot_longer(c('access_time', 'egress_time')) %>% 
  ggplot() +
  geom_sf(aes(fill = value), col = NA) +
  infirmary_point + 
  scale_fill_viridis_b(breaks = seq(0, 15, 5), direction = -1) +
  facet_wrap(~name) +
  labs(fill = 'Minutes') +
  theme_void()


# Destinations: Glasgow hospitals -------------------------------------------

# {osmdata} to get data from OpenStreetMap 
library(osmdata)

# Get data from OSM
hospitals_glasgow <- opq(bbox = 'Glasgow, UK') %>%
  add_osm_feature(key = 'amenity', value = 'hospital') %>% 
  osmdata_sf()
# Represent hospitals with a centroid
hospitals <- hospitals_glasgow$osm_polygons %>% 
  st_centroid()

# Name identification column as 'id'
hospitals <- rename(hospitals, id = osm_id)


# All to all travel time matrix -------------------------------------------

# Time window in minutes
time_window <- 30
# Percentiles
pcts <- c(25, 50, 75)


# Compute travel time matrix (May take several minutes)
ata_ttm <-
  travel_time_matrix(
    r5r_core = r5r_core, 
    origins = glasgow_centroids, 
    destinations = hospitals, 
    mode = mode,
    departure_datetime = departure_datetime,
    walk_speed =  walk_speed, 
    max_walk_time = max_walk_time, 
    time_window = time_window, 
    percentiles = pcts
  )
# Dimensions
dim(ata_ttm)
# Head
head(ata_ttm)

# Find nearest hospital for each percentile
nearest_hospital <- ata_ttm %>% 
  group_by(from_id) %>% 
  summarise(
    travel_time_p25 = min(travel_time_p25, na.rm = TRUE),
    travel_time_p50 = min(travel_time_p50, na.rm = TRUE),
    travel_time_p75 = min(travel_time_p75, na.rm = TRUE)
  )

# Summary
summary(nearest_hospital)

# Plot nearest hospital by percentile
nearest_hospital %>% 
  pivot_longer(-from_id, names_to = "percentil", values_to = "travel_time") %>% 
  left_join(data_zones, by = c("from_id" = "DataZone")) %>%
  st_as_sf() %>% 
  ggplot() +
  geom_sf(aes(fill = travel_time), col = NA) +
  facet_wrap(~percentil) +
  scale_fill_viridis_b(direction = -1, breaks = seq(0, 90, 15)) +
  labs(
    title = 'Closest hopsital by public transport in Glasgow',
    fill = "Travel time \n(Minutes)"
  ) +
  theme_void() +
  theme(legend.position = "bottom")




