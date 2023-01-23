
############################################################################
############################################################################
###                                                                      ###
###                                PART 2                                ###
###                         TRAVEL TIME MATRICES                         ###
###                                                                      ###
############################################################################
############################################################################

# Date: December 2022

# Set r5r and java pars ---------------------------------------------------

# Set memory to be used
options(java.parameters = "-Xmx4G")
library(r5r)

# R5R Directory
r5r_dir <- 'scotland_router'
# Read network
r5r_core <- setup_r5(data_path = r5r_dir, verbose = TRUE)
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
# Filter Greater Glasgow and Clyde boundary
healthboard <- filter(healthboard, HBName == "Greater Glasgow and Clyde")

# Read data zones
data_zones <- st_read("data/datazones/SG_DataZone_Bdry_2011.shp")
# Define centroids within Glasgow
glasgow_centroids <- data_zones %>% 
  st_centroid() %>% 
  filter(st_intersects(., healthboard, sparse = FALSE)) %>% 
  st_transform(4326)

# Lastly, naming identification column as 'id'
glasgow_centroids <- rename(glasgow_centroids, id = DataZone)


# Travel time parameters --------------------------------------------------

# Mode
mode <- c("WALK", "TRANSIT")
# Time and date of departure
departure_datetime <- as.POSIXct(
  x = "09-11-2022 08:00:00", 
  format = "%d-%m-%Y %H:%M:%S"
)
# Max trip duration, Def. 120 (min)
max_trip_duration <- 90
# Walk speed (Km/h), Def. to 3.6 Km/h 
walk_speed <- 4.8
# Max walk dist
max_walk_dist <- 1000


# Single travel time matrix -----------------------------------------------

# Single destination: Glasgow Royal Infirmary
royal_infirmary <- 
  data.frame(id = "roy_inf", lon = -4.234407, lat = 55.865749)
# As SF
royal_infirmary <-
  st_as_sf(royal_infirmary, coords = c("lon", "lat"), crs = 4326)

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
    max_walk_dist = max_walk_dist,
    verbose = FALSE
)
# Head
dim(single_ttm)
head(single_ttm)

# Location of Glasgow Royal Infirmary
infirmary_point <- 
  geom_sf(data = royal_infirmary, shape = 21, fill = 'white', size = 2)

# Visualize travel time to Royal Infirmary
data_zones %>% 
  left_join(single_ttm, by = c("DataZone" = "fromId")) %>% 
  drop_na() %>% 
  ggplot() +
  geom_sf(aes(fill = travel_time), col = NA) +
  infirmary_point + 
  scale_fill_viridis_c(direction = -1) +
  theme_minimal()


# Detailed travel time matrix ---------------------------------------------

# Estimate detailed travel time distance to a single destination
breakdown_ttm <-
  travel_time_matrix(
    r5r_core = r5r_core,
    origins = glasgow_centroids,
    destinations = royal_infirmary,
    mode = mode,
    departure_datetime = departure_datetime,
    max_trip_duration = max_trip_duration,
    walk_speed = walk_speed,
    max_walk_dist = max_walk_dist,
    breakdown = TRUE,
    verbose = FALSE
  )
# Dimension
dim(breakdown_ttm)
# Head
head(breakdown_ttm)

# Visualize number of rides
data_zones %>% 
  left_join(breakdown_ttm, by = c("DataZone" = "fromId")) %>% 
  drop_na() %>% 
  mutate(n_rides = factor(n_rides)) %>%
  ggplot() +
  geom_sf(aes(fill = n_rides), col = NA) +
  infirmary_point +
  scale_fill_viridis_d(direction = -1) +
  theme_minimal()

# Visualize wait time
data_zones %>% 
  left_join(breakdown_ttm, by = c("DataZone" = "fromId")) %>% 
  drop_na() %>% 
  ggplot() +
  geom_sf(aes(fill = wait_time), col = NA) +
  infirmary_point + 
  scale_fill_viridis_b(breaks = seq(0, 15, 5), direction = -1) +
  theme_minimal()

# Visualize access/egress time
data_zones %>% 
  left_join(breakdown_ttm, by = c("DataZone" = "fromId")) %>% 
  drop_na() %>% 
  pivot_longer(c('access_time', 'egress_time')) %>% 
  ggplot() +
  geom_sf(aes(fill = value), col = NA) +
  scale_fill_viridis_b(breaks = seq(0, 15, 5), direction = -1) +
  facet_wrap(~name) +
  theme_void()


# Destinations: Acute hospitals -------------------------------------------

# Get data
# Hospital locations
hospitals <- read_csv(read_lines('data/hospitals_url.txt'))
# Beds available
beds_nhs <- read_csv(read_lines('data/beds_url.txt'))

# Key for acute hospitals in Scotland 
hosp_key <- beds_nhs %>% 
  filter(
    SpecialtyName == "All Acute Specialties" &
      FinancialYear == '2021/22' &
      AllStaffedBeds > 1000
  ) %>% 
  pull(Location)

# Keep only acute hospitals in Greater Glasgow
hospitals <- hospitals %>% 
  filter(Location %in% hosp_key) 
# Hospitals as SF and CRS WGS84 (4326)
hospitals <- hospitals %>% 
  drop_na(XCoordinate, YCoordinate) %>% 
  st_as_sf(coords = c('XCoordinate', 'YCoordinate'), crs = 27700) %>% 
  filter(st_intersects(., healthboard, sparse = FALSE)) %>% 
  st_transform(4326)

# Name identification column as 'id'
hospitals <- rename(hospitals, id = Location)


# All to all travel time matrix -------------------------------------------

# Time window in minutes
time_window <- 60
# Percentiles
pcts <- c(25, 50, 75)


# Compute travel time matrix (May take several minutes in low spec machines)
ata_ttm <-
  travel_time_matrix(
    r5r_core = r5r_core, 
    origins = glasgow_centroids, 
    destinations = hospitals, 
    mode = mode,
    departure_datetime = departure_datetime,
    walk_speed =  walk_speed, 
    max_walk_dist = max_walk_dist,
    time_window = time_window, 
    percentiles = pcts,
    verbose = FALSE
)
# Dimensions
dim(ata_ttm)
# Head
head(ata_ttm)

# Find nearest hospital for each percentile
nearest_hospital <- ata_ttm %>% 
  group_by(fromId) %>% 
  summarise(
    travel_time_p25 = min(travel_time_p025, na.rm = TRUE),
    travel_time_p50 = min(travel_time_p050, na.rm = TRUE),
    travel_time_p75 = min(travel_time_p075, na.rm = TRUE)
)

# Summary
summary(nearest_hospital)

# Plot nearest hospital by percentile
nearest_hospital %>% 
  pivot_longer(-fromId, names_to = "percentil", values_to = "travel_time") %>% 
  left_join(data_zones, by = c("fromId" = "DataZone")) %>%
  st_as_sf() %>% 
  ggplot() +
  geom_sf(aes(fill = travel_time), col = NA) +
  facet_wrap(~percentil) +
  scale_fill_viridis_b(direction = -1, breaks = seq(0, 90, 15)) +
  theme_minimal() +
  theme(legend.position = "bottom")


# # Detailed matrix -----------------------------------------------------------
# 
# 
# # Detailed route matrix
# detailed_ata <-
#   lapply(1:nrow(hospitals), function(x){
#     r5r::detailed_itineraries(
#       r5r_core = r5r_core, 
#       origins = glasgow_centroids, 
#       destinations = hospitals[x,], 
#       mode = mode,
#       departure_datetime = departure_datetime, 
#       max_trip_duration = max_trip_duration, 
#       walk_speed =  walk_speed, 
#       max_walk_dist = max_walk_dist, 
#       shortest_path = TRUE,
#       verbose = FALSE
#     )
#   }
# )
# # Bind DF
# detailed_ata <- 
#   detailed_ata %>% 
#     purrr::keep(~nrow(.) > 0) %>% 
#     bind_rows()
# 
# # Find nearest
# nearest_hosp <- detailed_ata %>% 
#   group_by(fromId) %>% 
#   slice_min(total_duration)
# 
# # Location of hospitals
# nearest_dz <- data_zones %>% 
#   filter(DataZone %in% unique(nearest_hosp$fromId))
# # Map routes
# nearest_map <- nearest_hosp %>% 
#   left_join(st_set_geometry(hospitals, NULL), by = c("toId" = "id")) %>% 
#   ggplot() +
#   geom_sf(aes(col = LocationName), alpha = 0.5, size = 14) +
#   geom_sf(
#     data = name_hosp, 
#     aes(col = LocationName),
#     fill = "white", pch=21, size = 1.5, stroke = 1.35
#   ) +
#   labs(
#     title  = "Closest acute hospital by public transport in Greater Glasgow Area",
#     col = ""
#   ) +
#   theme_void()
# 
# # Folder for outputs
# dir.create("output")
# 
# # Save map
# ggsave(
#   "output/neares_hospital.png", 
#   nearest_map,
#   height = 6, width = 9,
#   dpi = 400, bg = 'white'
# )
