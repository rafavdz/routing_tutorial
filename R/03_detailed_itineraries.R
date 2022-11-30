
###########################################################################
###########################################################################
###                                                                     ###
###                 DETAILED ROUTE ITINERARIES WITH R5R                 ###
###                                                                     ###
###########################################################################
###########################################################################

# Date: December 2022


# Set r5r and java pars ---------------------------------------------------

# Uses R5R V. ‘0.7.1’
# Requires JDK version is 11
# Verify Java version
system("java -version")

# Allocate RAM to Java 
options(java.parameters = "-Xmx62G")
# Load R5R
library(r5r)


# Build/Load network ------------------------------------------------------


# Define R5R directory
r5r_dir <- 'scotland_router'

# # Create directory to store input data and network
# dir.create(r5r_dir)

# # Copy input files
# input_files <- list.files(
#   'data', recursive = TRUE, 
#   pattern = 'gtfs\\.zip$|pbf$', 
#   full.names = TRUE
# )
# lapply(input_files, function(x) 
#   file.copy(x, paste0(r5r_dir, '/', basename(x)), overwrite = TRUE)
# )


# Build/read multimodal transport network
# Indicate the path where OSM and GTFS data are stored
r5r_core <- setup_r5(data_path = r5r_dir, verbose = TRUE)
gc(reset = TRUE)


# Points in Glasgow -------------------------------------------------------

library(sf)
library(tidyverse)
library(mapview)

# Point of origin
glasgow_central <- 
  data.frame(id = "glw_central", lat = 55.860611, lon = -4.257790)
# Point of destination
milngavie <-
  data.frame(id = "milngavie", lat = 55.941352, lon = -4.317884)


# Pedestrian route --------------------------------------------------------

# Routing inputs
# Mode
mode <-c("WALK")
# Max trip duration, Def. 120 (min)
max_trip_duration <- 4*60L
# Walk speed (Km/h), Def. to 3.6 Km/h 
walk_speed <- 4.8
# Time and date of departure
departure_datetime <- as.POSIXct(
  x = "09-11-2022 08:00:00", 
  format = "%d-%m-%Y %H:%M:%S"
)

# Estimate pedestrian route
pedestrian_route <- 
  detailed_itineraries(
    r5r_core = r5r_core, 
    origins = glasgow_central, 
    destinations =  milngavie, 
    mode = mode,
    departure_datetime = departure_datetime, 
    max_trip_duration = max_trip_duration, 
    walk_speed =  walk_speed
  )

# Class
class(pedestrian_route)

# Print route
pedestrian_route

# Map route
mapview(pedestrian_route, zcol = "mode", lwd = 4)


# Cycling route -----------------------------------------------------------

# Mode
mode <-c("BICYCLE")
# bike_speed, Def. to 12 Km/h
bike_speed <- 16.5
# Level of traffic stress (lts)
# 1 low - 4 high, Default is 2
bike_stress <- c(1, 2, 3, 4)


# Estimate bike route for different stress levels
bike_route <- 
  lapply(bike_stress, function(bsl)
    detailed_itineraries(
      r5r_core = r5r_core, 
      origins = glasgow_central, 
      destinations =  milngavie, 
      mode = mode,
      departure_datetime = departure_datetime, 
      max_trip_duration = max_trip_duration, 
      bike_speed = bike_speed, 
      max_lts = bsl
    )
)
# Bind results
bike_route <- bind_rows(bike_route, .id = "stress_level")
bike_route

# Map route
mapview(bike_route, zcol = "stress_level", lwd = 4)


# Public transport route --------------------------------------------------


# Routing inputs
# Mode
mode <- c("WALK", "TRANSIT")
# Max. access/egress walk dist.
max_walk_dist <- 1500

# Estimate PT route - Multiple options
pt_route <- 
  detailed_itineraries(
    r5r_core = r5r_core, 
    origins = glasgow_central, 
    destinations = milngavie, 
    mode = mode,
    departure_datetime = departure_datetime, 
    max_trip_duration = max_trip_duration, 
    walk_speed =  walk_speed, 
    max_walk_dist = max_walk_dist,
    shortest_path = FALSE
)

# Print routes
pt_route
# Number of options
n_distinct(pt_route$option)
# Map route
mapview(pt_route, zcol = "option", lwd = 3)


# Estimate best PT route
pt_route <- 
  detailed_itineraries(
    r5r_core = r5r_core, 
    origins = glasgow_central, 
    destinations = milngavie,
    mode = mode,
    departure_datetime = departure_datetime, 
    max_trip_duration = max_trip_duration, 
    walk_speed =  walk_speed, 
    max_walk_dist = max_walk_dist,
    shortest_path = TRUE
  )
# Print route
pt_route
# Map route
mapview(pt_route, zcol = "mode", lwd = 3)


# Map modes ---------------------------------------------------------------

# Alternative modes
mapview(pedestrian_route, color = "red", layer.name = "Pedestrian") +
  mapview(bike_route, color = "green", layer.name = "Bicycle") +
  mapview(pt_route, color = "orange", layer.name = "PT")

