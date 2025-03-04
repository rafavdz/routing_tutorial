
###########################################################################
###########################################################################
###                                                                     ###
###                 DETAILED ROUTE ITINERARIES WITH R5R                 ###
###                                                                     ###
###########################################################################
###########################################################################

# Date updated: Feb 2025

# Set r5r and Java pars ---------------------------------------------------

# This tutorial uses R5R V. 2.0.0
# R5R requires JDK version is 21

# We install necessary R packages
install.packages('r5r')
install.packages('rJavaEnv')
# And respective Java version
rJavaEnv::java_quick_install(version = 21)

# Allocate RAM to Java 
options(java.parameters = "-Xmx4G")
# Load R5R
library(r5r)


# Build/Load network ------------------------------------------------------


# Define R5R directory
r5r_dir <- 'glasgow_router'

# Create directory to store input data and network
dir.create(r5r_dir)
# Get input files
input_files <- list.files(
  'data', 
  recursive = TRUE,
  pattern = 'gtfs\\.zip$|pbf$',
  full.names = TRUE
)
# List input files
input_files
# Copy input files
lapply(input_files, function(x)
  file.copy(x, paste0(r5r_dir, '/', basename(x)))
)

# Build/read multimodal transport network
# Indicate the path where OSM and GTFS data are stored
# This may take few minutes
r5r_core <- setup_r5(data_path = r5r_dir, verbose = TRUE)

gc(reset = TRUE)



# Single Origin-Destination points ----------------------------------------

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
departure_datetime <- as.POSIXct("2022-11-09 08:00:00")

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
mapview(pedestrian_route, zcol = "mode", lwd = 5)


# Cycling route -----------------------------------------------------------

# Mode
mode <-c("BICYCLE")
# bike_speed, defaults to 12 Km/h
bike_speed <- 16.5
# Level of traffic stress (lts)
# 1 high - 4 low, Default is 2
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

# Print results total duration/distance by stress level
bike_route[,c('stress_level', 'total_duration', 'distance')]

# Map route
mapview(bike_route, zcol = "stress_level", lwd = 4)


# Public transport route --------------------------------------------------

# Routing inputs
# Mode
mode <- c("WALK", "TRANSIT")
# Max. access/egress walk time in minutes
max_walk_time <- 15

# Estimate fastest PT route - fixed departure time
pt_route <- 
  detailed_itineraries(
    r5r_core = r5r_core, 
    origins = glasgow_central, 
    destinations = milngavie,
    mode = mode,
    departure_datetime = departure_datetime, 
    max_trip_duration = max_trip_duration, 
    walk_speed =  walk_speed, 
    max_walk_time = max_walk_time
  )
# Print route
pt_route
# Map route
mapview(pt_route, zcol = "mode", lwd = 5)


# Define a time window
time_window <- 30

# Estimate PT options - Flexible departure time
pt_multiple <-
  detailed_itineraries(
    r5r_core = r5r_core,
    origins = glasgow_central,
    destinations = milngavie,
    mode = mode,
    departure_datetime = departure_datetime,
    max_trip_duration = max_trip_duration,
    max_walk_time = max_walk_time,
    walk_speed =  walk_speed,
    time_window = time_window,
    # If shortest_path is FALSE, it will return all options available
    # If it is TRUE, it will return the fastest
    shortest_path = FALSE
  )

# Print routes
pt_multiple

# Options as factor
pt_multiple$option <- factor(pt_multiple$option)
# Number of options
n_distinct(pt_multiple$option)
# Time of departure
sort(unique(pt_multiple$departure_time))
# Total duration
sort(unique(pt_multiple$total_duration))

# Map routes
mapview(pt_multiple, zcol = "option", lwd = 3)


# Map all modes ---------------------------------------------------------------

# Alternative modes
mapview(bike_route, color = "#277f8e", layer.name = "Bicycle") +
  mapview(pedestrian_route, color = "#fde725", layer.name = "Pedestrian") +
  mapview(pt_route, color = "#440154", layer.name = "PT")

# Clean environment (removes all objects)
rm(list = ls())
