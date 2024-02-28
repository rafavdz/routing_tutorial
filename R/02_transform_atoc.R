############################################################################
############################################################################
###                                                                      ###
###                      TRANSFORM ATOC (RAIL) DATA                      ###
###                                                                      ###
############################################################################
############################################################################

# Date: December 2022

# THIS CODE IS FOR REFERECE ONLY
# YOU DO NOT NEED TO RUN THIS SCRIPT AS DATA ALREADY EXIST IN REPOSITORY


# Packages ----------------------------------------------------------------

# Uses UK2GTFS V. ‘0.1.1’
# More info: https://itsleeds.github.io/UK2GTFS/articles/ATOC.html
# remotes::install_github("ITSleeds/UK2GTFS")
library(UK2GTFS)
library(tidyverse)
library(gtfstools)
library(sf)


# Transform ATOC (rail data) ----------------------------------------------

# Requires manual download
# Source: https://data.atoc.org/?q=user

# Detect number of cores
n_cores <- parallel::detectCores() -1

# Transform ATOC to GTFS
path_in <- "data/20221105_ttis543.zip"
ttis543 <- atoc2gtfs(path_in = path_in, shapes = TRUE, ncores = n_cores)

# Save as GTFS ------------------------------------------------------------

# Check internal validity
UK2GTFS::gtfs_validate_internal(ttis543)

## Force valid. This function does not fix problems, it just removes them
ttis543_gtfs <- UK2GTFS::gtfs_force_valid(ttis543)

## Compare original and valid
# Find difference
map2(ttis543, ttis543_gtfs, identical)
# Stops not included in GTFS version
gtfs_diff <- anti_join(ttis543$stop_times, ttis543_gtfs$stop_times)
gtfs_diff
# Stops missing
unique(gtfs_diff$stop_id)
# Frequency
count(gtfs_diff, stop_id)
# Trips affected
unique(gtfs_diff$trip_id)

## Write as GTFS
UK2GTFS::gtfs_write(ttis543_gtfs, folder = 'data/', name = 'ttis543.gtfs')

# Clean env.
rm(list = ls())
gc(reset = TRUE)


# Filter relevant train services ------------------------------------------

# Full GTFS
ttis543_gtfs <- read_gtfs('data/ttis543.gtfs.zip')

# Define Glasgow area
glasgow_boundary <- data.frame(x = -4.258950, y = 55.862028) %>% 
  st_as_sf(coords = c('x', 'y'), crs = 4326) %>% 
  st_buffer(30e3)

# Filter stops in Glasgow
stops_glasgow <- gtfstools::convert_stops_to_sf(ttis543_gtfs) 
stops_glasgow <- stops_glasgow[glasgow_boundary,]

# Filter services connected to Glasgow
ttis_glasgow <- filter_by_stop_id(ttis543_gtfs, unique(stops_glasgow$stop_id))

# Write Glasgow train services
write_gtfs(ttis_glasgow, 'data/glasgow_rail.gtfs.zip')

# Remove full GTFS rail file
unlink('data/ttis543.gtfs.zip')


