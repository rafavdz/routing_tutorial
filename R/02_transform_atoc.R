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


