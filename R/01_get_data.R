############################################################################
############################################################################
###                                                                      ###
###                            GET INPUT DATA                            ###
###                                                                      ###
############################################################################
############################################################################

# Date: December 2022

# THIS CODE IS FOR REFERECE ONLY
# YOU DO NOT NEED TO RUN THIS SCRIPT AS DATA ALREADY EXIST IN REPOSITORY


# Packages ----------------------------------------------------------------

# osmextract version ‘0.4.1’
library(osmextract)
library(mapview)


# Road and pedestrian network ---------------------------------------------

# PBF road network for Glasgow
glasgow_url <- oe_match('Glasgow')
# Download  PBF
oe_download(file_url = glasgow_url$url, download_directory = "./data/")

# Visualize
glasgow_pbf <- osmextract::oe_read('data/bbbike_Glasgow.osm.pbf')
mapview(glasgow_pbf)


# Bus time table data -----------------------------------------------------

# Bus open data (BOD) for Scotland (GTFS format)
url <- "https://data.bus-data.dft.gov.uk/timetable/download/gtfs-file/scotland/"
download.file(url = url, destfile = "data/20221105_itm_scotland_gtfs.zip")

