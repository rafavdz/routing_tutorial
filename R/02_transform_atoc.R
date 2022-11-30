############################################################################
############################################################################
###                                                                      ###
###                      TRANSFORM ATOC (RAIL) DATA                      ###
###                                                                      ###
############################################################################
############################################################################

# Date: December 2022

# Packages ----------------------------------------------------------------

# Uses UK2GTFS V. ‘0.1.1’
# More info: https://itsleeds.github.io/UK2GTFS/articles/ATOC.html
# remotes::install_github("ITSleeds/UK2GTFS")
library(UK2GTFS)
library(tidyverse)


# Transform ATOC (rail data) ----------------------------------------------

# Data source: https://data.atoc.org/?q=user

# Transform ATOC to GTFS
path_in <- "data/20221105_ttis543.zip"
ttis543 <- atoc2gtfs(path_in = path_in, shapes = TRUE, ncores = 14)

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


# Inspect calendar output -------------------------------------------------

# Packages
library(lubridate)
library(tidytransit)

# Read calendar
calendar <- read_delim(
  unzip("data/ttis543.gtfs.zip", "calendar.txt"), 
  col_types = cols(.default = "c")
  )

# Calendar
glimpse(calendar)
summary(parse_date(calendar$start_date, format = "%Y%m%d"))
summary(parse_date(calendar$end_date, format = "%Y%m%d"))
barplot(table(calendar$end_date))

# Define valid interval: between 2022-11-05 and 2022-11-12
valid_int <- interval(ymd("2022-11-05"), ymd("2022-11-12"))

# Examine start/end date of services
ttis543_calendar <- calendar %>% 
  mutate(
    across(c(start_date, end_date),  parse_date, format = "%Y%m%d"),
    days_active = end_date - start_date,
    # Future services (Start from Dec-2022)
    future_service = ifelse(start_date > "2022-12-01", TRUE, FALSE),
    # Expired services (Ended Nov-2022)
    expired_service = ifelse(end_date < "2022-11-07", TRUE, FALSE),
    # Valid on 2022-11-09
    valid_service = ifelse(
      int_overlaps(valid_int, interval(start_date, end_date)),
      TRUE, FALSE
    )
  )

# Days active
hist(as.numeric(ttis543_calendar$days_active))
# Total number of services
n_distinct(ttis543_calendar$service_id)
# Number of future services
ttis543_calendar %>% 
  filter(future_service == TRUE) %>% 
  n_distinct(.$service_id)
# Number of expired services
ttis543_calendar %>% 
  filter(expired_service == TRUE) %>% 
  n_distinct(.$service_id)
# Valid services
ttis543_calendar %>% 
  filter(valid_service == TRUE) %>%
  n_distinct(.$service_id)


# Dates of interest
dates_interest <- seq(as.Date("2022-10-01"), as.Date("2023-06-30"), by="days")
# Start-end interval of services
active_interval <- interval(calendar$start_date, calendar$end_date)

# Compute active services by day
active_services <-
  sapply(dates_interest, function(x){
    x %within% active_interval
  })

# dates_interest[50]
# calendar$active <- active_services[,50]
#View(calendar)

active_services <-
  data.frame(
    date = dates_interest,
    services_active = colSums(active_services)
  )

# Plot number of active services
active_services %>% 
  ggplot(aes(date, services_active)) +
  geom_line()
