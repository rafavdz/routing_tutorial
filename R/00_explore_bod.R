
# Explore the calendar of BOD for Scotland

# This code simply explores the dates active in the calendar of the feed.

# Packages
library(tidytransit)
library(tidyverse)
library(lubridate)
library(mapview)


# Read BOD files ----------------------------------------------------------

bod_sct <- read_gtfs('data/20221105_itm_scotland_gtfs.zip')
summary(bod_sct)


# Inspect calendar  -------------------------------------------------

# Calendar
glimpse(bod_sct$calendar)
summary(bod_sct$calendar$start_date)
summary(bod_sct$calendar$end_date)
barplot(table(bod_sct$calendar$end_date))

# Define valid interval: between 2022-11-05 and 2022-11-12
valid_int <- interval(ymd("2022-11-05"), ymd("2022-11-12"))

# Examine start/end date of services
bod_sct_calendar <- bod_sct$calendar %>% 
  mutate(
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
hist(as.numeric(bod_sct_calendar$days_active))
# Total number of services
n_distinct(bod_sct_calendar$service_id)
# Number of future services
bod_sct_calendar %>% 
  filter(future_service == TRUE) %>% 
  n_distinct(.$service_id)
# Number of expired services
bod_sct_calendar %>% 
  filter(expired_service == TRUE) %>% 
  n_distinct(.$service_id)
# Valid services
bod_sct_calendar %>% 
  filter(valid_service == TRUE) %>%
  n_distinct(.$service_id)

count(bod_sct$calendar, end_date, sort = TRUE)


# Map
# To SF
bod_sct <- gtfs_as_sf(bod_sct)


mapview(bod_sct$stops)



tidytransit::validate_gtfs(bod_sct) %>% 
  View()



# Explore active services -------------------------------------------------


# Dates of interest
dates_interest <- seq(as.Date("2022-10-01"), as.Date("2023-12-31"), by="days")
# Start-end interval of services
active_interval <- interval(bod_sct$calendar$start_date, bod_sct$calendar$end_date)

# Compute active services by day
active_services <-
  sapply(dates_interest, function(x){
    x %within% active_interval
  })
active_services <-
  data.frame(
    date = dates_interest,
    services_active = colSums(active_services)
  )

# Plot number of active services
active_services %>% 
  ggplot(aes(date, services_active)) +
  geom_line()

