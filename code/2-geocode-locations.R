# To run this script, you must have a Google Maps API key
rm(list = ls())
library(tidyverse)
library(ggmap)
library(rworldxtra)
library(sp)

# Change Unknown locations to NA, then replace NAs with empty strings
strike_data <- read_csv("data/processed/strike-data.csv") %>%
  mutate_at("location",
            na_if,
            y = "Unknown") %>%
  replace_na(list(location = ""))

# Paste locations with location and country
locs <- strike_data %>%
  select(location, country) %>%
  mutate(geo_loc = case_when(
    # If no location (i.e., empty space present), keep location as NA
    nchar(location) > 0 ~ paste(location, country, sep = ", ")
  )) %>%
  # Only geocode locations with a location (not just country)
  filter(!is.na(geo_loc)) %>%
  pull(geo_loc)

# Geocode the location with ggmap::geocode to get latitude, longitude
## register_google(key = [Insert API Key Here])
lat_long <- map_dfr(locs, geocode)
latitude_longitude <- matrix(nrow = nrow(strike_data), ncol = 2,
                             dimnames = list(NULL, c("long", "lat")))
location_info <- with(geocode_location, !is.na(geo_loc))
# If no geocoding, keep latitude and longitude as NA
latitude_longitude[location_info, ] <- as.matrix(lat_long)

# Add latitude and longitude to original strike data
strike_data <- bind_cols(strike_data, as_tibble(latitude_longitude))

# Validate the latitude and longitude of API results with spatial data
lat_long <- strike_data %>%
  select(long, lat) %>%
  filter(!is.na(long), !is.na(lat))

# Inspiration from the below Stack Overflow
# https://stackoverflow.com/questions/14334970/convert-latitude-and-longitude-coordinates-to-country-name-in-r
country_boundaries <- getMap(resolution = "high")
strike_spatial <- SpatialPoints(coords = lat_long,
                                proj4string = CRS(" +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
# See if the strike spatial data exists within the country boundaries
strike_matches <- over(strike_spatial, country_boundaries)
# $ADMIN gathers the country name
strike_country <- strike_matches$ADMIN %>%
  as.character() %>%
  # Somaliland and Somalia equivalent (valid data points)
  str_replace(pattern = "Somaliland", replacement = "Somalia")
country_match <- character(length = nrow(strike_data))
country_match[!is.na(strike_data$long)] <- strike_country

strike_data <- strike_data %>%
  mutate(spatial_country = country_match) %>%
  # Only keep latitude and longitude data for properly validated queries
  mutate(
    long = case_when(
      country == spatial_country ~ long
    ),
    lat = case_when(
      country == spatial_country ~ lat
    )
  ) %>%
  mutate(
    president = case_when(
      date < as.Date("2009-01-20") ~ "Bush",
      date < as.Date("2017-01-20") ~ "Obama",
      TRUE ~ "Trump"),
    avg_ppl_killed = (min_ppl_killed + max_ppl_killed) / 2,
    avg_civ_killed = (min_civil_killed + max_civil_killed) / 2
  )

# Write CSV file
write_csv(strike_data, "data/processed/strike-data-geocode.csv")

