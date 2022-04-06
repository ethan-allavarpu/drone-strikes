rm(list = ls())
library(readxl)
library(tidyverse)

# Read in and standardize data ----
pakistan <- read_xlsx("data/raw/the-guardian/us-pakistan-strikes-from-2004.xlsx",
                      sheet = "Drone strikes data",
                      col_names = c("strike_id", "date", "location", "area",
                                    "min_ppl_killed", "max_ppl_killed",
                                    "min_civil_killed", "max_civil_killed",
                                    "min_child_killed", "max_child_killed",
                                    "min_injured", "max_injured",
                                    "strike_link", "empty", "index"),
                      skip = 1) %>%
  # Add country name
  mutate(country = "Pakistan", .after = location) %>%
  # Remove unhelpful information
  select(!strike_link:index)
write_csv(pakistan, "data/processed/pakistan.csv")

somalia <- read_xlsx("data/raw/the-guardian/us-somalia-strikes-from-2007.xlsx",
                     sheet = "All US actions",
                     col_names = c("strike_id", "date", "location",
                                   "confirm_possible", "air_strike",
                                   "drone_strike", "strike_type",
                                   "min_strikes", "max_strikes",
                                   "min_ppl_killed", "max_ppl_killed",
                                   "min_civil_killed", "max_civil_killed",
                                   "min_child_killed", "max_child_killed",
                                   "min_injured", "max_injured", "strike_link",
                                   "empty"),
                      skip = 1) %>%
  # Add area, country
  mutate(area = NA,
         country = "Somalia",
         .after = location) %>%
  # Remove unhelpful information
  select(!strike_link:empty) %>%
  # Convert dummy variables to logical variables
  mutate_at(c("air_strike", "drone_strike"), .funs = as.logical)
write_csv(somalia, "data/processed/somalia.csv")

yemen <- read_xlsx("data/raw/the-guardian/us-yemen-strikes-from-2007.xlsx",
                   sheet = "All US actions",
                   col_names = c("strike_id", "date", "location", "area",
                                 "strike_type", "confirm_possible",
                                 "air_strike", "drone_strike", "min_strikes",
                                 "max_strikes", "min_ppl_killed",
                                 "max_ppl_killed", "min_civil_killed",
                                 "max_civil_killed", "min_child_killed",
                                 "max_child_killed", "min_injured",
                                 "max_injured", "strike_link", "empty"),
                   skip = 1) %>%
  # Add country name
  mutate(country = "Yemen", .after = location) %>%
  # Remove unhelpful information, reorder columns
  select(!strike_link:empty) %>%
  relocate(strike_type, .after = drone_strike) %>%
  # Convert dummy variables to logical variables
  mutate_at(c("air_strike", "drone_strike"), .funs = as.logical)
write_csv(yemen, "data/processed/yemen.csv")

# Combine individual country data frames
## Additional information in Somalia, Yemen given NA values for Pakistan
strike_data <- bind_rows(pakistan, somalia, yemen) %>%
  arrange(date)
write_csv(strike_data, "data/processed/strike-data.csv")
