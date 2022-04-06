rm(list = ls())
library(lubridate)
library(tidyverse)
library(readxl)

gtd_import <- read_xlsx("data/raw/gtd/gtd.xlsx", col_types = "text")
terrorism <- gtd_import %>%
  filter(country_txt %in% c("Pakistan", "Somalia", "Yemen"),
         iyear %in% as.character(2002:2020)) %>%
  select(eventid:iday,
         country_txt, provstate:longitude,
         nkill, nwound)

# Convert columns to proper type
# Filter observations on country-by-country basis
terrorism <- terrorism %>%
  mutate_at(vars(iyear:iday, latitude:nwound), as.numeric) %>%
  # Has to be within the range for a given country
  ## 2004+ for Pakistan, 2007+ otherwise
  mutate(start_yr = case_when(
    country_txt == "Pakistan" ~ 2004,
    TRUE ~ 2007
  )) %>%
  # Only consider acts of terrorism with deaths and specific date
  ## Cannot have just year
  filter(iyear >= start_yr,
         imonth > 0,
         iday > 0,
         nkill > 0)

terrorism <- terrorism %>%
  mutate(date = paste(iyear, imonth, iday, sep = "-") %>% ymd(),
         .after = iday)
