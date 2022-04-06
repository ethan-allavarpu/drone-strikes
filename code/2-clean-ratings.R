rm(list = ls())
library(lubridate)
library(tidyverse)

president_ratings <- map(list.files("data/raw/ucsb", full.names = TRUE),
                         read_csv)
names(president_ratings) <- c("bush", "obama", "trump")

tidy_ratings <- function(pres_rating, pres_name) {
  fixed_ratings <- pres_rating %>%
    # Easier names to use
    rename(start = `Start Date`,
           end = `End Date`,
           approve = Approving,
           disapprove = Disapproving,
           unsure = `Unsure/NoData`) %>%
    # Convert start, end to date formats
    mutate_at(vars(start, end), mdy) %>%
    # Add president of interest
    mutate(president = pres_name) %>%
    na.omit()
  write_csv(fixed_ratings,
            paste0("data/processed/", pres_name, "-approval-ratings.csv"))
  fixed_ratings
}

# Write each individual president's CSV file
map2_dfr(president_ratings, names(president_ratings), tidy_ratings) %>%
  # Write a joint CSV file with all approval ratings
  write_csv("data/processed/approval-ratings.csv")
