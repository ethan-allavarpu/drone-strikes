rm(list = ls())
library(lubridate)
library(tidyverse)

# Tidy up scraped tables ----
## Bush
bush_ratings <- read_csv("data/raw/gallup/bush-ratings.csv")
bush <- bush_ratings %>%
  # Add appropriate column names
  rename(period = X1, approve = X2, disapprove = X3, no_opinion = X4) %>%
  select(period:no_opinion) %>%
  # Empty rows indicate non-rating rows
  filter(nchar(approve) > 0,
         nchar(disapprove) > 0,
         nchar(no_opinion) > 0) %>%
  mutate_at(vars(approve:no_opinion), as.numeric) %>%
  # Get the proper subsection of observations
  mutate(section_id = cumsum(is.na(approve))) %>%
  filter(section_id == 3) %>%
  select(-section_id) %>%
  # Remove extraneous rows
  na.omit()

## Obama
obama_ratings <- read_csv("data/raw/gallup/obama-ratings.csv")
obama <- obama_ratings %>%
  # Add appropriate column names
  rename(period = X1, approve = X2, disapprove = X3, no_opinion = X4) %>%
  select(period:no_opinion) %>%
  # Empty rows indicate non-rating rows
  filter(nchar(approve) > 0,
         nchar(disapprove) > 0,
         nchar(no_opinion) > 0) %>%
  # Get the proper subsection of observations
  mutate(section_id = cumsum(approve == "Republicans")) %>%
  filter(section_id == 0) %>%
  select(-section_id) %>%
  mutate_at(vars(approve:no_opinion), as.numeric) %>%
  # Remove extraneous rows
  na.omit()

## Trump
trump_ratings <- read_csv("data/raw/gallup/trump-ratings.csv")
trump <- trump_ratings %>%
  # Add appropriate column names
  rename(period = X1, approve = X2, disapprove = X3, no_opinion = X4) %>%
  select(period:no_opinion) %>%
  # Empty rows indicate non-rating rows
  filter(nchar(approve) > 0,
         nchar(disapprove) > 0,
         nchar(no_opinion) > 0) %>%
  mutate_at(vars(approve:no_opinion), as.numeric) %>%
  # Get the proper subsection of observations
  mutate(section_id = cumsum(is.na(approve))) %>%
  filter(section_id == 3) %>%
  select(-section_id) %>%
  # Remove extraneous rows
  na.omit()

# Transform periods into date columns ----
get_date_range <- function(period) {
  # Split start and end by -
  date_pt <- period %>%
    str_split(pattern = "-") %>%
    unlist() %>%
    # Split date parts by spaces
    str_split(pattern = " ")
  # Second date part has one of the following three:
  ## Just the day: First two elements same as date_pt[[1]], third the day
  ## Month, day: First element same as date_pt[[1]], second, third as month, day
  ### Year, month, day: No elements taken from date_pt[[1]]--remains unchanged
  date_pt[[2]] <- c(date_pt[[1]][seq_len(3 - length(date_pt[[2]]))],
                    date_pt[[2]])
  # Collapse dates, convert to date format
  dates <- date_pt %>%
    map_chr(paste, collapse = " ") %>%
    ymd()
  names(dates) = c("start", "end")
  dates
}
add_date_range <- function(ratings) {
  with(ratings, map_dfr(period, get_date_range)) %>%
    # Add range to original data frame, after period
    bind_cols(ratings) %>%
    relocate(start, end, .after = period)
}

bush <- add_date_range(bush)
write_csv(bush, "data/processed/bush-approval-ratings.csv")
obama <- add_date_range(obama)
write_csv(obama, "data/processed/obama-approval-ratings.csv")
trump <- add_date_range(trump)
write_csv(trump, "data/processed/trump-approval-ratings.csv")

all_ratings <- bush %>% mutate(president = "Bush") %>%
  bind_rows(obama %>% mutate(president = "Obama"),
            trump %>% mutate(president = "Trump"))
write_csv(all_ratings, "data/processed/all-approval-ratings.csv")
