rm(list = ls())
library(rvest)
library(tidyverse)

# Scrape approval ratings and save to CSV files for further processing
scrape_ratings <- function(link, president) {
  session(link) %>%
    html_node("#block-system-main > div > div > div > div.field-body > table") %>%
    html_table() %>%
    write_csv(paste0("data/raw/ucsb/", president, "-ratings.csv"))
}

# Links for each president's approval ratings
president_links <- paste0("presidency.ucsb.edu/statistics/data/",
                          c("george-w-bush", "barack-obama", "donald-j-trump"),
                          "-public-approval")
presidents <- c("bush", "obama", "trump")
# Write CSV files for each president's ratings
map2(president_links, presidents, scrape_ratings)
