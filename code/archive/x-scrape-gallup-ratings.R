rm(list = ls())
library(tidyverse)
library(rvest)

# Scrape approval rating tables ----
get_ratings <- function(site) {
  site %>%
    html_nodes('#main > div > article > div.o-article__inner > section.section--default.section-default.section-rel--mcb') %>%
    html_table()
}
## Bush
bush <- session('https://news.gallup.com/poll/116500/presidential-approval-ratings-george-bush.aspx')
bush_ratings <- get_ratings(bush)[[1]]
write_csv(bush_ratings, "data/raw/gallup/bush-ratings.csv")

## Obama
obama <- session("https://news.gallup.com/poll/116479/barack-obama-presidential-job-approval.aspx")
obama_ratings <- get_ratings(obama)[[1]]
write_csv(obama_ratings, "data/raw/gallup/obama-ratings.csv")

## Trump
trump <- session("https://news.gallup.com/poll/203198/presidential-approval-ratings-donald-trump.aspx")
trump_ratings <- get_ratings(trump)[[1]]
write_csv(trump_ratings, "data/raw/gallup/trump-ratings.csv")

