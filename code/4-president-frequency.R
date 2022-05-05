rm(list = ls())
library(tidyverse)

strike_data <- read_csv("data/processed/strike-data-geocode.csv")

# Strike frequency ----
## Only consider dates with all three countries having strikes (overlapping range)
country_range <- strike_data %>%
  filter(date > as.Date("2004-01-01")) %>%
  group_by(country) %>%
  summarise(first_date = min(date),
            last_date = max(date)) %>%
  ungroup() %>%
  summarise(latest_start = max(first_date),
            earliest_end = min(last_date))
n_strikes <- strike_data %>%
  filter(date > country_range$latest_start,
         date < country_range$earliest_end) %>%
  group_by(president) %>%
  summarise(first_date = min(date),
            last_date = max(date),
            # n_days represents days as president
            n_days = last_date - first_date,
            # n_strike represents the number of strikes under a given president
            n_strike = n()) %>%
  mutate_at("n_days", as.numeric) %>%
  # pct_days represents the percent of days a given president was commander
  mutate(pct_days = n_days / sum(n_days))

n_strikes

# Did presidents strike the respective regions equally frequently?
# Cannot compare Bush because only Pakistan data available before 2007
## If not, which presidents executed more strikes?
chisq_test <- with(n_strikes, chisq.test(n_strike, p = pct_days))
chisq_test

# Results not statistically significant
## Fail to reject null that different strike frequencies
write_rds(chisq_test, 'data/processed/strike-frequency.rds')
