rm(list = ls())
library(tidyverse)

strike_data <- read_csv("data/processed/strike-data-geocode.csv")

# Pakistan: Bush vs. Obama
bush_obama <- strike_data %>%
  filter(country == "Pakistan",
         date > "2005-01-21",
         date < "2013-01-21")
bush_obama %>%
  mutate(is_obama = str_detect(strike_id, "Ob")) %>%
  group_by(is_obama, area) %>%
  summarise(n_strikes = n())


# Civilian Death Percentages by Country
pct_civilian <- strike_data %>%
  mutate(avg_ppl_killed = (min_ppl_killed + max_ppl_killed) / 2,
         avg_civ_killed = (min_civil_killed + max_civil_killed) / 2) %>%
  group_by(country) %>%
  summarise(ppl_killed = sum(avg_ppl_killed),
            civ_killed = sum(avg_civ_killed),
            non_civ_killed = ppl_killed - civ_killed) %>%
  mutate(pct_civ = civ_killed / ppl_killed)

# Expected probabilities would be the distribution of non-civilian deaths
chisq_test <- with(pct_civilian,
                   chisq.test(civ_killed,
                              p = non_civ_killed / sum(non_civ_killed)))

# Posthoc Analysis: Look at Pearson residuals
pct_civilian <- pct_civilian %>%
  mutate(expected_civ_deaths = chisq_test$expected,
         chisq_res = chisq_test$residuals)

# Pakistan has higher percentage of civilians killed, Somalia less
## Somalia more strategic?
## Might consider blocking by president
pct_civilian
