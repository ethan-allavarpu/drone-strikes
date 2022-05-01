rm(list = ls())
library(tidyverse)

strike_data <- read_csv("data/processed/strike-data-geocode.csv")

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
                   chisq.test(civ_killed, p = non_civ_killed, rescale.p = TRUE))

# Posthoc Analysis: Look at Pearson residuals
pct_civilian <- pct_civilian %>%
  mutate(expected_civ_deaths = chisq_test$expected,
         chisq_res = chisq_test$residuals)

# Pakistan has higher percentage of civilians killed, Somalia less
## Somalia more strategic?
pct_civilian

# Export table results for report
write_rds(list(chisq_test, pct_civilian), 'data/processed/drone-accuracy.rds')
