rm(list = ls())
library(tidyverse)

strike_data <- read_csv("data/processed/strike-data-geocode.csv")

# Average deadliness by president ----
president_death_anova <- aov(log(avg_ppl_killed + 1) ~ president,
                             data = strike_data)
summary(president_death_anova)
posthoc <- TukeyHSD(president_death_anova)

posthoc$president %>%
  as_tibble() %>%
  mutate(comparison = posthoc$president %>% rownames(), .before = diff) %>%
  rename(p_adj = `p adj`) %>%
  select(comparison, p_adj)

strike_data %>%
  group_by(president) %>%
  summarise(avg_death = mean(avg_ppl_killed),
            med_death = median(avg_ppl_killed))

ggplot(strike_data, aes(x = avg_ppl_killed, y = president, color = president)) +
  geom_boxplot() +
  scale_color_manual(values = rgb(c(0.5, 0, 0), c(0, 0.5, 0), c(0, 0, 0.5))) +
  labs(title = 'People Killed per Drone Strike by President',
       x = 'People Killed in Strike', y = 'President') +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        legend.position = 'none')

write_rds(president_death_anova, 'data/processed/lethality-analysis.rds')
