rm(list = ls())
library(tidyverse)
library(lubridate)

strike_data <- read_csv("data/processed/strike-data-geocode.csv")

# Range of casualties over time ----
strike_data %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = cumsum(min_ppl_killed)), col = "darkred") +
  geom_line(aes(y = cumsum(max_ppl_killed)), col = "darkred") +
  geom_ribbon(aes(ymin = cumsum(min_ppl_killed), ymax = cumsum(max_ppl_killed)),
              fill = rgb(0.5, 0, 0), alpha = 0.5) +
  labs(title = "Cumulative Deaths from Drone Strikes",
       x = "Year",
       y = "Total Deaths from Drone Strikes")

## Split range by country
strike_data %>%
  group_by(country) %>%
  mutate(country_min_killed = cumsum(min_ppl_killed),
         country_max_killed = cumsum(max_ppl_killed)) %>%
  ggplot(aes(x = date, color = country, fill = country)) +
  geom_line(aes(y = country_min_killed)) +
  geom_line(aes(y = country_max_killed)) +
  scale_color_manual(values = rgb(c(0.5, 0, 0), c(0, 0.5, 0), c(0, 0, 0.5))) +
  scale_fill_manual(values = rgb(c(0.5, 0, 0), c(0, 0.5, 0), c(0, 0, 0.5),
                                 alpha = 0.5)) +
  geom_ribbon(aes(ymin = country_min_killed, ymax = country_max_killed)) +
  labs(title = "Cumulative Deaths from Drone Strikes",
       x = "Year",
       y = "Total Deaths from Drone Strikes",
       fill = "Country",
       color = "Country") +
  facet_wrap(vars(country), nrow = 2)

# Plot drone strike locations ----
country_maps <- map_data("world") %>%
  # Bound the countries to plot based on latitude and longitude
  # Boundaries quasi-arbitrary: Used the "eye test" for a manual cutoff
  filter(long > 40, long < 80,
         lat > -5, lat < 40,
         !(region %in% c("Turkey", "China")))
ggplot() +
  geom_rect(aes(xmin = 40, xmax = 80, ymin = -5, ymax = 40),
            fill = "lightblue", colour = "black") +
  # Turkey's region should be grey
  geom_rect(aes(xmin = 40, xmax = 45, ymin = 35, ymax = 40),
            fill = "lightgrey", colour = "black") +
  # China's region should be grey
  geom_rect(aes(xmin = 70, xmax = 80, ymin = 30, ymax = 40),
            fill = "lightgrey", colour = "black") +
  # Add the countries within the region
  geom_polygon(aes(x = long, y = lat, group = group), country_maps,
               fill = "lightgrey", colour = "black") +
  # Plot drone strike locations
  geom_point(aes(x = long, y = lat, colour = country), data = strike_data) +
  scale_color_manual(values = rgb(c(0.5, 0, 0), c(0, 0.5, 0), c(0, 0, 0.5))) +
  labs(title = "Location of Drone Strikes",
       x = "Longitude",
       y = "Latitude",
       colour = "Country of Strike") +
  coord_equal() +
  theme_minimal() +
  theme(title = element_text(size = 16),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        panel.grid = element_blank())


# Trends in Presidential Approval Rating ----
approval_ratings <- read_csv("data/processed/approval-ratings.csv")
approval_over_terms <- approval_ratings %>%
  group_by(president) %>%
  mutate(years_since_start = as.numeric(start - min(start)) / 365)
approval_over_terms %>%
  ggplot(aes(x = years_since_start)) +
  geom_line(aes(y = approve), col = "darkgreen") +
  geom_line(aes(y = disapprove), col = "darkred") +
  labs(title = "Approval and Disapproval Ratings from Term Start",
       x = "Years Since Start of Term",
       y = "Rating (%)") +
  facet_wrap(vars(president %>% str_to_title()), nrow = 3)

# Relationship between Approval Rating and Strikes
approval_ratings <- read_csv('data/processed/approval-ratings.csv')

approval_shift <- approval_ratings %>%
  select(start:approve, president) %>%
  arrange(start) %>%
  group_by(president) %>%
  mutate(approve_shift = approve - lag(approve, n = 1)) %>%
  ungroup()

# Get the approval shift for the rating period just after the strike date
get_shift <- function(strike_date, approval_df) {
  approval_df %>%
    # Only look at start dates after the strike
    filter(start > strike_date) %>%
    # Get the next strike date
    slice_head(n = 1) %>%
    # Extract the approval rating change
    pull(approve_shift)
}

strike_info <- strike_data %>%
  mutate(approval_change = map_dbl(date,
                                   get_shift,
                                   approval_df = approval_shift))

summary(strike_info$approval_change)

ggplot(strike_info, aes(x = approval_change)) +
  geom_bar(fill = 'darkblue') +
  labs(title = 'Approval Changes After Drone Strikes',
       x = 'Approval Change', y = 'Count')

ggplot(strike_info, aes(x = approval_change, color = president)) +
  geom_density() +
  labs(title = 'Approval Changes After Drone Strikes by President',
       x = 'Approval Change', y = 'Count',
       color = 'President')

ggplot(strike_info, aes(x = date, y = approval_change)) +
  geom_line()  +
  labs(title = 'Approval Changes After Drone Strikes over Time',
      x = 'Date (Year)', y = 'Approval Change')

ggplot(strike_info, aes(x = avg_civ_killed, y = approval_change)) +
  geom_point() +
  geom_jitter() +
  labs(title = 'Approval Changes vs. Civilian Strike Deaths',
       x = 'Number of Civilian Deaths', y = 'Approval Change')

# No relationship between strikes and approval ratings
