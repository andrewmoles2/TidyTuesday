library(tidyverse)
library(ggforce)
library(ggrepel)
library(ggthemes)
library(gghighlight)
library(patchwork)
library(lubridate)
library(here)

ultra_rankings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-26/ultra_rankings.csv')
race <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-26/race.csv')


summary(race_rankings)

race_rankings %>%
  ggplot(aes(distance)) +
  geom_histogram() # need to remove 0 distance races...

# join up rankings to race and break up date for plotting
race_rankings <- left_join(ultra_rankings, race, by = "race_year_id") %>%
  mutate(day = day(date),
         month = month(date),
         year = year(date)) %>%
  filter(distance > 0)
  
# races with the most elevation (hills!)
race_rankings %>%
  group_by(event) %>%
  summarise(mean_elev = mean(elevation_gain, na.rm = TRUE)) %>%
  slice_max(mean_elev, n = 10) %>%
  pull(event) -> high_elev_events

race_rankings %>%
  group_by(event) %>%
  summarise(mean_elev = mean(elevation_gain, na.rm = TRUE),
            mean_dist = mean(distance, na.rm = TRUE),
            mean_age = mean(age, na.rm = TRUE)) %>%
  slice_max(mean_elev, n = 5)

race_rankings %>%
  filter(event %in% high_elev_events) %>%
  group_by(event, year) %>%
  summarise(avg_distance = mean(distance, na.rm = TRUE),
            mean_elev = mean(elevation_gain, na.rm = TRUE),
            mean_age = mean(age, na.rm = TRUE)) %>%
  ggplot(aes(year, mean_age, colour = event)) +
  geom_line() +
  labs(title = "Average age of competitors for most hilly events")

# UK races
race_rankings %>%
  filter(country == "United Kingdom") %>%
  count(event, year) %>%
  count(event) %>%
  filter(n >= 4) %>%
  pull(event) -> uk_races_4years

race_rankings %>%
  filter(event %in% uk_races_4years) %>%
  group_by(year, event) %>%
  summarise(avg_distance = mean(distance, na.rm = TRUE),
            mean_elev = mean(elevation_gain, na.rm = TRUE),
            mean_age = mean(age, na.rm = TRUE)) %>%
  mutate(year = factor(year)) %>%
  ggplot(aes(year, mean_age, colour = event, group = event)) +
  geom_line(size = 2.5,
            arrow = arrow(type = "open")) + 
  geom_point(size = 3) +
  facet_wrap(vars(event), nrow = 4) +
  ggthemes::theme_pander(base_family = "Avenir") +
  ggthemes::scale_colour_colorblind() +
  labs(title = "Average age of participants in UK ultra races",
       subtitle = "Only races that have been running for >4 years selected",
       x = "Year of event",
       y = "Mean age of participants",
       colour = "Name of event",
       caption = "Ultra Trail Running 2021-10-26 A.P.Moles") -> long_standing_uk_races

long_standing_uk_races

ggsave(here("Trail-running-2021-10-26", "long_standing_uk_races.png"),
       long_standing_uk_races, width = 10.2, height = 6.8, dpi = 150)

# ideas: try with gghighlight


