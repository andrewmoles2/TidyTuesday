# ideas and code from https://github.com/z3tt/30DayMapChallenge/blob/master/contributions/Day04_Hexagons/4_Hexagons.Rmd
# https://github.com/Aklongmuir/TidyTuesday/blob/main/W2/Week%202%20-%20Bee%20Colony%20Losses.R
# thanks Cedric Scherer for origin
library(tidyverse)
library(broom)
library(geojsonio)
library(rgdal)
library(rgeos)
library(patchwork)
library(showtext)
library(ggthemes)
library(here)

# load in TT data ----
colony <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-11/colony.csv')
stressor <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-11/stressor.csv')

# data on states (from Cedric Scherer)
#states_data <- readr::read_csv("https://raw.githubusercontent.com/z3tt/30DayMapChallenge/master/data/50_us_states_all_data.csv")

# prep map lat and long from hex ----
map_hex <- geojson_read("https://raw.githubusercontent.com/z3tt/30DayMapChallenge/master/data/us_states_hexgrid.geojson.json", what = "sp")
# clean up names
map_hex@data <- map_hex@data %>%
  mutate(google_name = gsub(" \\(United States\\)", "", google_name))
# tidy using broom to put into df
map_hex_fort <- tidy(map_hex, region = "google_name")

# calculate hex centres
centres <- cbind.data.frame(data.frame(gCentroid(map_hex, byid = TRUE), id = map_hex@data$iso3166_2))

# average survival for all years (adapted from year by year to total summary) ----
avg_colony_lost <- colony %>%
  group_by(year,state) %>%
  mutate(sum_loss = sum(colony_lost_pct)) %>%
  summarise(year,state, survived_pct = ((100 - sum_loss)/100)) %>%
  unique() %>%
  ungroup() %>%
  group_by(state) %>%
  summarise(avg_survived_pct = mean(survived_pct, na.rm = T))

# add missing states
state <-c("Alaska", "Rhode Island","New Hampshire", "Delaware", "District of Columbia", "Nevada")
avg_survived_pct <- NA
miss_dat <- data.frame(state, avg_survived_pct)

avg_colony_lost <- rbind(avg_colony_lost, miss_dat)

# plot eg ----
ggplot() +
  geom_polygon(data = map_hex_fort, aes(x = long, y = lat, group = group),
               fill = "skyblue", colour = "white") +
  geom_text(data = centres, aes(x = x, y = y, label = id)) +
  theme_map()

ggplot() +
  geom_polygon(data = map_hex_fort, aes(x = long, y = lat, group = group),
               fill = "skyblue", colour = "white") +
  geom_text(data = centres, aes(x = x, y = y, label = id)) +
  theme_void() +
  coord_map()

# join bees with map ----
chart_df <- map_hex_fort %>%
  left_join(., avg_colony_lost, by = c("id"="state"))

# build plot ----
ggplot() +
  geom_polygon(data = chart_df, colour = "#ffffff", 
               aes(fill = avg_survived_pct, x = long, y = lat,
                   group = group)) +
  geom_text(data = centres, aes(x = x, y = y, label = id), colour = "white", 
            size = 3, alpha = 1) +
  scale_fill_gradient(trans = "log",low = "#fdea8b", high = "#fb9a0e",
                      na.value = "#d3d3d3", breaks = c(1,0.75,0.5,0.25,0)) +
  theme_map(base_family = "Avenir", base_size = 10) +
  theme(legend.position = "bottom", legend.key.width = unit(3, "line")) +
  labs(title = "Bee colony loss averages from 2015 to 2021 in US States",
       fill = "Average survivial %",
       caption = "Tidy Tuesday 2022-01-11 | APM") -> bees

ggsave(here("Bees-2022-01-11","bee_loss.png"), bees, 
       width = 8, height = 6, bg = "white")
