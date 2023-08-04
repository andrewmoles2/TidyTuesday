# inspiration
# https://github.com/doehm/tidytues/tree/main/scripts/2023/week-28-temperatures
# https://github.com/nrennie/tidytuesday/tree/main/2023/2023-07-11
# cite Dan Oehm and Nicola Rennie

# load data ----
global_temps <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-11/global_temps.csv')
nh_temps <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-11/nh_temps.csv')
sh_temps <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-11/sh_temps.csv')
zonann_temps <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-11/zonann_temps.csv')

# libraries ----
library(tidyverse)
library(ggtext)
library(showtext)
library("ggchicklet")
library(here)

global_temps |> glimpse()

global_temps_long <- global_temps %>%
  pivot_longer(cols = Jan:Dec, names_to = 'month') %>%
  group_by(Year) %>%
  summarise(avg_temp_diff = mean(value, na.rm = TRUE))

pal <- c(low = "#08306B", mid = "#DEEBF7", high = "#67000D")

global_temps_long %>%
  ggplot(aes(x = Year, y = avg_temp_diff)) +
  geom_chicklet(aes(fill = avg_temp_diff), width = 1, 
                radius = grid::unit(3, "pt"), show.legend = FALSE) +
  geom_hline(yintercept = 0, linetype = 5, colour = "grey60") +
  scale_fill_gradient2(low = pal[1],mid = pal[2], high = pal[3]) +
  theme(legend.position = "none") +
  theme_minimal()

# ideas: add x labels to hline, and remove everything else. Then add plot title in middle of text, using ggtext to tell story of colours
yr <- seq(min(global_temps_long$Year), max(global_temps_long$Year), 10)

yr_df <- global_temps_long %>%
  filter(Year %in% yr)

temp_hist <- global_temps_long %>%
  ggplot(aes(x = Year, y = avg_temp_diff)) +
  geom_chicklet(aes(fill = avg_temp_diff), width = 1, 
                radius = grid::unit(3, "pt"), show.legend = FALSE) +
  geom_text(data = yr_df, mapping = aes(Year, 0, label = Year)) +
  scale_fill_gradient2(low = pal[1],mid = pal[2], high = pal[3]) +
  theme(legend.position = "none") +
  theme_void()

temp_hist

ggsave(here("global-temp-2023-07-11", "temp_hist.png"), temp_hist,
       dpi = 320, width = 13, height = 9, device = ragg::agg_png)
