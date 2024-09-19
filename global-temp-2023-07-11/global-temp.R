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
library(lubridate)

global_temps |> glimpse()

# prep and build histogram over years ----
global_temps_long <- global_temps %>%
  pivot_longer(cols = Jan:Dec, names_to = 'month') %>%
  group_by(Year) %>%
  summarise(avg_temp_diff = mean(value, na.rm = TRUE))

pal <- c('#08306b', '#08519c', '#2171b5', '#4292c6', '#6baed6', '#9ecae1', '#c6dbef', '#deebf7', '#ffffff',
         '#fee0d2', '#fcbba1', '#fc9272', '#fb6a4a', '#ef3b2c', '#cb181d', '#a50f15', '#67000d')

global_temps_long %>%
  ggplot(aes(x = Year, y = avg_temp_diff)) +
  geom_chicklet(aes(fill = avg_temp_diff), width = 1, 
                radius = grid::unit(3, "pt"), show.legend = FALSE) +
  geom_hline(yintercept = 0, linetype = 5, colour = "grey60") +
  scale_fill_gradientn(colours = pal) +
  theme(legend.position = "none") +
  theme_minimal()

# ideas: add x labels to hline, and remove everything else. Then add plot title in middle of text, using ggtext to tell story of colours
yr <- seq(min(global_temps_long$Year), max(global_temps_long$Year), 10)

yr_df <- global_temps_long %>%
  filter(Year %in% yr)

subtitle <- str_wrap("Estimate of global surface temperature change. Values are deviations from 1951-1980 means", width = 30)

temp_hist <- global_temps_long %>%
  ggplot(aes(x = Year, y = avg_temp_diff)) +
  geom_chicklet(aes(fill = avg_temp_diff), width = 1, colour = "grey10",
                radius = grid::unit(2, "pt"), show.legend = FALSE) +
  geom_text(data = yr_df, mapping = aes(Year, 0, label = Year),
              family = "Avenir", colour = "grey98") +
  annotate("text", x = 1890, y = 1.25, label = "Global Surface\nTemperatures", 
           family = "Avenir", size = 12, colour = "grey98", hjust = 0, fontface = "bold", 
           lineheight = 0.75, vjust = 1) +
  annotate("text", x = 1890, y = 1.01, label = subtitle, 
           family = "Avenir", size = 8, colour = "grey98", hjust = 0, 
           lineheight = 0.75, vjust = 1) +
  scale_fill_gradientn(colours = pal) +
  scale_y_continuous(breaks = seq(-0.75, 1.25, 0.25), position = "left") +
  labs(x = "", y = "",
       caption = "Source: GISTEMP Team, 2023: GISS Surface Temperature Analysis (GISTEMP), version 4 · Graphic: Andrew Moles") +
  theme(text = element_text(family = "Avenir", colour = "grey90"),
        plot.background = element_rect(fill = "grey10"),
        panel.background = element_rect(fill = "grey10"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(colour = "grey98", family = "Avenir"),
        legend.position = "none")
temp_hist

ggsave(here("global-temp-2023-07-11", "temp_hist.png"), temp_hist,
       dpi = 400, width = 12, height = 9, device = ragg::agg_png)

# yearly rise coord polar plot ----
# prep data
plot_data <- global_temps |>
  select(c(Year, Jan:Dec)) |>
  pivot_longer(-Year, names_to = "Month", values_to = "Temp") |>
  drop_na() |> 
  mutate(Month = factor(Month,
                        levels = month.abb)) |> 
  mutate(t = row_number())

# smooth join between Dec and Jan
padding <- plot_data[plot_data$Month == 'Jan',]
padding$Year <- padding$Year - 1 
padding$Month <- NA

final_plot_data <- rbind(plot_data, padding)

(ggplot(final_plot_data, aes(x = Month, y = Temp, group = Year, colour = Temp)) +
  geom_line() +
  coord_polar() +
  scale_x_discrete(expand = c(0,0), breaks = month.abb) +
  scale_y_continuous(limits = c(-1.5, 1.5)) +
  scale_colour_gradientn(colours = pal) +
  guides(colour = guide_colorbar(
    title = "Temperature deviation (°C)",
    title.position = "top",
    title.hjust = 0.5,
    barwidth = 15,
    barheight = 0.7
  )) +
  labs(x = "", y = "") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.direction = "horizontal") -> polar_temp)

ggsave(here("global-temp-2023-07-11", "polar_temp.png"), polar_temp, bg = "grey99",
       dpi = 300, width = 10, height = 10, device = ragg::agg_png)
