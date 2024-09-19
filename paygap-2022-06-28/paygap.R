# load libs ----
library(tidyverse)
library(lubridate)
library(geomtextpath)
library(showtext)
library(PostcodesioR)
library(sf)
library(rnaturalearth) 
library(rnaturalearthdata)
library(ggdist)
library(patchwork)
library(ggtext)
library(here)

# fetch a few cool fonts ----
font_add_google(c("Fira Code", "Source Code Pro",
                "Baloo 2", "MuseoModerno"))
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)
f1 <- "Fira Code"
f2 <- "Source Code Pro"
f3 <- "Baloo 2"
f4 <- "MuseoModerno"

# load in data ----
paygap <- readr::read_csv("paygap-2022-06-28/data/paygap.csv")
#paygap <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-28/paygap.csv')
SIC_codes <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-28/SIC07_CH_condensed_list_en.csv')

glimpse(paygap)

# viz of places I have worked----
my_unis <- c("London School Of Economics & Political Science", 
             "LONDON SCHOOL OF ECONOMICS & POLITICAL SCIENCE", 
             "LONDON SCHOOL OF ECONOMICS AND POLITICAL SCIENCE", "Birkbeck", 
             "UCL", "King's College London", "University of Surrey",
             "The University of Reading", "UNIVERSITY OF READING",
             "John Lewis PLC", "JOHN LEWIS PLC")

paygap %>%
  filter(employer_name %in% my_unis) %>%
  mutate(employer_name = str_to_title(employer_name)) %>%
  mutate(employer_name = case_when(
    employer_name == "The University Of Reading" ~ "University Of Reading",
    employer_name == "Ucl" ~ "UCL",
    employer_name == "London School Of Economics And Political Science" ~ "London School Of Economics & Political Science",
    TRUE ~ employer_name)) %>%
  mutate(year = year(due_date),
         month = month(due_date, label = TRUE)) -> workplaces

workplaces %>%
  group_by(employer_name) %>%
  summarise(median = median(diff_median_hourly_percent),
            max = max(diff_median_hourly_percent),
            min = min(diff_median_hourly_percent)) %>%
  arrange(median)

workplaces %>%
  mutate(
    size = ifelse(employer_name == "London School Of Economics & Political Science", 5, 4),
    colour = ifelse(employer_name == "London School Of Economics & Political Science", "#E40720", "#3A9D91")
  ) %>%
  ggplot() +
  aes(x = due_date, y = diff_median_hourly_percent, label = employer_name,
      colour = colour, group = employer_name) +
  geom_textpath(size = 5, lineend = "round", straight = TRUE, hjust = "auto",
                text_smoothing = 30, family = f2, linemitre = 15, rich = TRUE) +
  scale_colour_identity() +
  scale_y_continuous(limits = c(0, 30)) +
  labs(x = "", y = "", 
       title = "Difference in median men and womens pay for my previous employers<br>
       <span style = 'font-size:16pt;'>Above 0 indicates men are paid more, 0 indicates equality. My most recent employer,<br>
       <span style = 'color:#E40720;'>London School of Economics and Political Science (LSE)</span> are gradually improving</span>",
       caption = "Viz Andrew Moles | data from gender-pay-gap.service.gov.uk") +
  theme_minimal(base_family = f2, base_size = 14) +
  theme(plot.title.position = "plot",
        plot.title = element_textbox_simple(
          size = 20, lineheight = 1, padding = margin(0, 0, 5, 0))) +
  annotate(geom = "text", x = as.POSIXct("2020-10-01"), y = 17, 
           label = "General trend is downward which is encouraging!
           Yay for more equality", hjust = "left")

ggsave(filename = "paygap-2022-06-28/workplaces.png", dpi = 320,
       units = "px", width = 3500, height = 2500, bg = "white")

# Postcode mapping of uk----
#https://twitter.com/nrennie35/status/1541861448237842433

# Read in grid shapefile
gb_grid <- sf::read_sf(file.path(getwd(), "museums-2022-11-22/data/gb_grid/gb_grid.shp"))
gb_post <- read_sf("museums-2022-11-22/data/postcode_polygons.gpkg")

paygap_sic <- left_join(paygap, SIC_codes, by = c("sic_codes" = "SIC Code")) %>%
  mutate(year = year(date_submitted))

# Option to do TA on description as per Julia Silge
library(tidytext)

paygap_token <- paygap_sic %>%
  unnest_tokens(word, Description) %>%
  anti_join(get_stopwords()) %>%
  na.omit()

paygap_prep <- paygap_token %>%
  select(employer_name, post_code, diff_mean_hourly_percent, 
         diff_median_hourly_percent, word, year) %>%
  mutate(pc_area = str_extract(post_code, "[A-Z][A-Z]*")) %>%
  filter(!word %in% c("activities", "n.e.c", "general", "non"))

paygap_agg <- paygap_prep %>%
  group_by(pc_area) %>%
  summarise(avg_diff_mean_hour = mean(diff_mean_hourly_percent, na.rm = TRUE),
            avg_diff_median_hour = median(diff_median_hourly_percent, na.rm = TRUE))

paygap_agg_year <- paygap_prep %>%
  group_by(pc_area, year) %>%
  summarise(avg_diff_mean_hour = mean(diff_mean_hourly_percent, na.rm = TRUE),
            avg_diff_median_hour = median(diff_median_hourly_percent, na.rm = TRUE))

paygap_sic_agg <- paygap_prep %>%
  group_by(pc_area, word) %>%
  summarise(n = n(),
            avg_diff_median_hour = median(diff_median_hourly_percent, na.rm = TRUE),
            avg_diff_mean_hour = mean(diff_mean_hourly_percent, na.rm = TRUE)) %>%
  group_by(pc_area) %>%
  slice_max(order_by = n, n = 1) %>%
  ungroup() %>%
  add_count(pc_area, name = "ties") %>%
  mutate(word = if_else(ties > 1, "Multiple", word))

# join data to geo data - average over all years
paygap_type <- gb_grid %>%
  left_join(paygap_sic_agg)
paygap_type2 <- gb_post %>%
  left_join(paygap_sic_agg)
paygap_type3 <- gb_post %>%
  left_join(paygap_agg)
# with year data
paygap_type4 <- gb_post %>%
  left_join(paygap_agg_year)

# plot - two types, grid and post code areas
# import coolors function
devtools::source_gist("ffa7f0bae82f9df88c6c1d9458d980a8")

lvls <- sort(unique(paygap_sic_agg$word))

ggplot(paygap_type) +
  geom_sf(aes(fill = word), color = "white") +
  #scale_fill_manual(values = pal, breaks = lvls, drop = FALSE) +
  guides(fill = guide_legend(title = "Industry", override.aes = list(color = "grey95"))) +
  coord_sf(clip = "off") +
  theme_void(base_family = f1)

ggplot(paygap_type2) +
  geom_sf(aes(fill = word), color = "white") +
  #scale_fill_manual(values = pal, breaks = lvls, drop = FALSE) +
  guides(fill = guide_legend(title = "Industry", override.aes = list(color = "grey95"))) +
  coord_sf(clip = "off") +
  theme_void(base_family = f1)

grad_pal <- coolors("https://coolors.co/gradient-maker/ffe985-75fae2-6492f5")
scales::show_col(grad_pal)

(median_diff_post <- ggplot(paygap_type3) +
  geom_sf(aes(fill = avg_diff_median_hour), colour = "white") +
  scale_fill_gradient(low = grad_pal[1], high = grad_pal[3]) +
  guides(fill = guide_legend(title = "Average median difference\nin pay per hour", override.aes = list(color = "grey95"))) +
  coord_sf(clip = "off") +
  theme_void(base_family = f1) +
  #theme(
  #  plot.background = element_rect(fill = "grey95", color = NA),
  #  legend.position = c(2.0, 0.6),
  #  plot.margin = margin(0, 180, 0, 0)
  #) +
  plot_annotation(
    title = stringr::str_wrap('Difference in median men and womens pay by postal code area', 37),
    subtitle = "Average difference from 2017-2024\nAbove 0 indicates men are paid more, 0 indicates equality",
    caption = "Source: gender-pay-gap.service.gov.uk · Graphic: Andrew Moles"
  ) &
  theme(
    plot.background = element_rect(fill = "grey97", color = NA),
    legend.position = c(1.2, 0.6),
    plot.margin = margin(0, 70, 5, 1),
    plot.title = element_text(family = f1, face = "bold", size = 20, margin = margin(10, 0, 5, 0)),
    plot.subtitle = element_text(family = f1, size = 12),
    plot.caption = element_text(family = f1, size = 10, hjust = 0)
  ))

ggsave("paygap-2022-06-28/median_pay_diff_postcode.png", median_diff_post, 
       dpi = 320, units = "px", width = 2500, height = 3000, device = ragg::agg_png
       )

# plot change over years
paygap_type4 %>%
  filter(!is.na(year)) %>%
  ggplot() +
  geom_sf(aes(fill = avg_diff_median_hour), colour = "white") +
  scale_fill_gradient2(low = grad_pal[1], mid = "#CBBFBD", high = grad_pal[3], midpoint = 10) +
  guides(fill = guide_legend(title = "Average median\ndifference in\npay per hour", override.aes = list(color = "grey95"))) +
  facet_wrap(vars(year), ncol = 3) +
  coord_sf(clip = "off") +
  theme_void(base_family = f1) +
  plot_annotation(
    title = stringr::str_wrap('Difference in median men and womens pay by postal code area between 2017-2024', 37),
    subtitle = "Above 0 indicates men are paid more, 0 indicates equality",
    caption = "Source: gender-pay-gap.service.gov.uk · Graphic: Andrew Moles"
  ) &
  theme(
    plot.background = element_rect(fill = "grey97", color = NA),
    legend.position = c(0.9, 0.18), legend.direction = "vertical",
    plot.margin = margin(0, 70, 5, 1),
    plot.title = element_text(family = f1, face = "bold", size = 20, margin = margin(10, 0, 5, 0)),
    plot.subtitle = element_text(family = f1, size = 12),
    plot.caption = element_text(family = f1, size = 10, hjust = 0)
  ) -> median_diff_post_year
median_diff_post_year

ggsave("paygap-2022-06-28/median_pay_diff_postcode_year.png", median_diff_post_year, 
       dpi = 320, units = "px", width = 2500, height = 3000, device = ragg::agg_png
)


# a plot for each year
year_range <- range(paygap_type4$year, na.rm = TRUE)
years <- seq(year_range[1], year_range[2])

for (i in years) {
  p <- paygap_type4 %>%
    filter(!is.na(year)) %>%
    filter(year == i) %>%
    ggplot() +
    geom_sf(aes(fill = avg_diff_median_hour), colour = "white") +
    scale_fill_gradient2(low = grad_pal[1], mid = "#CBBFBD", high = grad_pal[3], midpoint = 10) +
    guides(fill = guide_legend(title = "Average median\ndifference in\npay per hour", override.aes = list(color = "grey95"))) +
    coord_sf(clip = "off") +
    theme_void(base_family = f1) +
    plot_annotation(
      title = stringr::str_wrap('Yearly difference in median men and womens pay by postal code area', 37),
      subtitle = paste0("Above 0 indicates men are paid more, 0 indicates equality\n", "Year: ",i),
      caption = "Source: gender-pay-gap.service.gov.uk · Graphic: Andrew Moles"
    ) &
    theme(
      plot.background = element_rect(fill = "grey97", color = NA),
      legend.position = c(1.2, 0.6), legend.direction = "vertical",
      plot.margin = margin(0, 70, 5, 1),
      plot.title = element_text(family = f1, face = "bold", size = 20, margin = margin(10, 0, 5, 0)),
      plot.subtitle = element_text(family = f1, size = 12),
     plot.caption = element_text(family = f1, size = 10, hjust = 0)
    )

  print(p)

  ggsave(here("paygap-2022-06-28", paste0("paygap_plot_", i, ".png")), dpi = 320, 
       units = "px", width = 2500, height = 3000, device = ragg::agg_png
  )

}

library(magick)

list.files(
  path = here("paygap-2022-06-28"),
  full.names = TRUE,
  pattern = "paygap_plot"
) %>%
  lapply(., image_read) %>%
  image_join() %>%
  image_animate(fps = 1) %>%
  image_write(path = here("paygap-2022-06-28", "paygap.gif"))

# interactive image of gender pay gap for a single year - pull latest year ----
library(ggiraph)

# https://www.doogal.co.uk/PostcodeDistricts
postcode_info <- read_csv("paygap-2022-06-28/data/Postcode_districts.csv")

postcode_info <- postcode_info %>%
  mutate(pc_area = gsub("[[:digit:]]+", "", Postcode)) %>%
  select(pc_area, Postcodes, Population, Region, `UK region`) %>%
  group_by(pc_area, Region, `UK region`) %>%
  summarise(n_postcodes = sum(Postcodes, na.rm = TRUE), 
            n_pop = sum(Population, na.rm = TRUE))

m_year <- max(paygap_type4$year, na.rm = TRUE)

paygap_type4 <- paygap_type4 %>%
  left_join(postcode_info, by = join_by(pc_area))

paygap_type4 %>%
  filter(!is.na(year)) %>%
  filter(year == m_year) %>%
  mutate(plot_text = paste0("PostCode area: ", pc_area, " has average median difference in pay of ", round(avg_diff_mean_hour, 2))) %>%
  ggplot() +
  ggiraph::geom_sf_interactive(aes(fill = avg_diff_median_hour,
                                   data_id = pc_area,
                                   tooltip = plot_text), 
                               colour = "white",
                               hover_nearest = TRUE) +
  #geom_sf(aes(fill = avg_diff_median_hour), colour = "white") +
  scale_fill_gradient2(low = grad_pal[1], mid = "#CBBFBD", high = grad_pal[3], midpoint = 10) +
  guides(fill = guide_legend(title = "Average median\ndifference in\npay per hour", override.aes = list(color = "grey95"))) +
  coord_sf(clip = "off") +
  theme_void(base_family = f1) +
  plot_annotation(
    title = stringr::str_wrap('Yearly difference in median men and womens pay by postal code area', 37),
    subtitle = paste0("Above 0 indicates men are paid more, 0 indicates equality\n", "Year: ",i),
    caption = "Source: gender-pay-gap.service.gov.uk · Graphic: Andrew Moles"
  ) &
  theme(
    plot.background = element_rect(fill = "grey97", color = NA),
    legend.position = c(1.2, 0.6), legend.direction = "vertical",
    plot.margin = margin(0, 70, 5, 1),
    plot.title = element_text(family = f1, face = "bold", size = 20, margin = margin(10, 0, 5, 0)),
    plot.subtitle = element_text(family = f1, size = 12),
    plot.caption = element_text(family = f1, size = 10, hjust = 0)
  ) -> pay_int

css_default_hover <- girafe_css_bicolor(primary = "#E2DBAA", secondary = "#ABB3E2")
set_girafe_defaults(opts_hover = opts_hover(css = css_default_hover))

girafe(ggobj = pay_int, fonts = list("Arial"),
       options = list(opts_toolbar(position = "bottom")))

