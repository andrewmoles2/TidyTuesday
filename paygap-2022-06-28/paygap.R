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

# fetch a few cool fonts
font_add_google(c("Fira Code", "Source Code Pro",
                "Baloo 2", "MuseoModerno"))
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)
f1 <- "Fira Code"
f2 <- "Source Code Pro"
f3 <- "Baloo 2"
f4 <- "MuseoModerno"

# load in data
paygap <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-28/paygap.csv')
SIC_codes <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-28/SIC07_CH_condensed_list_en.csv')

glimpse(paygap)

# Two visualisations
# my work places over the years 
# postcode mapping of uk

# viz of places I have worked
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
  scale_y_continuous(limits = c(0, 25)) +
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

## Postcode mapping of uk
#https://twitter.com/nrennie35/status/1541861448237842433

