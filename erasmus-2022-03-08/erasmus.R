# libraries ----
library(tidyverse)
library(zoo)
library(ggalluvial)
library(MetBrewer)
library(countrycode)

# load data ----
erasmus <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-08/erasmus.csv')

# cool ideas
# https://twitter.com/KittJonathan/status/1501166071633612809 - circle bar
# https://twitter.com/hec_vini/status/1501542906754072577 - Sankey
# Look at UK students and where they go
# Also look at where people are coming from when coming to uk 

# quick review of data ----
erasmus %>% glimpse()

# data wrangling ----
country_codes <- codelist %>%
  select(iso2c, country_name = country.name.en)

uk_students <- erasmus %>%
  filter(participant_nationality == "UK") %>%
  select(sending_country_code, sending_city, receiving_country_code, receiving_city, participants) %>%
  left_join(country_codes, by = c("receiving_country_code" = "iso2c")) %>%
  rename(receiving_country_name = country_name) %>%
  left_join(country_codes, by = c("sending_country_code" = "iso2c")) %>%
  rename(sending_country_name = country_name) %>%
  mutate(receiving_country_name = case_when(
    receiving_country_code == "EL" ~ "Greece",  # add missing country names 
    receiving_country_code == "UK" ~ "United Kingdom",
    receiving_country_code == "CZ" ~ "Czech Republic",
    TRUE ~ receiving_country_name),
    sending_country_name = case_when(
      sending_country_code == "EL" ~ "Greece",  # add missing country names 
      sending_country_code == "UK" ~ "United Kingdom",
      sending_country_code == "CZ" ~ "Czech Republic",
      TRUE ~ sending_country_name))

all_uk <- uk_students %>%
  group_by(receiving_country_name) %>%
  summarise(total = sum(participants)) %>%
  arrange(desc(total)) %>%
  ungroup() %>%
  mutate(percent = 100 * total/ sum(total)) %>%
  mutate(receiving_country_name = factor(receiving_country_name))

no_uk_uk <- uk_students %>%
  filter(receiving_country_name != "United Kingdom") %>%
  group_by(receiving_country_name) %>%
  summarise(total = sum(participants)) %>%
  arrange(desc(total)) %>%
  ungroup() %>%
  mutate(percent = 100 * total/ sum(total)) %>%
  mutate(receiving_country_name = fct_reorder(receiving_country_name, percent))

sub_title <- paste0(round(all_uk[1,"percent"],2),"% of ",nrow(uk_students)," UK Erasmus students stay within the UK")

# plotting 
background <- "#E6D0BF" #"#2C3827"
pal <- met.brewer("Manet")[c(-4,-5,-6)]
titles <- "#663589"

erasmus_1 <- no_uk_uk %>%
  ggplot(aes(x = receiving_country_name, y = total, fill = receiving_country_name)) +
  geom_bar(stat = "identity", width = 0.8, show.legend = FALSE) +
  coord_polar(theta = "y", start = 0) +
  scale_fill_manual(values = rep(pal, 11)) +
  ylim(c(0, 125)) +
  labs(title = "Where do UK erasmus students go other than the UK?",
       subtitle = sub_title,
       caption = "Andrew Moles | TidyTuesday 2022 week 10",
       x = "",
       y = "") +
  geom_text(aes(x = receiving_country_name, y = 0, label = paste0(receiving_country_name, " - ", round(percent, 1), " %")),
            hjust = 1.05, family = "Avenir", size = 3, colour = rep(rev(pal), 11)[1:31]) +
  theme_void() +
  theme(plot.background = element_rect(fill = background, colour = background),
        panel.background = element_rect(fill = background, colour = background),
        plot.title = element_text(family = "Avenir", size = 18, colour = titles,
                                  hjust = 0.5, margin = margin(t = 20)),
        plot.subtitle = element_text(family = "Avenir", size = 14, colour = titles,
                                     hjust = 0.5),
        plot.caption = element_text(family = "Avenir", size = 8, colour = titles,
                                    hjust = 0.5, margin = margin(b = 20)))
erasmus_1  

ggsave("erasmus-2022-03-08/erasmus_polar.png", erasmus_1, dpi = 320, 
       width = 12, height = 12)
  