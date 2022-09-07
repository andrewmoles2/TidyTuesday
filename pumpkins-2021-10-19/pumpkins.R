# load libraries ----
library(tidyverse)
library(ggforce)
library(patchwork)
library(gganimate)
library(emojifont)
library(ggwaffle)
library(emoGG)
library(here)
library(Cairo)

# load in data ----
pumpkins <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-19/pumpkins.csv')

pumpkins %>% glimpse()

# split up id variable and tidy up data ----
# Types: F = "Field Pumpkin", P = "Giant Pumpkin", S = "Giant Squash", W = "Giant Watermelon", L = "Long Gourd" (length in inches, not weight in pounds), T = Tomato
to_num <- c("year", "weight_lbs", "ott", "est_weight", "pct_chart")
to_fct <- c("city","state_prov", "country", "gpc_site")

pumpkins_tidy <- pumpkins %>%
  separate(col = id, into = c("year", "type"),
           sep = "-") %>%
  mutate(type = if_else(type == "F", "Field Pumpkin",
                        if_else(type == "P", "Giant Pumpkin",
                                if_else(type == "S", "Giant Squash",
                                        if_else(type == "W", "Giant Watermelon",
                                                if_else(type == "L", "Long Gourd", "Tomato"))))),
         across(to_num, as.numeric),
         across(to_fct, as_factor),
         weight_kg = weight_lbs*0.453592)


# find the pumkpin emoji code ----
emoGG::emoji_search("pumpkin")

# make some plots! ----
cols <- c("#6A6A1C","#937F21","#C1922C","#F4A23E")

scale_factor = 3

(pumpkins_tidy %>%
  filter(type != "Long Gourd" & type != "Tomato") %>%
  ggplot(aes(x = type, y = weight_kg)) +
  geom_point(size = 5, colour = cols[4]) +
  stat_summary(fun = mean, geom = "point",
               shape = 95, size = 25) +
  add_emoji(emoji = "1f383") +
  guides(colour = "none") +
  labs(title = "Weight (kg) of pumpkins entered into the Great Pumpkin Commonwealth",
       subtitle = "Average weight indicated by bar. These pumpkins are huge!",
       x = "Type of Pumpkin",
       caption = "Tidy Tuesday 2021-10-19 A.P.Moles") +
  theme_minimal() +
    theme(text = element_text(size= scale_factor * 4.5, family = "Avenir"),
          axis.text = element_text(size= scale_factor * 3.5, family = "Avenir")) -> pumpkins_weight)

(pumpkins_tidy %>%
  filter(type == "Giant Pumpkin") %>%
  group_by(year) %>%
  summarise(median_kg = median(weight_kg, na.rm = TRUE),
            mean_kg = mean(weight_kg, na.rm = TRUE)) %>%
  ggplot(aes(x = as_factor(year), y = median_kg)) +
  geom_emoji(emoji = "1f383", size = 0.1) +
  labs(title = "Median Giant Pumpkin weight (kg) from 2013 to 2021",
       subtitle = "The average weight of humans is between 60-80kg",
       x = "Year of Great Pumpkin Commonwealth competition",
       y = "Median kilograms",
       caption = "Tidy Tuesday 2021-10-19 A.P.Moles") +
  theme_dark() +
    theme(text = element_text(size= scale_factor * 13, family = "Avenir"),
          axis.text = element_text(size= scale_factor * 12, family = "Avenir")) -> giant_pumpkin_avg)



# save the plots
ggsave(here("pumpkins-2021-10-19", "pumpkins_weight.png"), pumpkins_weight,
       bg = "white", width = 6, height = 5, dpi = 150)
ggsave(here("pumpkins-2021-10-19", "giant_pumpkin_avg.png"), giant_pumpkin_avg, width = 6.5)

# other attempts ----

pumpkins_tidy %>%
  filter(type != "Long Gourd" & type != "Tomato") %>%
  ggplot(aes(x = type, y = weight_kg)) +
  geom_emoji(emoji = "1f383")

pumpkins_tidy %>%
  filter(year >= 2018) %>%
  group_by(type) %>%
  slice_max(weight_kg, n = 20) %>%
  filter(type != "Long Gourd" & type != "Tomato") %>%
  ggplot(aes(x = type, y = weight_kg)) +
  geom_emoji(emoji = "1f383") +
  facet_wrap(vars(year))


# how to add emojis to plots - https://r-charts.com/miscellaneous/emogg/

  