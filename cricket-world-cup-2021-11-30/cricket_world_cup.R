# tidytuesday 2021-11-30 cricket world cup
library(tidyverse)
library(lubridate)
library(ggimage)
library(ggthemes)

matches <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-11-30/matches.csv')

glimpse(matches)

# add year column
matches %>%
  mutate(
    year = str_sub(match_date, start = -4),
  ) -> matches

# winners and runner up 1996-2003 world cups
(cwc_finalist <- data.frame(
  year = c("1996", "1999", "2003"),
  winners = c("Sri Lanka", "Australia", "Australia"),
  runner_up = c("Australia", "Pakistan", "India")
))

# extract world cup matches 1996-2003
world_cup <- matches %>%
  filter(str_detect(series, "World Cup"))

# three ideas: Find most "player of matches" players, England cricket world cup record, 
# and Sachin Tendulkar record over time 1996-2005

world_cup %>%
  select(player_of_match, player_of_match_team) %>%
  group_by(player_of_match, player_of_match_team) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(desc(n)) %>%
  slice_max(n, n = 10)
  
world_cup %>%
  select(player_of_match, player_of_match_team, year) %>%
  group_by(player_of_match, player_of_match_team, year) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(desc(player_of_match)) %>%
  filter(n > 1)

world_cup %>%
  group_by(year, player_of_match_team) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  #add flag images - images taken from https://www.flaticon.com/packs/countrys-flags
  mutate(flag = case_when(
    player_of_match_team == "Sri Lanka" ~ 'https://cdn-icons-png.flaticon.com/512/197/197398.png',
    player_of_match_team == "South Africa" ~ 'https://cdn-icons-png.flaticon.com/512/197/197562.png',
    player_of_match_team == "Pakistan" ~ 'https://cdn-icons-png.flaticon.com/512/197/197606.png',
    player_of_match_team == "India" ~ 'https://cdn-icons-png.flaticon.com/512/197/197419.png',
    player_of_match_team == "Australia" ~ 'https://cdn-icons-png.flaticon.com/512/197/197507.png',
    player_of_match_team == "Netherlands" ~'https://cdn-icons-png.flaticon.com/512/197/197441.png',
    player_of_match_team == "United Arab Emirates" ~'https://cdn-icons-png.flaticon.com/512/197/197569.png',
    player_of_match_team == "Kenya" ~'https://cdn-icons-png.flaticon.com/512/197/197608.png',
    player_of_match_team == "Zimbabwe" ~'https://cdn-icons-png.flaticon.com/512/197/197394.png',
    player_of_match_team == "England" ~'https://cdn-icons-png.flaticon.com/512/197/197485.png',
    player_of_match_team == "New Zealand" ~'https://cdn-icons-png.flaticon.com/512/197/197589.png',
    player_of_match_team == "West Indies" ~'https://s.ndtvimg.com/images/entities/300/west-indies-2119.png',
    player_of_match_team == "Canada" ~ "https://cdn-icons-png.flaticon.com/512/197/197430.png",
    player_of_match_team == "Bangladesh" ~ "https://cdn-icons-png.flaticon.com/512/197/197509.png",
    player_of_match_team == "Namibia" ~ "https://cdn-icons-png.flaticon.com/512/197/197617.png"
  )) -> n_player_of_match

n_player_of_match %>%
  ggplot(aes(x=year, y=n, colour = player_of_match_team)) +
  geom_point(position = position_jitterdodge())

(n_player_of_match %>%
  ggplot(aes(x = year, y = n, group = player_of_match_team)) +
  geom_image(aes(image = flag), position = position_jitter(seed = 123, width = 0.3, height = 0.5)) +
  theme_solarized(light = FALSE, base_family = "Avenir") +
  annotate(geom = "text", x = "1996", y = 7, label = "Ski Lanka - winner\nAustralia - runner-up", 
           colour = "white",family = "Avenir") +
  annotate(geom = "text", x = "1999", y = 8, label = "Australia - winner\nPakistan - runner-up",
           colour = "white", family = "Avenir") +
  annotate(geom = "text", x = "2003", y = 7, label = "Australia - winner\nIndia - runner-up",
           colour = "white", family = "Avenir") +
  labs(title = "Count of best players per team from \nCricket World Cups 1996 to 2003",
       x = "Year of Cricket World Cup",
       y = "Number of best player in matches") +
  scale_y_continuous(limits = c(0, 9), breaks = seq(0,9,1)) -> n_best_players)

ggsave("cricket-world-cup-2021-11-30/n_best_players.png", n_best_players,
       dpi = 300)



