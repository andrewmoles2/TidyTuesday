# dr who
library(tidyverse)
library(RColorBrewer)
library(ggthemes)

directors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-11-23/directors.csv')
episodes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-11-23/episodes.csv')
writers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-11-23/writers.csv')
imdb <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-11-23/imdb.csv')

pal <- brewer.pal(11, "RdBu")
scales::show_col(pal)
ggthemes::theme_hc()

episodes %>%
  filter(type == "special") %>%
  ggplot(aes(x = first_aired, rating, colour = uk_viewers)) +
  geom_point(size = 6) +
  geom_line(size = 3) +
  geom_smooth(method = "lm", se = FALSE, 
              colour = "black", size = 2,
              linetype = 2) +
  labs(title = " Dr Who specials imdb rating over time") +
  scale_colour_continuous(low = pal[2], high = pal[10]) +
  ggthemes::theme_hc(base_family = "Avenir")
  #ggthemes::theme_solarized_2(base_family = "Avenir")

episodes %>%
  filter(type == "special") %>%
  ggplot(aes(x = first_aired, rating, colour = factor(season_number))) +
  geom_point(size = 6) +
  geom_smooth(method = "lm", se = FALSE, 
              colour = "black", size = 2,
              linetype = 2) +
  labs(title = " Dr Who specials imdb rating over time") +
  scale_colour_manual(values = brewer.pal(12, "Set3")) +
  ggthemes::theme_hc(base_family = "Avenir")
#ggthemes::theme_solarized_2(base_family = "Avenir")
