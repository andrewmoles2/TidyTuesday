# load libaries
library(tidyverse)
library(RColorBrewer)
library(gganimate)
library(ggforce)
library(patchwork)
library(magick)
library(here)
library(data.table)
library(janitor)
library(ggimage)

# load in data from webscraping repo
pokemon_raw <- fread("https://raw.githubusercontent.com/andrewmoles2/webScraping/main/R/data/pokemon.csv") %>%
  clean_names()

# make ggplot theme
theme_andrew <- function(){
  font <- "Avenir" 
  
  theme_bw() %+replace%  # replace elements of theme to change
    
    theme(
      
      # modify border/grid
      panel.border = element_rect(
        fill = NA,
        linetype = 3),
      panel.grid = element_line(
        linetype = 3,
        size = 0.25),
      
      # change text
      plot.title = element_text(
        family = font,
        size = 16,
        face = 'bold',
        hjust = 0,
        vjust = 2),
      plot.subtitle = element_text(
        family = font,
        size = 10,
        hjust = 0,
        vjust = 2,
        colour = "#F1945E"),
      plot.caption = element_text(
        family = font,
        size = 9,
        hjust = 1),
      axis.title = element_text(
        family = font,
        size = 10),
      axis.text =  element_text(
        family = font,
        size = 9)
    )    
}

theme_set(theme_andrew())

# type colours
type_col <- c(Bug = "#6a8b5a", Dark = "#414152", Dragon = "#5a8bee", 
              Electric = "#f6e652", Fairy = "#ffd5bd", Fighting = "#b40000", 
              Fire = "#ee8329", Flying = "#6ab4e6", Ghost = "#8b6283", Grass = "#20b49c", 
              Ground = "#c57341", Ice = "#e6e6f6", Normal = "#E8E8E8", 
              Poison = "#a483c5", Psychic = "#f65273", Rock = "#e6d5ac", 
              Steel = "#A1A1A1", Water = "#083962")

scales::show_col(type_col)

# Turn certain cols to factors
fact <- c("type1", "type2", "generation")

pokemon_raw <- pokemon_raw %>%
  mutate(across(all_of(fact), as_factor),
         type1 = fct_relevel(type1, sort),
         type2 = fct_relevel(type2, sort)) 

# avg total per gen
pokemon_raw %>%
  group_by(generation) %>%
  summarise(avg_total = median(total, na.rm = TRUE)) %>%
  ggplot() + geom_bar(aes(x = generation, y = avg_total, fill = generation), stat = 'identity') +
  labs(title = "Median stat total by\n generation of Pokémon",
       x = "Pokémon Generation", y = "Median Total Stats",
       fill = "Generation") + 
  geom_hline(yintercept = median(pokemon_raw$total, na.rm = TRUE),
             linetype = 2, colour = "Black") +
  scale_fill_brewer(palette = 'Dark2')

# density plot per gen
pokemon_raw %>%
  ggplot() +
  geom_density(aes(x = total, fill = generation)) + 
  facet_wrap(vars(generation), nrow = 4) +
  scale_fill_brewer(palette = 'Dark2')

# avg stats across all gens
pokemon_avg_gen <- pokemon_raw %>%
  group_by(generation) %>%
  summarise(hp = median(hp),
            attack = median(attack),
            defense = median(defense),
            sp_atk = median(sp_atk),
            sp_def = median(sp_def),
            speed = median(speed)) %>%
  pivot_longer(!generation, names_to = "avg_stat", values_to = "median")

pokemon_avg_gen %>%
  ggplot(aes(x = avg_stat, y = median, fill = generation)) + 
  geom_bar(stat = 'identity', position = 'dodge', colour = "black") +
  scale_fill_brewer(palette = 'Dark2') +
  labs(title = "Median for each statistic\n by generation",
       x = "Pokémon Statistics", y = "Median Stats",
       fill = "Generation") +
  coord_flip() 

# pokemon type plots
# count per type per gen (good candidate for filter, all, gen 1, etc.)
type1 <- pokemon_raw %>%
  ggplot(aes(x = type1, fill = type1)) +
  geom_bar(colour = 'black') + 
  scale_fill_manual(values = type_col) +
  labs(title = "Count of Pokémon first typing",
       fill = "Pokémons Type") +
  coord_flip() +
  facet_wrap(vars(generation))

type2 <-pokemon_raw %>%
  tidyr::drop_na(type2) %>%
  ggplot(aes(x = type2, fill = type2)) +
  geom_bar(colour = 'black') + 
  scale_fill_manual(values = type_col) +
  labs(title = "Count of Pokémon second typing",
       fill = "Pokémons Type") +
  coord_flip() +
  facet_wrap(vars(generation))

patchwork::wrap_plots(type1, type2, guides = 'collect')

# total stats by type 1
pokemon_raw %>%
  ggplot(aes(x = type1, y = total, colour = type1)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position = 'jitter') +
  scale_colour_manual(values = type_col) +
  labs(title = "Total stats by type 1")

# type stats
type_agg <- pokemon_raw %>%
  group_by(generation, type1, type2) %>%
  summarise(hp = median(hp, na.rm = TRUE),
            attack = median(attack, na.rm = TRUE),
            defense = median(defense, na.rm = TRUE),
            sp_atk = median(sp_atk, na.rm = TRUE),
            sp_def = median(sp_def, na.rm = TRUE),
            speed = median(speed, na.rm = TRUE)) %>%
  pivot_longer(!c(generation, type1, type2), names_to = "avg_stat", values_to = "median")

type_agg %>%
  ggplot(aes(x = avg_stat, y = median, fill = type1)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_fill_manual(values = type_col) + 
  facet_wrap(vars(generation))

type_agg %>%
  filter(generation == 1) %>%
  ggplot(aes(x = avg_stat, y = median, fill = type1)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_fill_manual(values = type_col)

type_agg %>%
  ggplot(aes(x = type1, y = median, fill = type1)) +
  geom_point(size = 5, shape = 21) +
  scale_fill_manual(values = type_col) +
  facet_wrap(vars(generation), nrow = 4) + 
  theme(axis.text.x = element_text(angle = -35))

type_agg %>%
  ggplot(aes(x = type1, y = median, fill = type1)) +
  geom_point(size = 5, shape = 21) +
  scale_fill_manual(values = type_col) +
  facet_wrap(vars(avg_stat), nrow = 4) + 
  theme(axis.text.x = element_text(angle = -35))

type_agg %>%
  ggplot(aes(x = type1, y = median, colour = type1, shape = generation)) +
  geom_point(size = 8) +
  scale_colour_manual(values = type_col) +
  facet_wrap(vars(avg_stat), nrow = 4) + 
  theme_dark(base_family = "Avenir") +
  scale_shape_manual(values = c(49:56),
                     guide = "none") +
  theme(axis.text.x = element_text(angle = -35))

type_agg %>%
  filter(generation == 8) %>%
  slice_max(median, n = 5)

# top 5 stats
pokemon_raw %>%
  filter(generation == 7) %>%
  slice_max(hp, n = 5)

pokemon_raw %>%
  filter(generation == 2) %>%
  slice_max(speed, n = 5)

# ash team vs other gen 1 pokemon ----
ash_pokemon <- c('Bulbasaur', 'Charizard', 'Squirtle', 'Butterfree', 'Pidgeot', 'Pikachu')

# find gen 1 total average (mean)
pokemon_raw %>%
  filter(generation == 1) %>%
  summarise(avg_total = mean(total, na.rm = TRUE)) %>%
  pull(avg_total) -> gen1_avg

(pokemon_raw %>%
  filter(name %in% ash_pokemon) %>%
  mutate(name = factor(name, levels = ash_pokemon)) %>%
  ggplot(aes(x = name, y = total)) +
  geom_pokemon(aes(image = str_to_lower(ash_pokemon)), size=.125) +
  geom_hline(yintercept = gen1_avg, linetype = 2) +
  scale_y_continuous(limits = c(250,600), breaks = seq(250, 600, 50)) +
  annotate(geom = "text", x = "Bulbasaur", y = (gen1_avg+10),
           label = "Generation 1 average total", hjust = "left") +
  labs(title = "Ash's Pokémon from the first generation",
       subtitle = "Total statistics in games compared to other Pokémon",
       x = "Pokémon name",
       y = "Total Statistics") -> ash_pokemon_plot)

ggsave("Pokemon_viz/ash_pokemon_plot.png", ash_pokemon_plot)


