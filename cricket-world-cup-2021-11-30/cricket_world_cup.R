# tidytuesday 2021-11-30 cricket world cup
library(tidyverse)
library(lubridate)
library(ggimage)
library(ggthemes)
library(ggforce)

matches <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-11-30/matches.csv')

glimpse(matches)

# make ggplot theme ----
theme_andrew <- function(){
  font <- "Avenir" 
  
  theme_bw() %+replace%  # replace elements of theme to change
    
    theme(
      
      # modify border/grid
      panel.border = element_rect(
        fill = NA,
        linetype = 2,
        colour = "#D1BFB4"),
      panel.grid = element_line(
        linetype = 2,
        size = 0.25,
        colour = "#D1BFB4"),
      
      # facet grid colour
      strip.background = element_rect(
        colour = "black", fill = "#CFE9C4"),
      strip.text.x = element_text(
        size = 10, color = "black", face = "bold.italic"
      ),
      strip.text.y = element_text(
        size = 10, color = "black", face = "bold.italic"
      ),
      
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
        colour = "#133977"),
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

# best players ----

world_cup %>%
  select(player_of_match, player_of_match_team) %>%
  group_by(player_of_match, player_of_match_team) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(desc(n)) %>%
  filter(n >= 3) %>%
  pull(player_of_match) -> top_players
  
world_cup %>%
  select(player_of_match, player_of_match_team, year) %>%
  group_by(player_of_match, player_of_match_team, year) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(player_of_match %in% top_players) -> top_players_year

pal <- c(
  "Aravinda de Silva" = "#436398",
  "Glenn McGrath" = "#DEC140",
  "Lance Klusener" = "#16320C",
  "Mark Waugh" = "#DEC140",
  "Marvan Atapattu" = "#436398",
  "Maurice Odumbe" = "#4C1C1C",
  "Sachin Tendulkar" = "#4085DE",
  "Sourav Ganguly" = "#4085DE",
  "Neil Johnson" = "#A32323",
  "Roger Twose" = "black",
  "Sanath Jayasuriya" = "#436398",
  "Shane Warne" = "#DEC140"
)

player_tournament <- data.frame(
    year = c("1996", "1999", "2003"),
    player = c("Sanath Jayasuriya", "Lance Klusener", "Sachin Tendulkar"),
    team = c("Sri Lanka", "South Africa", "India")
    )

facet_labs <- c(paste(player_tournament$year[1], "player of tournament:", player_tournament$player[1]),
                paste(player_tournament$year[2], "player of tournament:", player_tournament$player[2]),
                paste(player_tournament$year[3], "player of tournament:", player_tournament$player[3]))
names(facet_labs) <- player_tournament$year

top_players_year %>%
  ggplot(aes(x = player_of_match_team, y = n, label = player_of_match)) +
  geom_segment(aes(x = player_of_match_team, y = 0,
                   xend = player_of_match_team, yend = n),
               linetype = 2, colour = "#D1BFB4") +
  geom_text(aes(colour = player_of_match), check_overlap = TRUE, vjust = 0.4,
            family = "Avenir", size = 4.5, fontface = "bold") +
  facet_wrap(vars(year), ncol = 1,
             labeller = labeller(year = facet_labs)) +
  labs(title = "Top players from Cricket World Cups 1996 to 2003",
       subtitle = "Sachin Tendulkar is only player to win more than one best player award\n between 1996 and 2003 world cups",
       x = "Team", y = "Number of player of match awards",
       caption = "2021-11-30 A.P.Moles TT") +
  guides(colour = "none") +
  scale_colour_manual(values = pal) +
  scale_y_continuous(limits = c(0, 5))
  
top_players_year %>%
  ggplot(aes(x = player_of_match, y = n, label = player_of_match_team)) +
  geom_segment(aes(x = player_of_match, y = 0,
                   xend = player_of_match, yend = n),
               linetype = 2, colour = "black") +
  geom_label(aes(colour = player_of_match),  hjust = 0.5,
             family = "Avenir", size = 4.5, fontface = "bold") +
  facet_wrap(vars(year), ncol = 1,
             labeller = labeller(year = facet_labs)) +
  coord_flip() + guides(colour = "none") +
  scale_colour_manual(values = pal) +
  scale_y_continuous(limits = c(0, 4.5)) +
  labs(title = "Top players from Cricket World Cups 1996 to 2003",
       subtitle = "Sachin Tendulkar is only player to win more than one best player award\n between 1996 and 2003 world cups",
       x = "Team", y = "Number of player of match awards",
       caption = "2021-11-30 A.P.Moles TT") -> top_players_plot

top_players_plot

ggsave("cricket-world-cup-2021-11-30/top_players.png", top_players_plot,
       width = 8.2, height = 8.75, dpi = 320)

# player of match per team ----
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
  labs(title = "Count of players of the match per team from \nCricket World Cups 1996 to 2003",
       subtitle = "This era of cricket was dominated by Australia", 
       x = "Year of Cricket World Cup",
       y = "Number of best player in matches",
       caption = "2021-11-30 A.P.Moles TT") +
  scale_y_continuous(limits = c(0, 9), breaks = seq(0,9,1)) -> n_best_players)

ggsave("cricket-world-cup-2021-11-30/n_best_players.png", n_best_players,
       dpi = 320)



