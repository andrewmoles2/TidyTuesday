# load libraries ----
library(tidyverse)
library(MetBrewer)
library(ggthemes)

# pull in data using tidytuesday package
tuesdata <- tidytuesdayR::tt_load('2021-12-14')
lyrics <- tuesdata$lyrics

# vector of songs released to the charts ----
releases <- c("Wannabe", "Say You'll Be There", "2 Become 1",
              "Who Do You Think You Are", "Spice Up Your Life",
              "Too Much", "Stop", "Viva Forever", "Goodbye",
              "Holler")

# fix issue with ' and filter for only released songs ----
lyrics <- lyrics %>%
  mutate(song_name = str_replace(song_name,  "�", "'")) %>%
  filter(song_name %in% releases)

# Find total lines per song ----
lyrics %>%
  group_by(song_name) %>%
  summarise(n_lines = n()) -> total_lines

# sum of lines per song for each spice girl ----
lyrics %>%
  group_by(song_name) %>%
  summarise(
    All = sum(str_detect(section_artist, "All"), na.rm = TRUE),
    Baby = sum(str_detect(section_artist, "Baby"), na.rm = TRUE),
    Posh = sum(str_detect(section_artist, "Posh"), na.rm = TRUE),
    Sporty = sum(str_detect(section_artist, "Sporty"), na.rm = TRUE),
    Scary = sum(str_detect(section_artist, "Scary"), na.rm = TRUE),
    Ginger = sum(str_detect(section_artist, "Ginger"), na.rm = TRUE)
  ) -> spice_girl_lines

# join up the total lines and spice girl line counts ----
spice_girl_lines <- left_join(spice_girl_lines, total_lines)

# work out proportions and pivot to long for plotting ----
spice_df <- spice_girl_lines %>%
  mutate(
    All = round(All/n_lines, 2)*100,
    Baby = round(Baby/n_lines, 2)*100,
    Posh = round(Posh/n_lines, 2)*100,
    Sporty = round(Sporty/n_lines, 2)*100,
    Scary = round(Scary/n_lines, 2)*100,
    Ginger = round(Ginger/n_lines, 2)*100
  ) %>%
  pivot_longer(cols = All:Ginger, names_to = "spice_girl", values_to = "prop_lines")

head(spice_df)

# plot with polar coords ---- 
spice_df %>%
  ggplot(aes(x = spice_girl, y = prop_lines)) +
  geom_point(aes(colour = spice_girl, size = prop_lines),
             alpha = 0.6) + 
  geom_text(label = paste0(spice_df$prop_lines, "%"),
            check_overlap = TRUE, family = "Avenir",
            size = 2.75) +
  coord_polar(start = 0, clip = "off") +
  facet_wrap(vars(song_name),
             ncol = 5) +
  guides(colour = "none", size = "none") +
  scale_colour_manual(values = met.brewer("Juarez")) + 
  theme_minimal(base_family = "Avenir") +
  labs(title = "Proportion of lines sung by each Spice Girl for each released song",
       subtitle = "Some lines were sung by multiple Spice Girls at once",
       x = "Spice Girl",
       y = "Proportion of lines sung",
       caption = "Spice Girls TT | A.P.Moles") +
  theme(axis.text.y = element_blank()) -> spice

ggsave("spice-girls-2021-12-14/spice_girls.png", spice,
       width = 12, height = 7, dpi = 320, bg = "white")

# colour pals tested
RColorBrewer::brewer.pal(8, "Pastel2")
met.brewer("Juarez")
MetBrewer::MetPalettes$Cross
MetBrewer::colorblind_palettes
