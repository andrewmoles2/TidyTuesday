# idea from - https://github.com/leeolney3/TidyTuesday/blob/main/2022/week_05/2022_05.R
library(tidyverse)
library(janitor)
library(ggimage)
library(ggbump)
library(geomtextpath)
library(ggtext)
library(MetBrewer)
library(showtext)
library(here)

font_add_google("Amatic SC")
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)
f1 <- "Amatic SC"

# Load data
breed_traits <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_traits.csv') %>% clean_names()
trait_description <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/trait_description.csv') %>% clean_names()
breed_rank_all <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_rank.csv') %>% clean_names()

# calculate friendliest dog traits average
breed_traits <- breed_traits %>%
  mutate(
    avg_friendliness = select(.,
      affectionate_with_family:good_with_other_dogs,
      openness_to_strangers:playfulness_level,
      adaptability_level:trainability_level
    ) %>% rowMeans(na.rm = TRUE)
  ) 

# tidy breed names
breed_traits_clean <- breed_traits %>% select(breed, avg_friendliness) %>%
  mutate(id = seq(1, nrow(breed_traits))) %>%
  select(-breed)

breed_rank_all <- breed_rank_all %>%
  mutate(breed_clean = trimws(str_replace_all(breed, "\\*|\\(|\\)", "")),
         id = seq(1, nrow(breed_traits))) %>%
  select(-breed)

breed_rank_avg <- full_join(breed_rank_all,breed_traits_clean , by = c("id")) %>%
  select(breed_clean, everything())

# create ranked data
rank_data <- breed_rank_avg %>%
  pivot_longer(x2013_rank:x2020_rank,
               names_to = "year", values_to = "rank") %>%
  mutate(year = parse_number(year)) %>%
  select(-id)

# friendliest dogs
breed_rank_avg %>%
  group_by(breed_clean) %>%
  summarise(avg = mean(avg_friendliness)) %>%
  arrange(desc(avg)) %>%
  filter(avg >= 4.5) %>%
  pull(breed_clean) -> nice_dogs

rank_data_filt <- rank_data %>%
  filter(breed_clean %in% nice_dogs) %>%
  drop_na()

rank_data_filt %>% 
  dplyr::count(breed_clean) %>%
  filter(n > 1) %>%
  pull(breed_clean) -> nice_dogs2

rank_data_filt <- rank_data %>%
  filter(breed_clean %in% nice_dogs2) %>%
  filter(rank <= 120) %>%
  mutate(rank_cat = case_when(
    breed_clean == "Retrievers Labrador" ~ "Top Dog",
    breed_clean == "French Bulldogs" ~ "Top Dog",
    breed_clean == "Retrievers Golden" ~ "Top Dog",
    breed_clean == "Poodles" ~ "Top Dog",
    breed_clean == "Siberian Huskies" ~ "Top Dog",
    breed_clean == "Havanese" ~ "Good Boy",
    breed_clean == "Shetland Sheepdogs" ~ "Good Boy",
    breed_clean == "Pugs" ~ "Good Boy",
    breed_clean == "Vizslas" ~ "Good Boy",
    breed_clean == "Bichons Frises" ~ "Good Boy",
    breed_clean == "Portuguese Water Dogs" ~ "Good Boy",
    breed_clean == "Papillons" ~ "Good Boy",
    breed_clean == "Setters Irish" ~ "Underrated Pups",
    breed_clean == "Keeshonden" ~ "Underrated Pups",
    breed_clean == "Retrievers Flat-Coated" ~ "Underrated Pups")
    )

# plot!
p1 <- rank_data_filt %>%
  ggplot(aes(year, rank)) + 
  geom_textpath(aes(group = breed_clean, colour = factor(rank_cat), label = breed_clean),
                family = f1, size = 3, fontface = 2, hjust = 0.02, vjust = 0.2,
                linewidth = 2.25, straight = FALSE, lineend = "round") +
  scale_y_reverse(breaks = seq(1, max(rank_data_filt$rank), 3)) +
  scale_x_continuous(breaks = seq(2013, 2020, 1)) +
  scale_colour_manual("Loved or undervalued?", values = met.brewer("Egypt")) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 14, base_family = f1) +
  theme(
    legend.position = "top",
    legend.margin = margin(t=6,b=2),
    legend.justification = "left",
    plot.margin = unit(c(0.5, 1, 0.5, 0.5), "cm"),
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(size=12, margin=margin(t=3)),
    axis.text.y = element_text(size = 12, margin = margin(t=3)),
    plot.caption.position = "plot",
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank(), axis.line = element_line(colour = "darkgrey")
  ) +
  labs(
    title = "Who are the friendliest dogs breeds, and how popular are they?",
    subtitle = "Pupularity of dog breeds by AKC registration statistics from 2013-2020.
    Friendliness is average of nice traits, such as Affectionate With Family",
    caption = "TidyTuesday week 5 | Data from American Kennel Club",
    y = "Pupularity of dog breed (rank)"
  ) + 
  geom_image(data = rank_data_filt %>% filter(year == min(year)), aes(image = image, x = year-.3), asp = 2, size = 0.012) +
  geom_image(data = rank_data_filt %>% filter(year == max(year)), aes(image = image, x = year+.3), asp = 2, size = 0.012)
  
p1

# save
ggsave(here("dog-breeds-2022-02-01","good_boys.png"), p1,
       bg = "white", dpi = 320, width = 7.5, height = 7.5)

