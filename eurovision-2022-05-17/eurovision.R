# setup ----
# install ggflags
# devtools::install_github('rensa/ggflags', force = TRUE)
# libraries
library(tidyverse)
library(ggflags)
library(MetBrewer)
library(ggthemes)

path <- "eurovision-2022-05-17/"

# load in and view data ----
eurovision <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-17/eurovision.csv')
eurovision_votes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-17/eurovision-votes.csv')

# reviewing data ---
glimpse(eurovision)
summary(eurovision)

# looking at countries with the best win ratio ----
# make winner into int for aggregation
eurovision$winner_int <- as.integer(eurovision$winner)
# now filter for final and grand-final, agg by country
unique(eurovision$section)
# calculate avg points and rank, and total wins and year
country_agg <- eurovision %>%
  filter(section == "final" | section == "grand-final") %>%
  group_by(artist_country) %>%
  summarise(avg_points = mean(total_points, na.rm = TRUE),
            avg_rank = mean(rank, na.rm = TRUE),
            tot_wins = sum(winner_int, na.rm = TRUE),
            tot_years = n()) %>%
  arrange(desc(tot_wins)) %>%
  # calc win ratio (tot_wins/tot_years)
  mutate(win_ratio = (tot_wins/tot_years)*100)

country_agg

# plot only winners of 3 eurovisions or more
top_countries <- country_agg %>%
  filter(tot_wins >= 3) %>%
  ggplot(aes(x = avg_points, y = win_ratio, colour = artist_country)) +
  geom_point() +
  scale_colour_manual(values = met.brewer("Archambault", 11)) +
  labs(title = "Top Eurovision countries!",
       subtitle = "Only winners with 3 or more wins included",
       x = "Average points per competition",
       y = "Win ratio (wins/number years competing)",
       color = "Country") +
  annotate("text", x = 175, y = 14.5, hjust = 'right',
           label = "Ukraine are Eurovision\n superstars with\n 3 wins in 17 years") +
  annotate('segment', x = 175, y = 15, xend = 197, yend = 17.5,
         arrow = arrow(length = unit(2, "mm"))) +
  ggthemes::theme_clean()

top_countries

ggsave(filename = paste0(path, "top_countries.png"), top_countries,
       width = 3508, height = 3508, units = "px", dpi = 500)

# make visualisation with flags and winners ----
# extract two digit flag code to use with ggflags
flags <- eurovision$country_emoji
str_remove(flags, ":flag_") |> str_remove(":") -> flag_emoji
eurovision$flag_emoji <- flag_emoji

# testing out ggflags
eurovision %>%
  filter(year >= 2018) %>%
  filter(year != 2020) %>%
  filter(section == "grand-final") %>%
  ggplot(aes(x = rank, y = total_points, country = flag_emoji)) +
  geom_flag(show.legend = FALSE) +
  facet_wrap(vars(year))

# Look at winners by year, with points on the y. 
# bit of cleaning required
# filter for just finals, winners, and year over 1970
winners <- eurovision %>%
  filter(section == "final" | section == "grand-final") %>%
  filter(winner == TRUE) %>%
  filter(year >= 1970) %>%
  filter(nchar(flag_emoji) <= 2) %>% # deal with any with no flags
  # make text for plot
  mutate(winner_text = paste0(year, ": ", artist, " (", total_points, " Points)"))

# plot with flags, and text with artist and points
ggplot(winners, aes(year, total_points, country = flag_emoji)) +
  geom_bar(stat = "identity", width = 0.05) +
  geom_flag() +
  geom_text(aes(label = winner_text),
            hjust = -0.1, vjust = 0.15, angle = 0,
            family = "Avenir") +
  scale_y_continuous(limits = c(0, 1200)) +
  coord_flip() +
  labs(x = "Year", y = "Points",
       title = "Eurovision winners: 1970 - 2022") +
  theme_minimal(base_family = "Avenir")

# same but cleaner and annotated
png(filename = paste0(path, "eurovision_winners.png"), 
    width = 4000, height = 4000, res = 340, pointsize = 14)
ggplot(winners, aes(year, total_points)) +
  geom_segment(aes(x = year, xend = year, y = 0, yend = total_points)) +
  geom_flag(aes(country = flag_emoji)) + 
  geom_text(aes(label = winner_text),
            hjust = -0.1, vjust = 0.15, angle = 0,
            family = "Avenir", size = 3) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 1000)) +
  labs(x = "", y = "",
       title = "Eurovision winners: 1970 - 2022") +
  # add annotation information
  annotate(geom = "text", x = 1989, y = 975, size = 3,
           label = "1989 winners Yugoslavia\n removed due to no flag",
           family = "Avenir") +
  annotate(geom = "curve", x = 1989, y = 925, xend = 1989, yend = 300, 
           curvature = -0.2, colour = "forestgreen") +
  annotate(geom = "text", x = 2020, y = 975, size = 3,
           label = "No competition in 2020\n due to COVID-19",
           family = "Avenir") +
  annotate(geom = "curve", x = 2020, y = 925, xend = 2020, yend = 500, 
           curvature = 0.035, colour = "forestgreen") +
  theme_minimal(base_family = "Avenir", base_size = 16) +
  theme(plot.title.position = "plot")
dev.off()


# which winners were also the hosts? ----
library(gghighlight)
winners %>%
  ggplot(aes(x = year, y = total_points, colour = host_country)) +
  geom_point() +
  gghighlight(host_country == artist_country)

# trying out some mapping ----
# Lovely example of using k-means on voting
# https://gist.github.com/juliasilge/c5e76731e8dc4e2b709f5afd4ebf9b61

library(maps)

# fix uk so the it matches what we have in the eurovision data
map_df <- map_data('world') %>%
  mutate(region = ifelse(region == "UK", "United Kingdom", region))

# make aggregation for maps
eurovision %>%
  filter(section == "final" | section == "grand-final") %>%
  group_by(artist_country) %>%
  summarise(average_rank = mean(rank, na.rm = TRUE),
            total_wins = sum(winner_int, na.rm = TRUE)) %>%
  mutate(winners_cat = case_when(
    total_wins < 1 ~ "No wins",
    total_wins == 1 ~ "One win",
    total_wins > 1 ~ "Multiple wins"
  )) %>%
  ungroup() %>%
  mutate(winners_cat = factor(winners_cat, levels = c("No wins", "One win", "Multiple wins"))) -> eurovision_rankings

# join up the datasets
eurovision_rankings_map <- map_df %>%
  filter(region %in% eurovision_rankings$artist_country) %>%
  left_join(eurovision_rankings, by = c("region" = "artist_country"))

# which countries have won, or not?
pal <- c('#ff00bb','#f7d600', "#00E1FA")

eurovision_rankings_map %>%
  ggplot(aes(long, lat, group = group, fill = winners_cat)) +
  geom_polygon(colour = "black", linewidth = 0.15) +
  coord_map() +
  scale_fill_manual(values = pal) +
  labs(title = "Which countries have won Eurovision?",
       fill = "How many wins?") +
  theme_map(base_size = 16, 
            base_family = "Avenir") -> winners_map

winners_map

# gradient map of average ranking
eurovision_rankings_map %>%
  ggplot(aes(long, lat, group = group, fill = average_rank)) +
  geom_polygon(colour = "grey80", linewidth = 0.15) +
  coord_map() +
  scale_fill_continuous(trans = 'reverse',
                        low = pal[1], high = pal[2]) +
  labs(title = "What are the average final ranking of countries?",
       fill = "Average Eurovision final ranking") +
  theme_map(base_size = 16, 
            base_family = "Avenir") -> ranking_map

ranking_map

# bring the maps together into one plot
library(patchwork)

winners_map + ranking_map +
  plot_annotation(
    caption = 'Data: TidyTuesday | Visual: Andrew Moles',
    theme = theme(plot.caption = element_text(size = 10, family = "Avenir"))
  ) -> eurovision_maps
eurovision_maps

# save the visuals
ggsave(filename = paste0(path, "winners_map.png"), winners_map, bg = "white",
       width = 4000, height = 3508, units = "px", dpi = 400)
ggsave(filename = paste0(path, "ranking_map.png"), ranking_map, bg = "white",
       width = 4000, height = 3508, units = "px", dpi = 400)
ggsave(filename = paste0(path, "eurovision_maps.png"), eurovision_maps, bg = "white",
       width = 5000, height = 3508, units = "px", dpi = 320)
