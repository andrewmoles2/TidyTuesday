library(tidyverse)
library(ggimage)
library(ggflags)
library(patchwork)
library(countrycode)
library(glue)

winners <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-04-25/winners.csv')
london_marathon <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-04-25/london_marathon.csv')

winners <- winners %>%
  mutate(
    flag_code = countrycode(Nationality, origin = 'country.name', destination = 'iso2c') %>%
      tolower()
  ) %>%
  mutate(plot_text = glue("{Year} - {Athlete}: {Time}"))

men_winners <- subset(winners, Category == "Men")
women_winners <- subset(winners, Category == "Women")

font <- "Alegreya"

men <- men_winners %>%
  ggplot(aes(x = Year, y = Time, country = flag_code)) +
  #geom_segment(aes(xend = Year, yend = hms("00:00:00"))) +
  geom_flag() +
  geom_text(aes(label = plot_text), family = font,
            check_overlap = TRUE, hjust = -0.07) +
  coord_flip() +
  scale_y_time(expand = expansion(mult = c(0.01, 0.3))) +
  labs(title = "Winning times and years for male London Marathon winners",
       subtitle = "Kenya and Ethiopia have dominated since 2003",
       caption = "Visual: Andrew Moles | Data: Nicola Rennie",
       x = "", y = "") +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(family = font, size = 18),
    plot.subtitle = element_text(family = font, size = 14),
    plot.caption = element_text(family = font, size = 8, hjust = 0.5),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "#ECEBE6"),
    panel.background = element_rect(fill = "#ECEBE6")
  )
men

women <- women_winners %>%
  ggplot(aes(x = Year, y = Time, country = flag_code)) +
  geom_flag() +
  geom_text(aes(label = plot_text), family = font,
            check_overlap = TRUE, hjust = -0.07) +
  coord_flip() +
  scale_y_time(expand = expansion(mult = c(0.01, 0.3))) +
  labs(title = "Winning times and years for female London Marathon winners",
       subtitle = "Kenya and Ethiopia have dominated since 2010",
       caption = "Visual: Andrew Moles | Data: Nicola Rennie",
       x = "", y = "") +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(family = font, size = 18),
    plot.subtitle = element_text(family = font, size = 14),
    plot.caption = element_text(family = font, size = 8, hjust = 0.5),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "#ECEBE6"),
    panel.background = element_rect(fill = "#ECEBE6"))
women

ggsave("london-marathon-2023-04-25/men_winners.png", men, dpi = 320,
       units = "px", width = 3300, height = 2800)
ggsave("london-marathon-2023-04-25/women_winners.png", women, dpi = 320,
       units = "px", width = 3300, height = 2800)

# other ideas if I had time. Add cool marathon image as background, or add in emojis too

#ggimage::ggbackground(men, "london-marathon-2023-04-25/london-marathon-route-map.pdf")

# add emoji from https://emojipedia.org/search/?q=run
men_winners %>%
  ggplot(aes(Year, Time)) +
  geom_image(image = "https://em-content.zobj.net/thumbs/120/apple/354/man-running_1f3c3-200d-2642-fe0f.png")
