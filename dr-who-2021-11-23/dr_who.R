# inspiration from <https://github.com/tashapiro/TidyTuesday/tree/master/2021/W48>
library(tidyverse)
library(png)
library(ggimage)
library(RColorBrewer)
library(ggthemes)
library(here)
library(patchwork)

# load in the seperate datasets ----
directors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-11-23/directors.csv')
episodes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-11-23/episodes.csv')
writers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-11-23/writers.csv')
imdb <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-11-23/imdb.csv')

# make a list of doctors per season ----
doctor <- c("Christopher Eccleston","David Tennant","David Tennant","David Tennant",
          "Matt Smith","Matt Smith","Matt Smith","Peter Capaldi","Peter Capaldi",
          "Peter Capaldi","Jodie Whittaker","Jodie Whittaker","Jodie Whittaker")
season <- 1:13
doctors <- data.frame(season,doctor)

# clean up missing season data on special episodes ----
episodes %>%
  mutate(
    season_number = case_when(
      first_aired >= "2008-12-25" & first_aired <= "2010-01-01" ~ 4,
      first_aired >= "2013-11-23" & first_aired <= "2013-12-25" ~ 7,
      TRUE                                                      ~ season_number
    )
  ) -> episodes

# make main data through joins ----
df_who <- left_join(episodes, directors, by = "story_number")
df_who <- left_join(df_who, doctors, by = c("season_number" = "season"))

df_who %>%
  mutate(
    director = factor(director),
    doctor = factor(doctor, levels = c("Christopher Eccleston", "David Tennant",
                                       "Matt Smith", "Peter Capaldi", "Jodie Whittaker"))
  ) -> df_who

# set up palettes ----
pal <- c(
  "#B4301B", # dark red (CE)
  "#38631F", # dark green (DT)
  "#6A51A3", # purple (MS)
  "#0455EB", # vivid blue (PC)
  "#FFA62B" # vivid orange (JW)
)

scales::show_col(pal)

# image path to data frame ----
df_who %>%
  mutate(
    image = case_when(
      doctor == "Peter Capaldi" ~ here("dr-who-2021-11-23", "dr-img", "peter_c.png"),
      doctor == "David Tennant" ~ here("dr-who-2021-11-23", "dr-img", "david_t.png"),
      doctor == "Matt Smith" ~ here("dr-who-2021-11-23", "dr-img", "matt_s.png"),
      doctor == "Jodie Whittaker" ~ here("dr-who-2021-11-23", "dr-img", "jodie_w.png"),
      doctor == "Christopher Eccleston" ~ here("dr-who-2021-11-23", "dr-img", "chris_e.png")
    )
  ) -> df_who

# average rating and viewers ----
df_who %>%
  group_by(doctor) %>%
  mutate(
    avg_dr_rating = mean(rating, na.rm = TRUE),
    avg_dr_viewers = mean(uk_viewers, na.rm = TRUE),
    avg_rating = mean(avg_dr_rating, na.rm = TRUE)
  )  -> df_who

# dr who specials ----
df_who %>%
  filter(type == "special") %>%
  ungroup() %>%
  summarise(range(rating)) %>%
  pull() -> rating_range

df_who %>%
  filter(type == "special") %>%
  ggplot(aes(x = first_aired, y = rating)) + 
  geom_smooth(method = "lm", se = FALSE, 
              colour = "black", size = 1.5,
              linetype = 8) +
  geom_image(aes(image = image), asp = 1.5) +
  scale_y_continuous(limits = c(75, 90)) +
  labs(title = "Dr Who specials IMDB rating over time",
       colour = "Doctor",
       y = "IMDB rating",
       x = "Date special first aired",
       caption = "Dr Who 2021-11-23 A.P.Moles") +
  ggthemes::theme_hc(base_family = "Avenir") 

(df_who %>%
  filter(type == "special") %>%
  ggplot(aes(x = first_aired, rating, colour = doctor)) +
  geom_point(size = 8, alpha = 0.85) +
  geom_smooth(method = "lm", se = FALSE, 
              colour = "black", size = 1.5,
              linetype = 8) +
  labs(subtitle = "Specials",
       colour = "Doctor",
       y = "IMDB rating",
       x = "Date special first aired") +
  scale_colour_manual(values = pal[2:5]) +
  scale_y_continuous(limits = c(75, 90)) +
  scale_x_date(limits = as.Date(c("2005-03-26", "2021-12-05"))) +
  ggthemes::theme_hc(base_family = "Avenir") +
  guides(colour = "none") -> specials_plot)
#ggthemes::theme_solarized_2(base_family = "Avenir")

# episodes ----
(df_who %>%
  filter(type != "special") %>%
  ggplot(aes(x = first_aired, rating, colour = doctor)) +
  geom_smooth(method = "lm", se = FALSE, 
              colour = "black", size = 1.5,
              linetype = 8) +
  geom_point(size = 8, alpha = 0.8) +
  scale_colour_manual(values = pal) +
  ggthemes::theme_hc(base_family = "Avenir") +
  labs(subtitle =  "Episodes",
       colour = "Doctor",
       y = "IMDB rating",
       x = "Date episode first aired") + 
  scale_y_continuous(limits = c(75, 95)) -> episodes_plot)

df_who %>%
  filter(type != "special") %>%
  ggplot(aes(x = first_aired, y = rating)) +
  geom_image(aes(image = image)) +
  geom_smooth(method = "lm", se = FALSE, 
              colour = "black", size = 1.5,
              linetype = 8) +
  scale_y_continuous(limits = c(75, 95)) +
  ggthemes::theme_hc(base_family = "Avenir") +
  labs(title = "Dr Who episodes IMDB rating over time",
       colour = "Doctor",
       y = "IMDB rating",
       x = "Date episode first aired",
       caption = "Dr Who 2021-11-23 A.P.Moles") 

# by doctor ----
(df_who %>%
  ggplot(aes(x = doctor, y = rating)) +
  geom_jitter(size = 7, width = 0.25, alpha = 0.8, aes(colour = doctor)) +
  geom_hline(aes(yintercept = avg_rating, colour = doctor), size = 1, linetype = 2) +
  geom_segment(aes(x = doctor, xend = doctor, y = avg_rating, yend = avg_dr_rating, colour = doctor), size = 1) +
  geom_point(aes(x = doctor, y = avg_dr_rating),colour = "black", size = 4) +
  geom_image(aes(x = doctor, y = avg_dr_rating, image = image), asp = 1.5) +
  scale_colour_manual(values = pal) + 
  ggthemes::theme_hc(base_family = "Avenir") +
  scale_y_continuous(limits = c(75, 95)) +
  labs(title = "Dr Who episodes IMDB rating by Doctor",
       x = "Doctor", y = "IMDB rating",
       caption = "Dr Who 2021-11-23 A.P.Moles") +
  guides(colour = "none") -> by_doctor)

# combine plots ----
(by_time <- (specials_plot /  episodes_plot) + 
  plot_annotation(title = "Dr Who specials and episodes IMDB rating over time",
                  caption = "Dr Who 2021-11-23 A.P.Moles"))

# save ----
ggsave(here("dr-who-2021-11-23", "avg_doctor_rating.png"), by_doctor,
       dpi = 300, width = 11, height = 7)

ggsave(here("dr-who-2021-11-23", "doctor_rating_time.png"), by_time,
       dpi = 300, width = 10, height = 7.5)
 
