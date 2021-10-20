library(tidyverse)
library(janitor)
library(lubridate)
library(patchwork)
library(here)

lemurs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-24/lemur_data.csv')

col_scale <- c("#F5B250","#E0A048","#CC8F41",
               "#B77E3A","#A26E33","#8E5E2C",
               "#7A4F25","#66401E","#523317")

scales::show_col(col_scale)

# heavy lemurs
(lemurs %>%
  mutate(name = str_to_title(name)) %>%
  group_by(name) %>%
  summarise(avg_weight = mean(weight_g, na.rm = TRUE),
            avg_age = mean(age_at_wt_y, na.rm = TRUE),
            sd = sd(weight_g, na.rm = TRUE)) %>%
  slice_max(avg_weight, n = 10) %>%
  mutate(name = fct_reorder(name, avg_weight)) %>%
  ggplot(aes(x = name, y = avg_weight, fill = avg_age)) +
  geom_bar(stat = 'identity') + 
  geom_errorbar(aes(ymin = avg_weight-sd, ymax = avg_weight+sd),
                width = 0.3, colour = "darkblue") +
  coord_flip() +
  labs(title = "The top 10 heaviest lemurs on average",
       x = "Name of lemur",
       y = "Mean weight in grams",
       fill = "Mean age") +
  scale_fill_continuous(low = col_scale[1], high = col_scale[9]) +
  theme_bw() -> heavy_lemurs)

# births per year
(lemurs %>%
  mutate(year = year(dob)) %>%
  group_by(year) %>%
  summarise(lemur_births_year = n()) %>%
  mutate(avg_births = mean(lemur_births_year)) %>%
  ggplot(aes(x = year, y = lemur_births_year)) +
  geom_line() +
  geom_area(fill = col_scale[5]) +
  geom_hline(aes(yintercept = avg_births[1]), linetype = 2) +
  labs(title = "Number of lemur births per year") +
  theme_bw() -> lemur_births)


(lemur_plots <- heavy_lemurs/lemur_births +
  plot_annotation(caption = "Tidy Tuesday 2021-08-24 A.P.Moles"))

ggsave(here("Lemurs_2021-08-24", "lemur_plots.png"), lemur_plots,
       width = 6, height = 5, dpi = 150)

# extra ideas
lemurs %>%
  group_by(name) %>%
  summarise(avg_weight = mean(weight_g, na.rm = TRUE),
            avg_age = mean(age_at_wt_y, na.rm = TRUE)) %>%
  ggplot(aes(x = avg_weight, y = avg_age)) +
  geom_point()

lemurs %>%
  group_by(dob) %>%
  summarise(dob_count = n()) %>%
  ggplot(aes(x = dob, y = dob_count)) +
  geom_line()
