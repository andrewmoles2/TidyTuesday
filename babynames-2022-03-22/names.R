# libraries ----
library(babynames)
library(tidyverse)
library(gghighlight)
library(ggbump)
library(geomtextpath)

data(babynames)

# family ----
fam_names <- c("Sebastian", "Andrew", "Robert", "Joan",
               "Barry", "Teresa", "Olivia", "Rosalind",
               "Margarita")

babynames %>%
  filter(name %in% fam_names) %>%
  filter(case_when(
    name == "Sebastian" ~ sex == "M",
    name == "Andrew" ~ sex == "M",
    name == "Robert" ~ sex == "M",
    name == "Joan" ~ sex == "F",
    name == "Barry" ~ sex == "M",
    name == "Teresa" ~ sex == "F",
    name == "Olivia" ~ sex == "F",
    name == "Rosalind" ~ sex == "F",
    name == "Margarita" ~ sex == "F",
  )) -> family_names

median(babynames$prop)
mean(babynames$prop)

family_names %>%
  ggplot(aes(x = year, y = prop, group = name, colour = name)) +
  geom_line(size = 2, alpha = 0.7) +
  scale_x_continuous(breaks = seq(min(babynames$year), max(babynames$year),15)) +
  facet_wrap(vars(name)) +
  theme_minimal()

family_names %>%
  ggplot(aes(x = year, y = prop, group = name, colour = name)) +
  geom_line(size = 2.5, alpha = 0.9, lineend = "round") +
  gghighlight(name == "Andrew", use_group_by = FALSE) +
  scale_x_continuous(breaks = seq(min(babynames$year), max(babynames$year),5)) +
  theme_minimal(base_family = "Avenir")

family_names %>%
  ggplot(aes(x = year, y = prop, colour = name)) +
  geom_bump(size = 2, smooth = 20) +
  scale_x_continuous(breaks = seq(min(babynames$year), max(babynames$year),15)) +
  theme_minimal(base_family = "Avenir") +
  facet_wrap(vars(name))

# work ----
work <- c("Andrew", "Michael", "Nedelin",
          "Jennifer", "Nina", "Mark", 
          "Sue", "Rachel")

babynames %>%
  filter(name %in% work) %>%
  filter(case_when(
    name == "Andrew" ~ sex == "M",
    name == "Michael" ~ sex == "M",
    name == "Nedelin" ~ sex == "M",
    name == "Jennifer" ~ sex == "F",
    name == "Nina" ~ sex == "F",
    name == "Mark" ~ sex == "M",
    name == "Sue" ~ sex == "F",
    name == "Rachel" ~ sex == "F",
  )) -> work_names

work_names %>%
  ggplot(aes(x = year, y = prop, group = name, colour = name)) +
  geom_line(size = 2, alpha = 0.7) +
  scale_x_continuous(breaks = seq(min(babynames$year), max(babynames$year),15)) +
  facet_wrap(vars(name), nrow = 2) +
  theme_minimal()

work_names %>%
  ggplot(aes(x = year, y = prop, group = name, colour = name)) +
  geom_line(size = 2.5, alpha = 0.9, lineend = "round") +
  gghighlight(name == "Jennifer", use_group_by = FALSE) +
  scale_x_continuous(breaks = seq(min(babynames$year), max(babynames$year),5)) +
  theme_minimal(base_family = "Avenir")

work_names %>%
  ggplot(aes(x = year, y = prop, colour = name)) +
  geom_bump(size = 2, smooth = 20) +
  scale_x_continuous(breaks = seq(min(babynames$year), max(babynames$year),15)) +
  theme_minimal(base_family = "Avenir") +
  facet_wrap(vars(name))

work_names %>%
  ggplot(aes(x = year, y = prop, colour = name, label = name)) +
  geom_textpath(text_smoothing = 30, linewidth = 1.2, alpha = 0.7, family = "Avenir",
                show.legend = NA, size = 5, straight = TRUE, hjust = "auto") +
  scale_x_continuous(breaks = seq(min(babynames$year), max(babynames$year),5)) +
  scale_colour_manual(values = sample(MetBrewer::met.brewer("Cross"))) +
  labs(title = "Who in the DSL has the most popular name according to US registered baby names?",
       subtitle = "Appearently, no one in the USA is registered with the name Nedelin...",
       y = "Proportion of baby name total for given year",
       x = "Year name registered",
       caption = "Andrew Moles | TT week 12") +
  theme_minimal(base_family = "Avenir") +
  guides(colour = "none") -> work_names_plot

work_names_plot

ggsave("babynames-2022-03-22/dsl_names.png",
       width = 10, height = 7, bg = "white")

# individuals (can do this with for loops over names)
# either use gghighlight or filter
# Sebastian
babynames %>%
  filter(name == "Sebastian" & sex == "M") %>%
  ggplot(aes(x = year, y = prop)) +
  geom_line() + 
  scale_x_continuous(breaks = seq(min(babynames$year), max(babynames$year),5))

# Andrew
babynames %>%
  filter(name == "Andrew" & sex == "M") %>%
  ggplot(aes(x = year, y = prop)) +
  geom_line() + 
  scale_x_continuous(breaks = seq(min(babynames$year), max(babynames$year),5))

# Robert
babynames %>%
  filter(name == "Robert" & sex == "M") %>%
  ggplot(aes(x = year, y = prop)) +
  geom_line() + 
  scale_x_continuous(breaks = seq(min(babynames$year), max(babynames$year),5))

# Teresa
babynames %>%
  filter(name == "Teresa" & sex == "F") %>%
  ggplot(aes(x = year, y = prop)) +
  geom_line() + 
  scale_x_continuous(breaks = seq(min(babynames$year), max(babynames$year),5))

# Ros
babynames %>%
  filter(name == "Rosalind" & sex == "F") %>%
  ggplot(aes(x = year, y = prop)) +
  geom_line() + 
  scale_x_continuous(breaks = seq(min(babynames$year), max(babynames$year),5))




# example of regex on baby names
babynames %>%
  filter(sex == "M") %>%
  select(name) %>%
  distinct() %>%
  filter(str_detect(name, "ell")) %>%
  filter(!str_detect(name, "ell$") &
           !str_detect(name, "^el") &
           !str_detect(name, "^s"))
