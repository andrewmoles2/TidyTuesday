# libraries
library(tidyverse)
library(janitor)
library(easystats)
library(lattice)

# load in data
# might also want area dep data from gov too (for mapping)
museums <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-22/museums.csv') |>
  clean_names()

report(museums)

# have a look at cambridge
cambridge <- museums |> 
  filter(village_town_or_city == "Cambridge")

# clean up year opened
museums$year_opened <- as.integer(strtrim(museums$year_opened, width = 4))

# find most Affluent and numerous museum areas
(museums |> 
  filter(year_closed == '9999:9999') |>
  filter(accreditation != 'Unaccredited') |>
  group_by(village_town_or_city) |>
  summarise(
    n_museums = n(),
    avg_dep = mean(area_deprivation_index, na.rm = TRUE),
    avg_year_open = mean(year_opened, na.rm = TRUE),
    oldest = min(year_opened, na.rm = TRUE),
    newest = max(year_opened, na.rm = TRUE)
    ) |>
  ungroup() |>
  filter(n_museums >= 10) |>
  arrange(avg_year_open) -> affluent_museums)
