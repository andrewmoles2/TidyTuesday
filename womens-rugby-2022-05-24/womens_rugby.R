library(tidyverse)
library(lubridate)
library(gt)
library(gtExtras)

sevens <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-24/sevens.csv')
fifteens <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-24/fifteens.csv')

fifteens <- fifteens |> 
  mutate(year = year(date))

# filter for just World Cup, pivot long, calculate wins and results
fifteens_long <- fifteens |>
  filter(tournament == "World Cup") |> 
  pivot_longer(cols = c(team_1, team_2), 
                names_to = "Home_away", 
               values_to = "team") |>
  relocate(team) |> 
  select(team, score_1:score_2, margin_of_victory:loser, year) |> 
  mutate(result = ifelse(team == winner, margin_of_victory, -margin_of_victory)) |> 
  mutate(win = ifelse(result == 0, 0.5, ifelse(result > 0, 1, 0)))

# can either do full agg of all world cups or do best team each year
fifteens_year <- fifteens_long |> 
  group_by(team, year) |> 
  summarise(wins = length(win[win == 1]),
            losses = length(win[win == 0]),
            draws = length(win[win == 0.5]),
            played = sum(wins, losses, draws),
            points_difference = mean(result),
            form = list(win),
            .groups = "drop") |> 
  arrange(desc(year), desc(wins)) |> 
  mutate(rank = row_number()) |> 
  relocate(rank)

fifteens_agg <- fifteens_long |> 
  group_by(team) |> 
  summarise(wins = length(win[win == 1]),
            losses = length(win[win == 0]),
            draws = length(win[win == 0.5]),
            played = sum(wins, losses, draws),
            points_difference = mean(result), 
            form = list(win),
            .groups = "drop") |> 
  arrange(desc(wins), desc(points_difference)) |> 
  mutate(rank = row_number()) |> 
  relocate(rank)

# visualise using GT!
# total agg
fifteens_agg %>%
  gt() %>%
  gt_theme_nytimes() %>%
  tab_style(
    style = list(cell_text(color = "blue")),
    locations = cells_body(columns = c(wins),
                           rows = wins > 10)
  ) %>%
  tab_style(
    style = list(cell_text(color = "red")),
    locations = cells_body(columns = c(wins),
                           rows = wins <= 10)
  ) %>%
  gt_plt_winloss(column = form, max_wins = 38)

# last 2 world cups
# ideas to quickly improve
# add in for and against trys, calculate points, remove played
years_tab <- fifteens_year %>%
  filter(year == 2017 | year == 2014) %>%
  select(-rank) %>%
  gt() %>%
  gt_theme_espn() %>%
  tab_style(
    style = list(cell_text(color = "blue")),
    locations = cells_body(columns = c(points_difference),
                           rows = points_difference >= 0)) %>%
  tab_style(
    style = list(cell_text(color = "red")),
    locations = cells_body(columns = c(points_difference),
                           rows = points_difference < 0)) %>%
  gt_plt_winloss(column = form) %>%
  tab_style(
    style = cell_borders(sides = "bottom", color = "black", weight = px(1)),
    locations = cells_body(rows = 12)) %>%
  tab_style(
    style = cell_borders(sides = "right", color = "black", weight = px(1)),
    locations = cells_body(columns = 1)
  ) %>%
  cols_align(align = "center", columns = c(points_difference, form)) %>%
  tab_header(
    title = md("**2017 and 2014 Womens Rugby World Cup tables**"),
    subtitle = md(glue("**<span style = 'color:#e30613'>New Zealand</span>** have high points difference across both world cups."))
  ) %>%
  tab_source_note(
    source_note = md("DATA: Womens rubgy: TidyTuesday 24/05/22.<br>Table:  Andrew Moles | Inspired by @steodosescu.")
  )

gtsave(years_tab, "womens-rugby-2022-05-24/womens_rugby.png")
