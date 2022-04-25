library(tidyverse)
library(tidytext)
library(vroom)
library(gt)
library(kableExtra)
library(data.table)
library(waffle)
library(hrbrthemes)
library(extrafont)
library(patchwork)

# load in data
big_dave <- vroom('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-19/big_dave.csv')
times <- vroom('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-19/times.csv')
# combine datasets
crosswords <- rbind(big_dave, times)
# tidy up the answer and definitions
crosswords <- crosswords |> 
  mutate(definition = str_to_title(definition),
         answer = str_to_title(answer))
# make into data.table to speed things up a bit
crosswords_dt <- as.data.table(crosswords)

# counts of regular definitions and answers (plants, birds, drinks etc.) ----
#crosswords |> 
#  count(definition) |>
#  arrange(desc(n)) |>
#  filter(!is.na(definition)) |>
#  slice_head(n = 20)

crosswords_dt[!is.na(definition), .(.N), by = .(definition)
              ][order(desc(N))][1:20] -> top_definitions
# top counts of answers and definitions
#crosswords |>
#  count(answer, definition) |>
#  arrange(desc(n)) |>
#  filter(!is.na(answer) & !is.na(definition)) |>
#  slice_head(n = 10)

crosswords_dt[!is.na(answer) & !is.na(definition), .(.N), by = .(answer, definition)
              ][order(desc(N))][1:20] -> top_ans_definitions

# top counts of answers
crosswords_dt[!is.na(answer), .(.N), by = .(answer)
              ][order(desc(N))
                ][nchar(answer) > 2][1:20] -> top_answers

# plot the top counts with waffles ----
ggplot(top_answers[1:6], aes(fill = answer, values = N)) +
  geom_waffle(n_rows = 10, colour = "white", flip = TRUE, make_proportional = TRUE) +
  scale_fill_manual(
    name = NULL, 
    values = c("#6072E6","#2F8C00","#2ec4b6","#e71d36", "#ff9f1c", "#8a30ff"),
    labels = top_answers$answer[1:6]) +
  coord_equal() +
  theme_ipsum(grid="", base_family = "Avenir") +
  theme_enhance_waffle() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Answers") -> ans


# for pictogram - https://fontawesome.com/search?q=drink&s=solid%2Cbrands
# https://nsaunders.wordpress.com/2017/09/08/infographic-style-charts-using-the-r-waffle-package/
ggplot(as.data.frame(top_definitions)[1:6,], aes(label = definition, values = N)) +
  geom_pictogram(aes(colour = definition), n_rows = 10, size = 6, flip = TRUE, make_proportional = TRUE) +
  scale_colour_manual(
    name = NULL, 
    values = c("#6072E6","#2F8C00","#2ec4b6","#e71d36", "#ff9f1c","#8a30ff"),
    labels = top_definitions$definition[1:6]) +
  scale_label_pictogram(
    name = NULL,
    values = c("kiwi-bird", "leaf", "glass-whiskey", "flag-usa", "fish","apple-alt"),
    labels = top_definitions$definition[1:6],
  ) +
  coord_equal() +
  theme_ipsum(grid="", base_family = "Avenir") +
  theme_enhance_waffle() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Definitions") -> def

wrap_plots(ans, def) +
  plot_annotation(
    title = "Top 6 most frequent crossword answers and definitions",
    caption = "Andrew Moles | Data from Cryptic Crossword Clues",
    theme = theme(plot.title = element_text(family = "Avenir", size = 24, hjust = 0.5),
                  plot.caption = element_text(family = "Avenir", size = 10, hjust = 0.5))
    ) -> cross_ans_def

# doesn't save icons - used manual save for now
# ggsave("crosswords-2022-04-19/cross_ans_def.png", cross_ans_def,width = 9, height = 8)

# try some tidytext on clues ----
tidy_crossword <- crosswords |>
  mutate(clue = gsub(x = clue, pattern = "[0-9]+|[[:punct:]]|\\(.*\\)", replacement = "")) |>
  unnest_tokens(word, clue) |>
  anti_join(stop_words)

tidy_crossword |>
  count(word, sort = TRUE) |>
  slice_head(n = 10) |>
  mutate(word = str_to_title(word)) |>
  ggplot(aes(x = word, y = n)) +
  geom_col(fill = "#BDE7D3") +
  coord_flip() +
  scale_y_continuous(breaks = seq(0, 6000, 500)) +
  labs(title = "Top 10 tokens in crossword clues",
       x = "", y = "",
       caption = "Andrew Moles | Data from Cryptic Crossword Clues") +
  theme_minimal(base_family = "Avenir") +
  theme(text = element_text(colour = "#E7BCD0"),
        axis.text = element_text(colour = "#E7BCD0"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) -> crossword_tokens
crossword_tokens
ggsave("crosswords-2022-04-19/crossword_tokens.png", crossword_tokens, 
       bg = "#656565", dpi = 320, width = 6, height = 4.5)

#############################################
# Making some art with the crossword clues! ----
#############################################
# steps: save flow as df -> sample clues to new col in df -> use geomtextpath 
library(aRtsy)
library(geomtextpath)
#pal <- c("#6072E6","#2F8C00","#2ec4b6","#e71d36", "#ff9f1c","#8a30ff")
pal <- MetBrewer::met.brewer("Nattier")
seed <- sample(c(1:20), 1)
set.seed(seed)
noise <- sample(c("svm", "knn", "worley"), 1)
(flow <- aRtsy::canvas_flow(colors = pal, background = "#EEEEEE", 
                   lines = 60, iterations = 30, lwd = 2.7, angles = noise))
flow <- flow$data
flow <- flow |>
  mutate(
    clue = sample(crosswords$clue, nrow(flow), replace = TRUE),
    clue = gsub(x = clue, pattern = "[0-9]+|[[:punct:]]|\\(.*\\)", replacement = "")
  )

ggplot(flow, aes(x, y, group = factor(z), label = clue)) +
  geomtextpath::geom_textpath(
    show.legend = FALSE, colour = flow$color,
    alpha = 0.95, family = "Avenir",
    text_only = TRUE, alpha = 0.99, size = 3
  ) -> clue_flow

background <- "#EEEEEE"
titles <- "#663589"
clue_flow +
  labs(title = "Cryptic crossword clues",
       subtitle = paste0("aRtsy flow field using seed ", seed, " with ", noise, " noise"),
       caption = "Andrew Moles | Data from Cryptic Crossword Clues") +
  theme_void() +
  theme(plot.background = element_rect(fill = background, colour = background),
        panel.background = element_rect(fill = background, colour = background),
        plot.title = element_text(family = "Avenir", size = 18, colour = titles,
                                  hjust = 0.5, margin = margin(t = 20)),
        plot.subtitle = element_text(family = "Avenir", size = 8, colour = titles,
                                     hjust = 0.5),
        plot.caption = element_text(family = "Avenir", size = 8, colour = titles,
                                    hjust = 0.5, margin = margin(b = 20))) -> clue_flow_sav
clue_flow_sav
#theme_canvas(clue_flow, background = "#EEEEEE")
ggsave("crosswords-2022-04-19/clue_flow.png", clue_flow_sav, dpi = 300,
       width = 11, height = 7)


#############################################
# testing out table with gt + kableExtra ----
crosswords |> 
  subset(
    definition == "Plant" |
      definition == "Food" |
      definition == "Instrument"
  ) -> df
df %>%
  group_by(answer) %>%
  mutate(n_answer = n()) %>%
  ungroup() %>%
  group_by(definition) %>%
  mutate(n_definition = n()) %>%
  ungroup() -> df

answer_range <- range(df$n_answer, na.rm = T)
cross_list <- split(df$n_answer, df$definition)

gt_plot <- function(table_data, column, plot_data, plot_fun, ...){
  text_transform(
    table_data,
    # note the use of {{}} here - this is tidy eval
    # that allows you to indicate specific columns
    locations = cells_body(columns = vars({{column}})),
    fn = function(x){
      plot <- map(plot_data, plot_fun, width = 300, height = 70, same_lim = TRUE, ...)
      plot_svg <- map(plot, "svg_text")
      map(plot_svg, gt::html)
    }
  )
}
cross_list %>% str()
tibble(definition = c("Food", "Instrument", "Plant"),
       boxplot = "", hist = "", poly = "") %>%
  gt() %>%
  gt_plot(column = boxplot, plot_data = cross_list, plot_fun = spec_boxplot, lim = answer_range) %>%
  gt_plot(column = hist, plot_data = cross_list, plot_fun = spec_hist, lim = answer_range) %>%
  gt_plot(column = poly, plot_data = cross_list, plot_fun = spec_plot, polymin = 5, ylim = answer_range)

