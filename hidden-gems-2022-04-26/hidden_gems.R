library(tidyverse)
library(tidytext)
library(cowplot)

hidden_gems <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-26/hidden_gems.csv')

# idea - do sentiment analysis of review column
# first let's get some counts of tokens from the review
hidden_gems_tidy <- hidden_gems %>%
  unnest_tokens(word, review) %>%
  anti_join(stop_words)

hidden_gems_tidy %>%
  count(word, sort = T)

# get and count positive sentiments
positive <- get_sentiments("bing") %>%
  filter(sentiment == "positive")

hidden_gems_tidy %>%
  semi_join(positive) %>%
  count(word, sort = T)

# get all sentiments and plot positive and negative
bing <- get_sentiments("bing")

hidden_gems_sentiment <- hidden_gems_tidy %>%
  inner_join(bing) %>%
  count(sentiment, date, sort = T) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(
    sentiment = positive - negative,
    colour = ifelse(hidden_gems_sentiment$sentiment >0, "#20BC68", "#BB203F"))

ggplot(hidden_gems_sentiment, aes(x = date, y = sentiment, fill = colour)) +
  geom_bar(stat = 'identity', show.legend = FALSE) +
  scale_fill_identity() +
  scale_x_date(date_breaks = "3 months", date_minor_breaks = "1 month") +
  theme_minimal(base_family = "Avenir") +
  labs(title = "Hidden Gem Kaggle review sentiments",
       subtitle = "Overall postive sentiments from reviews",
       caption = "Andrew Moles | Martin Henze Kaggle Hidden Gem reviews",
       x = "Date of review", y = "Postive or negetive sentiment") -> over_time
over_time

ggsave("hidden-gems-2022-04-26/sentiments_over_time.png", over_time, 
       bg = "#F3F3F3", width = 7, height = 6)

# most common positive and negative words
hg_word_counts <- hidden_gems_tidy %>%
  inner_join(bing) %>%
  count(word, sentiment, sort = T) %>%
  filter(n > 3) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(word = reorder(word, n)) %>%
  mutate(colour = if_else(n > 0, "#20BC68", "#BB203F"))

hg_word_counts %>%
  ggplot(aes(word, n, fill = colour)) +
  geom_col() + 
  scale_fill_identity() +
  scale_y_continuous(breaks = seq(-10, 40, 5)) +
  coord_flip() + 
  labs(title = "Most common positive and negative words in the Hidden Gems reviews",
       y = "Contribution to sentiment", x = "Word",
       caption = "Andrew Moles | Martin Henze Kaggle Hidden Gem reviews") +
  theme_minimal(base_family = "Avenir") +
  annotate(geom = 'text', x = "strong", y = 25, hjust = 'centre',
           label = "Hidden gem notebooks show\n postive sentiments for clear writing", 
          family = "Avenir", size = 3) +
  annotate(geom = 'curve', x = 'powerful', y = 25, xend = 'clean', yend = 30,
           curvature = .3, arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = 'curve', x = 'powerful', y = 25, xend = 'comprehensive', yend = 15,
           curvature = -0.3, arrow = arrow(length = unit(2, "mm"))) -> top_sentiments
top_sentiments

ggsave("hidden-gems-2022-04-26/top_sentiments.png", top_sentiments, 
       bg = "#F3F3F3", width = 7.5, height = 6)

cowplot::plot_grid(top_sentiments, over_time, nrow = 2, align = 'v') -> review_text_mining
review_text_mining

ggsave("hidden-gems-2022-04-26/review_text_mining.png", review_text_mining, 
       bg = "#F3F3F3", width = 7.5, height = 7.5, dpi = 320)
