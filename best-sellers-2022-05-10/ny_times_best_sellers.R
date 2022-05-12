library(gghighlight)
library(tidyverse)
library(janitor)
# scrap data 
library(rvest)
library(httr)
library(polite)

# load data ----
nyt_titles <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-10/nyt_titles.tsv')
nyt_full <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-10/nyt_full.tsv')

# make titles in title case - looks better ----
nyt_titles <- nyt_titles |> 
  mutate(title = str_to_title(title))
nyt_full <- nyt_full |> 
  mutate(title = str_to_title(title))

nyt_titles[grep("Tartt", nyt_titles$author), ]
nyt_titles[grep("Douglas Adams", nyt_titles$author), ]

# web scraped data from the booker prize ----
# fetch url
url_win <- "https://en.wikipedia.org/wiki/Booker_Prize"
url_win_bow <- bow(url_win)
# extract and convert to table
booker_win_html <-
  polite::scrape(url_win_bow) %>%
  rvest::html_nodes("table.wikitable.sortable") %>%
  rvest::html_table(fill = TRUE)
# make into df and tidy names
booker_winners <-
  booker_win_html[[1]] %>%
  clean_names() %>%
  mutate(author = str_replace(author, "\\[.*", "")) |> 
  rename(genre = genre_s)

# pull authors
booker <- booker_winners |> pull(author) |> unique()
booker <- factor(booker, levels = booker)

booker_nyt <- filter(nyt_titles, author %in% booker) |> pull(author) |> unique()
booker_nyt <- factor(booker_nyt, levels = booker_nyt)

# prep data for plotting ----
plot_df <- nyt_titles |> 
  filter(author %in% booker) |> 
  arrange(author, year) |> 
  group_by(author) |> 
  mutate(num = row_number(), # for calculating number of books per author
         author = fct_reorder(author, num, .fun = max, .desc = TRUE))

# use num to find total books per author
tot_books <- plot_df |> 
  select(-title) |> 
  group_by(author) |> 
  slice_max(num, n = 1)

# plot ----
subt <- str_wrap("Margaret Atwood is the Booker Prize winner with the most books in the NYT Best Seller list (10); Three of her books (The Handmaid's Tale, Cat's Eye, and The Testaments) have been in the list for 10 weeks or more.", 35)
cap <- "Data: Post45 Data | Andrew Moles"

p <- ggplot() +
  geom_text(data = plot_df,
            mapping = aes(x = fct_reorder(author, num, .fun = max, .desc = T), 
                          y = num, label = str_wrap(title, width = 10)),
            size = 3, colour = '#BFFF75', family = "Avenir") +
  gghighlight(total_weeks >= 10, 
              unhighlighted_colour = "white") +
  geom_text(data = tot_books,
            mapping = aes(x = author, y = num+1, label = num),
            size = 5, colour = 'white', family = "Avenir", fontface = 'bold') +
  #ggplot2::geom_text(mapping = ggplot2::aes(label = cap, x="Richard Flanagan", 
   #                      y=10, size = 2, family = "Avenir", colour = "white"), show.legend = F) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(breaks = seq(1, 12, 1)) +
  labs(x = "", y = "", 
       title = str_wrap("Booker Prize Winners in The New York Times Best Seller List", 40),
       subtitle = subt, caption = cap) +
  theme(plot.title = element_text(hjust = 1, vjust = -1, colour = "white",
                                  face = "bold", size = 40, family = "Avenir"),
        plot.subtitle = element_text(hjust = 1, vjust = -1, colour = "white",
                                  face = "bold", size = 25, family = "Avenir"),
        plot.caption = element_text(hjust = 1, vjust = -1, colour = "white",
                                     face = "italic", size = 7, family = "Avenir"),
        axis.text.x = element_text(colour = 'white', family = "Avenir",
                                   size = 10, lineheight = 0.7, vjust = -.2),
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        panel.background = element_rect(fill = "black", colour = "black"),
        plot.background = element_rect(fill = "black", colour = "black"),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = unit(c(0.7, 0.7, 0.5, 0.5), "cm")
        )
p

library(here)
ggsave("best-sellers-2022-05-10/booker_nty.png", p,
       dpi = 280, units = "px", width = 3500, height = 3000)

