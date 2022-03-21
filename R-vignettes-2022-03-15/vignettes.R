library(tidyverse)
library(lubridate)
library(gganimate)
library(MetBrewer)
library(ggimage)
library(poissoned)
library(deldir)

bioc <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-15/bioc.csv')
cran <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-15/cran.csv')

# data prep ----
fav_packages <- c("ggplot2", "DiagrammeR", "rayshader", "dplyr", "knitr", "rmarkdown",
                  "lubridate", "rvest", "tidymodels", "patchwork")

#grepl("^[[:digit:]]",(substr(cran$date,4,nchar(cran$date))))

cran_favs <- cran %>%
  filter(package %in% fav_packages) %>%
  mutate(date_clean = case_when(
    grepl("^[[:digit:]]", substr(date, 1, 4)) ~ as_date(date),
    TRUE ~ as.Date(substr(date, 5, nchar(date)), "%b %d %H:%M:%S %Y")),
    year = year(date_clean),
    month = month(date_clean, label = TRUE),
    week = week(date_clean))

#cheat to summarise data later
cran_favs$counter <- 1

df <- cran_favs %>%
  # check for complete, and give na to incomplete
  complete(package = fav_packages, year = min(year):max(year)) %>%
  group_by(package, year) %>%
  summarise(releases = sum(counter)) %>%
  mutate(releases = replace_na(releases, 0)) %>%
  # get first release data
  merge(cran_favs %>% group_by(package) %>% summarise(first = min(date_clean)) %>% arrange(first), by="package")

# plot! ----
theme_set(theme_minimal(base_family = "Avenir"))

# year by year plot
scales::show_col(RColorBrewer::brewer.pal(9, "Blues"))
background <- RColorBrewer::brewer.pal(9, "Greys")[2]
text_col <- "#E70F37"
lines <- RColorBrewer::brewer.pal(9, "Blues")[7]

p <- ggplot(df, aes(x = year, y = releases, group = factor(package))) +
  geom_point(aes(size = releases), colour = lines) +
  geom_path(size = 2, colour = lines) +
  scale_x_continuous(breaks = seq(2007, 2021, 2)) +
  scale_y_continuous(breaks = seq(0, 10, 2), limits = c(0,11)) +
  facet_wrap(vars(package), nrow = 6)

p <- p + theme(
  legend.position = "bottom",
  panel.grid.major = element_line(linetype = "dashed"),
  panel.grid.minor = element_line(linetype = "dashed"),
  strip.text = element_text(colour = text_col, face = "bold", size = 12),
  plot.title = element_text(colour = text_col, size = 20, hjust = 0.5),
  plot.subtitle = element_text(colour = text_col, size = 16, hjust = 0.5),
  plot.caption = element_text(size = 8, colour = text_col, hjust = 0.5),
  axis.title = element_text(size = 12, colour = text_col),
  axis.text = element_text(colour = text_col),
  plot.background = element_rect(fill = background, colour = background),
  panel.background = element_rect(fill = background, colour = background)
) + guides(size = "none")
p

# animate
p_an <- p + gganimate::transition_reveal(year) +
  labs(title = "My favourite R packages and their releases", 
       subtitle = "Year: {as.integer(frame_along)}",
       x = "Year", y = "Package Releases",
       caption = "R Vignettes | Andrew Moles | 2022-03-15")
p_an

gganimate::anim_save("R-vignettes-2022-03-15/Fav_packages.gif", width = 600, height = 440)

# R shapes plot
title <- RColorBrewer::brewer.pal(9,"Blues")[3]
axis <- RColorBrewer::brewer.pal(9,"Blues")[3]

p2 <- ggplot(df, aes(x = year, y = package, colour = releases, size = releases^2)) +
  geom_point(shape = 82) +
  scale_x_continuous(breaks = seq(2007, 2021, 1)) +
  scale_colour_gradient(low = RColorBrewer::brewer.pal(9,"Blues")[1], 
                        high = RColorBrewer::brewer.pal(9,"Blues")[4]) +
  guides(colour = 'none', size = 'none') +
  labs(x = "",
       y = "") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 18, colour = title,
                                  hjust = 0.5, margin = margin(t = 5)),
        plot.subtitle = element_text(size = 14, colour = title, hjust = 0.5),
        plot.caption = element_text(size = 8, colour = title, hjust = 0.5, 
                                    margin = margin(b = 5)),
        axis.text = element_text(size = 10, colour = axis)
        )
p2
# add grey to light orange gradient background
scales::show_col(RColorBrewer::brewer.pal(9,"Blues"))
MetBrewer::MetPalettes

# make grey gradient background
pts <- poisson_disc(ncols = 150, nrows = 400, cell_size = 2, xinit = 150, yinit = 400, keep_idx = TRUE)
back <- ggplot(pts, aes(x = x, y = y, colour = idx)) +
  geom_point(size = 10, alpha = 0.7) +
  scale_colour_gradientn(colours = met.brewer("Ingres") ,guide = "none") +
  theme_void() +
  ggplot2::coord_cartesian(xlim = c(min(pts$x)+15, max(pts$x)-15), ylim = c(min(pts$y)+30, max(pts$y)-30))
back
ggsave("R-vignettes-2022-03-15/background.png", back, 
       width = 8, height = 8)

p2_back <- ggimage::ggbackground(p2, "R-vignettes-2022-03-15/background.png")
p2_back

ggsave("R-vignettes-2022-03-15/Fav_packages.png", p2_back, width = 8, dpi = 320)
