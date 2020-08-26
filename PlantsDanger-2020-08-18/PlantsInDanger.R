# load packages
library(tidyverse)
library(data.table)

# load in data from https://github.com/rfordatascience/tidytuesday/tree/master/data/2020/2020-08-18
actions <- data.table::fread("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-18/actions.csv")
plants <- fread("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-18/plants.csv")
threats <- fread("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-18/threats.csv")

