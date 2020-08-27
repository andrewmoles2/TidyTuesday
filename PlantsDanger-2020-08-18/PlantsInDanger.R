# load packages ----
library(tidyverse)
library(data.table)
library(networkD3)
library(patchwork)
library(ggforce)
library(gganimate)
library(psych)

# load in data from https://github.com/rfordatascience/tidytuesday/tree/master/data/2020/2020-08-18 ----
actions <- data.table::fread("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-18/actions.csv")
plants <- fread("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-18/plants.csv")
threats <- fread("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-18/threats.csv")

# check data ----
c(str(actions), str(plants), str(threats))

summary(actions)
summary(plants)
summary(threats)

head(actions)
head(plants)
head(threats)

# exploring the data ----
# actions data
# little education, lots of unknown
table(actions$action_type, actions$action_taken == 1, exclude = F)

# plants data
# consistent over time
table(plants$year_last_seen)
# mostly flowering plants becoming extinct
plants %>%
  group_by(group, red_list_category) %>%
  summarise(count = n())
# where going extinct and most at risk
table(plants$country, plants$red_list_category)

# threats - agriculture top hit
threats %>%
  filter(threatened == 1) %>%
  group_by(threat_type) %>%
  summarise(count = n())
# by continent
threatsAgg <- threats %>%
  filter(threatened == 1) %>%
  group_by(continent, country, threat_type) %>%
  summarise(count = n())

# networks ----
plantNet <- select(plants, group, continent, red_list_category)
simpleNetwork(Data = plantNet, Source = 'continent', Target = 'group')

# figures ----
theme_set(theme_bw())
# plant groups going extinct
ggplot(plants, aes(group, fill = red_list_category)) + 
  geom_bar(position = 'dodge') + coord_flip() + 
  labs(title = "Plant groups going extinct")
# and where
ggplot(plants, aes(group, fill = continent)) + 
  geom_bar(position = 'dodge') + coord_flip() + 
  labs(title = "Where are plant groups going extinct")

# actions being taken
actions_filt <- actions %>%
  select(continent, group, action_type, action_taken) %>%
  filter(action_taken == 1) %>%
  group_by(continent, group, action_type) %>%
  summarise(count = n())

ggplot(actions_filt, aes(action_type, count, fill = group)) + 
  geom_bar(position = 'dodge', stat = 'identity') +
  facet_grid(rows = vars(continent)) +
  coord_flip() + labs(title = 'What action is being taken on each contient')

# threats
ggplot(threatsAgg, aes(threat_type, count, fill = continent)) + 
  geom_bar(stat = 'identity', position = 'dodge') + coord_flip()
