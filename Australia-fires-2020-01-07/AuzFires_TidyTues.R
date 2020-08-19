## loading libraries plus data------
library(tidyverse)
#library(tidytuesdayR)
library(RColorBrewer)
library(gganimate)
library(ggforce)
library(patchwork)
library(magick)
library(here)

#tuesData <- tidytuesdayR::tt_load("2020-01-07")
# load in data from github repo
rainfall <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-07/rainfall.csv') %>%
  mutate(date = as.Date(paste0(year,"-", month, "-", day)))
temperature <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-07/temperature.csv')

## data manipulation----

# make year variable for plotting
temperature$year <- format(as.Date(temperature$date, format="%d/%m/%Y"),"%Y")

# rain aggregate (sum per year)
rainSummary <- rainfall %>%
  filter(!is.na(rainfall)) %>%
  select(city_name, year, rainfall) %>%
  group_by(city_name, year) %>%
  summarise(yearlyRain = sum(rainfall))

# temp aggregate (mean per year)
tempSummary <- temperature %>%
  select(city_name, temperature, year) %>%
  group_by(city_name, year) %>%
  summarise(yearlyAveTemp = mean(temperature))
# change names to cities
tempSummary$city_name <- if_else(tempSummary$city_name == "KENT", "ADELAIDE",
                                 if_else(tempSummary$city_name == "PORT", "PORT LINCOLN", tempSummary$city_name))
# make year numeric
tempSummary$year <- as.numeric(tempSummary$year)
# drop port as it has so few data points
tempSummary <- subset(tempSummary, tempSummary$city_name != "PORT LINCOLN")
# fix names to be titles
tempSummary$city_name <- str_to_title(tempSummary$city_name)

##Plotting-----
theme_set(theme_bw())

# temperature (remove port lincoln)
tempPlot <- ggplot(tempSummary, aes(x = year, y = yearlyAveTemp, group = city_name)) + 
  geom_line(aes(color = city_name)) + geom_smooth(aes(color = city_name), se = FALSE) +
  scale_color_brewer(palette = "Dark2") 

# rainfall
rainPlot <- rainSummary %>% filter(year > 1900) %>%
ggplot(aes(x = year, y = yearlyRain, group = city_name)) + 
  geom_line(aes(colour = city_name)) + geom_smooth(aes(color = city_name), se = FALSE) +
  xlim(1900 , max(rainSummary$year)) +
  scale_color_brewer(palette = "Dark2")

# combine
auzFire_static <- rainPlot / tempPlot
auzFire_static

# adding in ggannimate
rainPlotAnn <- rainSummary %>% filter(year > 1900) %>%
  ggplot(aes(x = year, y = yearlyRain, group = city_name)) + 
  geom_line(aes(colour = city_name))  +
  xlim(1900 , max(rainSummary$year)) +
  scale_color_brewer(palette = "Dark2") + 
  labs(title = 'Yearly rainfall in Australian cities', 
       subtitle = 'Year: {as.integer(frame_along)}',
                    y = 'Rain frequency',
                    colour = 'City Name') +
  transition_reveal(year) +
  ease_aes('linear')

rainAnnimate <- gganimate::animate(rainPlotAnn, width = 500, height = 440)

tempPlotAnn <- ggplot(tempSummary, aes(x = year, y = yearlyAveTemp, group = city_name)) + 
  geom_line(aes(color = city_name)) +
  scale_color_brewer(palette = "Dark2") +
  labs(title = 'Average yearly temperature in Australian cities', 
       subtitle = 'Year: {as.integer(frame_along)}',
       y = 'Average temperature',
       colour = 'City Name') +
  transition_reveal(year) +
  ease_aes('linear')

tempAnnimate <- gganimate::animate(tempPlotAnn, width = 500, height = 440)

# putting the gifs together 
rain_mgif <- magick::image_read(rainAnnimate)
temp_mgif <- magick::image_read(tempAnnimate)

raintemp_gif <- image_append(c(rain_mgif[1], temp_mgif[1]))
for(i in 2:100){
  combined <- image_append(c(rain_mgif[i], temp_mgif[i]))
  raintemp_gif <- c(raintemp_gif, combined)
}

# view combined gif
#raintemp_gif

#saving
image_write(raintemp_gif, here('Australia-fires-2020-01-07', 'Auz_Rain&Temp.gif'))
