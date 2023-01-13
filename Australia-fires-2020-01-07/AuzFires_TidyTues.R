## loading libraries plus data------
library(tidyverse)
#library(tidytuesdayR)
library(RColorBrewer)
library(gganimate)
library(ggtext)
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
  geom_line(aes(color = city_name), size = 2.5, alpha = 0.85) +
  scale_color_brewer(palette = "Dark2") +
    labs(y = "Yearly average temperature °C (Celsius)",
         colour = "City name") +
  scale_y_continuous(limits = c(10, 25), breaks = seq(10,25, by = 1)) +
  scale_x_continuous(breaks = seq(1900, 2025, 10)) +
  theme(plot.title.position = 'plot')

# rainfall
rainPlot <- rainSummary %>% filter(year > 1900) %>%
ggplot(aes(x = year, y = yearlyRain, group = city_name)) + 
  geom_line(aes(colour = city_name), size = 2.5, alpha = 0.85) +
  #xlim(1900 , max(rainSummary$year)) +
  scale_color_brewer(palette = "Dark2") +
  labs(y = "Yearly average rain frequency (millimeters)",
       colour = "City name") +
  scale_y_continuous(limits = c(0, 3500), breaks = seq(0, 3500, 500)) +
  scale_x_continuous(breaks = seq(1900, 2025, 10))

# combine
auzFire_static <- rainPlot / tempPlot +
  plot_annotation(title = "Average yearly temperature and rainful in Australian cities",
                  subtitle = "From 1900 to 2019",
                  caption = "Australia Fires 2020-01-07 A.P.Moles")
auzFire_static 

ggsave(here('Australia-fires-2020-01-07', 'Auz_Rain&Temp.png'), auzFire_static,
       width = 9.5, height = 8.5, dpi = 300)

# adding in ggannimate
rainPlotAnn <- rainSummary %>% filter(year > 1900) %>%
  ggplot(aes(x = year, y = yearlyRain, group = city_name)) + 
  geom_line(aes(colour = city_name), size = 1.5, alpha = 0.85) +
  #xlim(1900 , max(rainSummary$year)) +
  scale_color_brewer(palette = "Dark2") +
  labs(title = 'Yearly average rainfall (millimeters) in Australian cities', 
       subtitle = 'Year: {as.integer(frame_along)}',
       y = 'Yearly average rain frequency (millimeters)',
       colour = 'City Name',
       caption = "Australia Fires 2020-01-07 A.P.Moles") +
  scale_y_continuous(limits = c(0, 3500), breaks = seq(0, 3500, 500)) +
  scale_x_continuous(breaks = seq(1900, 2025, 10)) +
  theme(plot.title.position = 'plot',
        panel.grid.minor = element_blank()) +
  transition_reveal(year) +
  ease_aes('linear')

rainAnnimate <- gganimate::animate(rainPlotAnn, width = 500, height = 440)

tempPlotAnn <- ggplot(tempSummary, aes(x = year, y = yearlyAveTemp, group = city_name)) + 
  geom_line(aes(color = city_name), size = 1.5, alpha = 0.85, show.legend = TRUE) +
  scale_color_brewer(palette = "Dark2") +
  labs(title = 'Average yearly temperature °C (Celsius) in Australian cities', 
       subtitle = 'Year: {as.integer(frame_along)}',
       y = 'Yearly average temperature °C (Celsius)',
       colour = 'City Name',
       caption = "Australia Fires 2020-01-07 A.P.Moles") +
  scale_y_continuous(limits = c(10, 25), breaks = seq(10,25, by = 1)) +
  scale_x_continuous(breaks = seq(1900, 2025, 10)) +
  theme(plot.title.position = 'plot',
        panel.grid.minor = element_blank()) +
  transition_reveal(year) +
  ease_aes('linear')

tempAnnimate <- gganimate::animate(tempPlotAnn, width = 500, height = 440)

# putting the gifs together 
gganimate::anim_save(here('Australia-fires-2020-01-07', 'Auz_Rain.gif'), rainAnnimate)
gganimate::anim_save(here('Australia-fires-2020-01-07', 'Auz_Temp.gif'), tempAnnimate)

rain_mgif <- magick::image_read(here('Australia-fires-2020-01-07', 'Auz_Rain.gif'))
temp_mgif <- magick::image_read(here('Australia-fires-2020-01-07', 'Auz_Temp.gif'))

raintemp_gif <- image_append(c(rain_mgif[1], temp_mgif[1]))
for(i in 2:100){
  combined <- image_append(c(rain_mgif[i], temp_mgif[i]))
  raintemp_gif <- c(raintemp_gif, combined)
}

# view combined gif
raintemp_gif

#saving
image_write(raintemp_gif, here('Australia-fires-2020-01-07', 'Auz_Rain&Temp.gif'))

# adding legend to title
theme_set(theme_bw(base_family = "Avenir"))
RColorBrewer::brewer.pal(6, "Dark2")

rain_t <- "<span style = 'font-size:16pt; font-family:Avenir;'>Yearly average rainfall (millimeters) in Australian cities</span><br>
<span style = 'color:#1B9E77;'>Adelaide</span>,
<span style = 'color:#D95F02;'>Brisbane</span>,
<span style = 'color:#7570B3;'>Canberra</span>,
<span style = 'color:#E7298A;'>Melbourne</span>,
<span style = 'color:#66A61E;'>Perth</span>, and
<span style = 'color:#E6AB02;'>Sydney</span><br>
Year: {as.integer(frame_along)}"

rainPlotAnn2 <- rainSummary %>% filter(year > 1900) %>%
  ggplot(aes(x = year, y = yearlyRain, group = city_name)) + 
  geom_line(aes(colour = city_name), size = 1.5, alpha = 0.85, show.legend = FALSE) +
  #xlim(1900 , max(rainSummary$year)) +
  scale_color_brewer(palette = "Dark2") +
  labs(title = rain_t,
       y = "",
       x = "") +
  scale_y_continuous(limits = c(0, 3500), breaks = seq(0, 3500, 500)) +
  scale_x_continuous(breaks = seq(1900, 2025, 10)) +
  theme(plot.title.position = 'plot',
        plot.title = element_markdown(size = 12, lineheight = 1.2),
        panel.grid.minor = element_blank()) +
  transition_reveal(year) +
  ease_aes('linear')
rainAnnimate2 <- gganimate::animate(rainPlotAnn2, width = 500, height = 440)

temp_t <- "<span style = 'font-size:16pt; font-family:Avenir;'>Average yearly temperature °C (Celsius) in Australian cities</span><br>
<span style = 'color:#1B9E77;'>Adelaide</span>,
<span style = 'color:#D95F02;'>Brisbane</span>,
<span style = 'color:#7570B3;'>Canberra</span>,
<span style = 'color:#E7298A;'>Melbourne</span>,
<span style = 'color:#66A61E;'>Perth</span>, and
<span style = 'color:#E6AB02;'>Sydney</span><br>
Year: {as.integer(frame_along)}"

tempPlotAnn2 <- ggplot(tempSummary, aes(x = year, y = yearlyAveTemp, group = city_name)) + 
  geom_line(aes(color = city_name), size = 1.5, alpha = 0.85, show.legend = FALSE) +
  scale_color_brewer(palette = "Dark2") +
  labs(title = temp_t,
       y = '',
       x = "",
       caption = "Australia Fires 2020-01-07 A.P.Moles") +
  scale_y_continuous(limits = c(10, 25), breaks = seq(10,25, by = 1)) +
  scale_x_continuous(breaks = seq(1900, 2025, 10)) +
  theme(plot.title.position = 'plot',
        plot.title = element_markdown(size = 12, lineheight = 1.2),
        panel.grid.minor = element_blank()) +
  transition_reveal(year) +
  ease_aes('linear')

tempAnnimate2 <- gganimate::animate(tempPlotAnn2, width = 500, height = 440)

gganimate::anim_save(here('Australia-fires-2020-01-07', 'Auz_Rain2.gif'), rainAnnimate2)
gganimate::anim_save(here('Australia-fires-2020-01-07', 'Auz_Temp2.gif'), tempAnnimate2)

rain_mgif2 <- magick::image_read(here('Australia-fires-2020-01-07', 'Auz_Rain2.gif'))
temp_mgif2 <- magick::image_read(here('Australia-fires-2020-01-07', 'Auz_Temp2.gif'))

raintemp_gif2 <- image_append(c(rain_mgif2[1], temp_mgif2[1]))
for(i in 2:100){
  combined2 <- image_append(c(rain_mgif2[i], temp_mgif2[i]))
  raintemp_gif2 <- c(raintemp_gif2, combined2)
}

raintemp_gif2

image_write(raintemp_gif2, here('Australia-fires-2020-01-07', 'Auz_Rain&Temp2.gif'))