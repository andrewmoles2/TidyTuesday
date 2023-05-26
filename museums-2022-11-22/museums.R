# libraries
library(tidyverse)
library(easystats)
library(janitor)
library(lattice)
library(PostcodesioR)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(patchwork)

# import coolors function
devtools::source_gist("ffa7f0bae82f9df88c6c1d9458d980a8")

# options to build maps 
# option 1: use geo grid - https://github.com/gkaramanis/tidytuesday/blob/master/2022/2022-week_26/paygap.R and https://github.com/gkaramanis/tidytuesday/blob/master/2022/2022-week_47/museums.R
# build geo grid of postcodes
#library(geogrid)
#polygons <- sf::read_sf(file.path(getwd(), "museums-2022-11-22/data/postcode_polygons.gpkg")) %>%
#  rmapshaper::ms_simplify()
#polygons_grid <- calculate_grid(shape = polygons, grid_type = "hex", seed = 3)
#gb_grid <- assign_polygons(polygons, polygons_grid)
#if(file.exists("museums-2022-11-22/data/gb_grid") == FALSE) {dir.create("museums-2022-11-22/data/gb_grid")}
#sf::st_write(gb_grid, file.path(getwd(), "museums-2022-11-22/data/gb_grid/gb_grid.shp"))

# option 2: use rnaturalearth - https://github.com/nrennie/tidytuesday/blob/main/2022/2022-06-28/20220628.R 
uk <- ne_countries(scale = 'medium', type = 'map_units', returnclass = 'sf', continent = 'Europe')
uk <- subset(uk, sov_a3 == "GB1")

# load in data
# might also want area dep data from gov too (for mapping)
museums <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-22/museums.csv') |>
  janitor::clean_names()

report(museums)

# have a look at Cambridge
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
  filter(n_museums >= 5) |>
  arrange(desc(avg_dep)) -> affluent_museums)

# Read in grid shapefile
gb_grid <- sf::read_sf(file.path(getwd(), "museums-2022-11-22/data/gb_grid/gb_grid.shp"))
gb_post <- read_sf("museums-2022-11-22/data/postcode_polygons.gpkg")

# Group data by post code
# change this from subject matter to year categories
# < 1800, 1800-1900, 1950, 1950-2000, 2000 > 

museums %>% group_by(village_town_or_city) %>%
  summarise(avg_year = mean(year_opened, na.rm = T)) %>%
  arrange(avg_year)
summary(museums$year_opened) # most are 1950-1990

mus_post_agg <- museums %>%
  filter(str_detect(year_closed, "9999")) %>%
  select(museum_id, name_of_museum, postcode, subject_matter) %>%
  mutate(
    pc_area = str_extract(postcode, "[A-Z][A-Z]*"),
    subjm = case_when(
      str_detect(subject_matter, "-") ~ str_extract(subject_matter, "^.*?-"),
      TRUE ~ subject_matter
    ),
    subjm = str_remove(subjm, "-"),
    subjm = str_replace_all(subjm, "_", " ")
  ) %>%
  count(pc_area, subjm) %>%
  group_by(pc_area) %>%
  slice_max(order_by = n, n = 1) %>%
  ungroup() %>%
  add_count(pc_area, name = "ties") %>%
  mutate(subjm = if_else(ties > 1, "Multiple", subjm))

# join data to geo data
mus_type <- gb_grid %>%
  left_join(mus_post_agg)
mus_type2 <- gb_post %>%
  left_join(mus_post_agg)

lvls <- sort(unique(mus_type$subjm))
f1 <- "Avenir"
pal <- coolors("https://coolors.co/669900-99cc33-ccee66-006699-3399cc-990066-cc3399-ff6600-ff9900-ffcc00")

# plot - two types, grid and post code areas
ggplot(mus_type) +
  geom_sf(aes(fill = subjm), color = "white") +
  scale_fill_manual(values = pal, breaks = lvls, drop = FALSE) +
  guides(fill = guide_legend(title = "Subject Matter", override.aes = list(color = "grey95"))) +
  coord_sf(clip = "off") +
  theme_void(base_family = f1) +
  theme(
    plot.background = element_rect(fill = "grey95", color = NA),
    legend.position = c(1.2, 0.6),
    plot.margin = margin(0, 150, 0, 0)
  ) +
  plot_annotation(
    title = "Most common museums by postal code area",
    subtitle = "From about 3 400 museums that have opened since 1621 and are still open",
    caption = "Source: Mapping Museums project · Graphic: Georgios Karamanis"
  ) &
  theme(
    plot.background = element_rect(fill = "grey97", color = NA),
    plot.title = element_text(family = f1, face = "bold", size = 20, margin = margin(10, 0, 5, 0)),
    plot.subtitle = element_text(family = f1, size = 12),
    plot.caption = element_text(family = f1, margin = margin(0, 0, 10, 0))
  )

ggplot(mus_type2) +
  geom_sf(aes(fill = subjm), color = "white") +
  scale_fill_manual(values = pal, breaks = lvls, drop = FALSE) +
  guides(fill = guide_legend(title = "Subject Matter", override.aes = list(color = "grey95"))) +
  coord_sf(clip = "off") +
  theme_void(base_family = f1) +
  theme(
    plot.background = element_rect(fill = "grey95", color = NA),
    legend.position = c(1.2, 0.6),
    plot.margin = margin(0, 150, 0, 0)
  ) +
  plot_annotation(
    title = "Most common museums by postal code area",
    subtitle = "From about 3 400 museums that have opened since 1621 and are still open",
    caption = "Source: Mapping Museums project · Graphic: Georgios Karamanis"
  ) &
  theme(
    plot.background = element_rect(fill = "grey97", color = NA),
    plot.title = element_text(family = f1, face = "bold", size = 20, margin = margin(10, 0, 5, 0)),
    plot.subtitle = element_text(family = f1, size = 12),
    plot.caption = element_text(family = f1, margin = margin(0, 0, 10, 0))
  )

# using natural earth to visualise dep index
# add in labels for cities with highest avg dep index, and adjust colours and look

gradient_pal <- coolors("https://coolors.co/e2691e-d6c1aa")

museum_prep <- museums |> 
  filter(year_closed == '9999:9999') |>
  filter(accreditation != 'Unaccredited') |>
  filter(latitude < 70) |>
  filter(size != "unknown") |>
  mutate(dep_high = if_else(area_deprivation_index > 8, gradient_pal[1], gradient_pal[2]))
 
ggplot() +
  geom_sf(data = uk, fill = "white") +
  geom_point(data = museum_prep,
             aes(x = longitude, y = latitude, colour = dep_high),
             size = 1, alpha = 0.7) +
  scale_colour_identity() +
  labs(title = "Museums deprivation indices",
       subtitle = "Orange shows more affluence") +
  ggthemes::theme_map()

# apply clustering, like k-means to see any groupings of dep and location
museum_prep %>%
  rowwise() %>%
  mutate(
    mean_dep = mean(c_across(cols = c(area_deprivation_index:area_deprivation_index_services)), na.rm = TRUE),
    median_dep = median(c_across(cols = c(area_deprivation_index:area_deprivation_index_services)), na.rm = TRUE)
  ) -> museum_prep

best_c <- function(data, ...) {
  clust <- data |>
    dplyr::select(...) %>%
    NbClust::NbClust(method = 'kmeans', min.nc = 2, max.nc = 15)
  best <- table(clust$Best.nc[1,]) |>
    as.data.frame()
  best <- best[which.max(best$Freq),1] |> as.character() |> as.integer()
  return(best)
}

best <- best_c(museum_prep, 'latitude', 'longitude', 'mean_dep')

museum_prep %>%
  select(latitude:longitude, area_deprivation_index) %>%
  drop_na() %>%
  kmeans(centers = best, nstart = 25) -> museum_kmeans

museum_prep %>%
  drop_na(area_deprivation_index) %>%
  mutate(cluster = as_factor(museum_kmeans$cluster),
         cluster_name = case_when(
           cluster == 1 ~ "Deprived" ,
           cluster == 2 ~ "Average",
           TRUE ~ "Affluent"
         )) -> museum_prep

museum_prep %>%
  group_by(cluster) %>%
  summarise(n = n(),
            avg_dep = mean(mean_dep))

(kmeans_plot <- ggplot() +
  geom_sf(data = uk, fill = "#529444") +
  geom_point(data = museum_prep,
             aes(x = longitude, y = latitude, colour = cluster_name),
             size = 1, alpha = 0.7) +
    guides(fill = guide_legend(override.aes = list(color = "#DBE8EB"))) +
  labs(title = "K-means clustering of museums location and its deprivation index",
       subtitle = "From 1650 accredited museums that are still open",
       caption = "Source: Mapping Museums project | Andrew Moles",
       colour = "Cluster") +
  scale_colour_manual(values = rev(c("#CD7F32","#C0C0C0","#FFD700"))) +
  coord_sf(clip = "off") +
  theme_void(base_family = "Avenir") +
  theme(legend.position = c(0.85, 0.6),
        plot.margin = margin(0, 0, 0, 50),
        plot.background = element_rect(fill = "#DBE8EB", colour = "#DBE8EB"),
        plot.title = element_text(size = 14, face = "bold", family = "Avenir",
                                  hjust = 0.8),
        plot.subtitle = element_text(size = 10, family = "Avenir", hjust = -.8),
        plot.caption = element_text(size = 9, family = "Avenir"))
  )

ggsave("museums-2022-11-22/kmean_museum.png", kmeans_plot, units = "px",
       dpi = 320, width = 2200, height = 3200)

# maybe look at north south divide? Or south (everything under Birmingham), north (Leeds and up), Midlands (Birmingham)
get_lat_long <- function(place) {
  x <- PostcodesioR::place_query(place)
  lat <- x[[1]][["latitude"]]
  long <- x[[1]][["longitude"]]
  return(c(latitude = lat, longitude = long))
}
get_lat_long("Cambridge")

birm <- get_lat_long("Birmingham")
leeds <- get_lat_long("Leeds")

museum_prep %>%
  mutate(divide = ifelse(latitude > birm[1] & latitude < leeds[1], "midlands",
                         ifelse(latitude >= leeds[1], "north", "south"))) %>%
  group_by(divide ,size) %>%
  summarise(avg_dep = mean(area_deprivation_index, na.rm = TRUE),
            total = sum(area_deprivation_index, na.rm = TRUE)) %>%
  arrange(size)

ggplot() +
  geom_sf(data = uk, fill = "white") +
  geom_point(data = museum_prep,
             aes(x = longitude, y = latitude, colour = dep_high),
             size = 1, alpha = 0.7) +
  scale_colour_identity() +
  annotate(geom = 'curve', x = get_lat_long("London")[2], y = get_lat_long("London")[1],
           xend = 0, yend = 50.05) +
  annotate("text", label = "London", x = 0, y = 50)