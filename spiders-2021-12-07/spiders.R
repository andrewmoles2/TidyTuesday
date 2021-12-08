library(tidyverse)
library(ggExtra)
library(showtext)
library(ggforce)
library(ggraph)
library(igraph)
library(network)
font_add_google("Offside")
font_add_google("Rambla")
showtext_opts(dpi = 320)
showtext_auto(enable = TRUE)

spiders <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-12-07/spiders.csv')

spiders %>% glimpse()

# use code to make spider in network dendrogram ----
auz_spiders <- spiders %>%
  filter(str_detect(distribution, "Australia"))

auz_spiders %>% group_by(family) %>% 
  summarise(n_genus = n_distinct(genus), n = n()) %>%
  arrange(desc(n)) -> family
auz_spiders %>% group_by(family, genus) %>% 
  summarise(n = n()) %>%
  arrange(desc(n)) -> genus
auz_spiders %>% group_by(family, genus, species) %>% 
  summarise(n = n()) %>%
  arrange(desc(n)) -> species

fam_sample <- family %>%
  select(family) %>%
  slice(1:3) %>% pull()

family_sample <- genus %>% 
  filter(family %in% fam_sample) %>% 
  select(family, genus) %>%
  rename("from_id" = family, "to_id" = genus) %>%
  mutate(family = from_id, level = "family")

genus_species_sample <- species %>%
  filter(family %in% fam_sample)

genus_sample <- genus_species_sample %>%
  rename("from_id" = genus, "to_id" = species) %>%
  select(from_id, to_id) %>%
  mutate(level = "genus")

species_sample <- genus_species_sample %>% 
  rename("from_id" = species) %>% 
  select(from_id) %>% 
  mutate(to_id = NA, level = "species")

sample <- rbind(family_sample, genus_sample)

sample$size<-ifelse(sample$level=='family',family[match(sample$from_id,family$family),]$n,
                    ifelse(sample$level=='genus',genus[match(sample$from_id,genus$genus),]$n, 1))


graph <- graph_from_data_frame(sample)

ggraph(graph, layout = "drl") +
  geom_edge_link(aes(colour = factor(family))) +
  geom_node_point()

ggraph(graph, layout = "kk") +
  geom_edge_fan(aes(alpha = ..index.., colour = family)) +
#  geom_edge_link(aes(colour = factor(family))) +
  geom_node_point() +
  scale_edge_alpha(guide = "none") +
  ggtitle("Sample of the most common Australian spiders") +
  theme_graph(background = 'grey20', text_colour = 'white')

ggraph(graph, 'dendrogram') + 
  geom_edge_diagonal()

ggraph(graph, layout = 'fr', weights = size) + 
  geom_edge_link(aes(colour = family)) + 
  geom_node_point() + 
  ggtitle("Sample of the most common Australian spiders") +
  theme_graph(background = 'grey20', text_colour = 'white')

ggraph(graph, "matrix") +
  geom_edge_point(aes(colour = family), mirror = TRUE)

pal <- RColorBrewer::brewer.pal(n = 3, "Set3")

ggraph(graph, 'stress') + 
  geom_node_voronoi(max.radius = 0.5, colour = 'white') + 
  geom_edge_link(aes(colour = family)) + 
  geom_node_point() +
  #geom_node_label(aes(label = family)) +
  labs(title = "The three most common Australian spider families",
       caption = "2021-12-07_A.P.Moles") +
  theme_graph(base_family = "Avenir") +
  scale_colour_manual(values = pal) +
  scale_edge_colour_manual(values = pal) -> network_of_auz_spiders

ggsave("spiders-2021-12-07/network_of_auz_spiders.png", network_of_auz_spiders,
       dpi = 320, width = 7, height = 6.5)

# https://github.com/HudsonJamie/tidy_tuesday/blob/main/2021/week_50/spiders.R
# using example from link above
malaysia_spiders <- spiders %>%
  filter(stringr::str_detect(distribution, "Malaysia")) %>%
  group_by(family) %>%
  summarise(n_species = n()) %>%
  ungroup() %>%
  slice_max(order_by = n_species, n = 10) %>% 
  mutate(family = fct_reorder(family, -n_species),
         family.fac = factor(family),
         family.num = as.numeric(as.factor(family)))

ggplot(malaysia_spiders) +
  geom_segment(mapping = aes(x = family.fac, y = n_species, xend = family.fac), yend = 0,
               linetype = "dashed", colour = "grey40",
               size = 0.5) +
  geom_ellipse(aes(x0 = family.num, y0 = n_species - 1.5, b = 2.2,
                   a = 0.12, angle = 0), 
               fill = "#40342A",
               colour = "#9C8150") +
  geom_curve(mapping = aes(x = family.num, xend = family.num + 0.3,
                           y = n_species + 3, yend = n_species + 6), size = 0.4,
             curvature = -0.4, colour = "#9C8150") + # bottom right leg
  geom_curve(mapping = aes(x = family.num, xend = family.num + 0.3,
                           y = n_species + 2.5, yend = n_species + 4), size = 0.4,
             curvature = -0.25, colour = "#9C8150") + # 2nd bottom right leg
  geom_curve(mapping = aes(x = family.num, xend = family.num + 0.3,
                           y = n_species + 2, yend = n_species), size = 0.4,
             curvature = 0.25, colour = "#9C8150") + #2nd top right leg
  geom_curve(mapping = aes(x = family.num, xend = family.num + 0.3,
                           y = n_species + 1.5, yend = n_species - 3), size = 0.4,
             curvature = 0.4, colour = "#9C8150") + # top right leg
  geom_curve(mapping = aes(x = family.num, xend = family.num - 0.3,
                           y = n_species + 3, yend = n_species + 6), size = 0.4,
             curvature = 0.4, colour = "#9C8150") + # bottom left leg
  geom_curve(mapping = aes(x = family.num, xend = family.num - 0.3,
                           y = n_species + 2.5, yend = n_species + 4), size = 0.4,
             curvature = 0.25, colour = "#9C8150") + # 2nd bottom left leg
  geom_curve(mapping = aes(x = family.num, xend = family.num - 0.3,
                           y = n_species + 2, yend = n_species), size = 0.4,
             curvature = -0.25, colour = "#9C8150") + #2nd top left leg
  geom_curve(mapping = aes(x = family.num, xend = family.num - 0.3,
                           y = n_species + 1.5, yend = n_species - 3), size = 0.4,
             curvature = -0.4, colour = "#9C8150") + # top left leg
  geom_text(aes(x = family.num, y = n_species - 1.5, label = n_species),
            colour = "white", size = 2, 
            family = "Rambla") +
  geom_text(aes(x = family.num - 0.2, y = 0, label = toupper(family)),
            colour = "grey50", size = 2.5, angle = -90, hjust = 0,
            family = "Rambla") +
  geom_point(mapping = aes(x = family.fac, y = n_species + 2),
             size = 4, pch = 21,
             fill = "#40342A",
             colour = "#9C8150") +
  geom_text(x = 6.2, y = -75, label = "SPIDERS OF MALAYSIA",
            colour = "BLACK", size = 9, hjust = 0.5,
            family = "Offside") +
  geom_text(x = 6.3, y = -85, label = "The 10 families of       found in Malaysia with the highest number of individual species",
            colour = "BLACK", size = 3, hjust = 0.5,
            family = "Rambla") +
  geom_text(x = 4.15, y = -105, label = "spider",
            colour = "BLACK", size = 3, hjust = 0.5, angle = 20,
            family = "Rambla") +
  geom_segment(x = 4.15, y = -85, xend = 4.15, yend = -103,
               linetype = "dashed", colour = "grey50",
               size = 0.3) +
  geom_segment(x = 4.05, y = -85, xend = 4.15, yend = -88,
               linetype = "dashed", colour = "grey50",
               size = 0.3) +
  geom_segment(x = 4.25, y = -85, xend = 4.15, yend = -88,
               linetype = "dashed", colour = "grey50",
               size = 0.3) +
  labs(x = NULL,
       y = NULL,
       caption = "@jamie_bio | source = World Spider Database") +
  scale_y_reverse() +
  theme(panel.background = element_rect(fill = "#F8F3EA"),
        plot.background = element_rect(fill = "#F8F3EA"),
        panel.border = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 25,
                                  family = "Offside"),
        plot.subtitle = element_text(size = 10,
                                     family = "Rambla"),
        plot.caption = element_text(size = 5,
                                    family = "Offside")) +
  removeGrid()





