library(tidyverse)

freedom <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-22/freedom.csv')

str(freedom)
summary(freedom)
glimpse(freedom)

theme_set(theme_minimal(base_family = "Avenir"))

# coolors fun 
coolors <- function(URL){
  # function takes coolors url, extracts hex codes and adds #
  
  # extract just the hex
  cstr <- gsub("coolors.co", "", URL)
  cstr <- gsub("https:///", "", cstr)
  cstr <- gsub("-", " ", cstr)
  
  # split into individual strings and add #
  cvec <- strsplit(cstr, " ")
  cvec <- paste0("#", cvec[[1]])
  
  return(cvec)
}

# test plot
freedom %>%
  group_by(year, Region_Name) %>%
  summarise(
    avg_civ_lib = mean(CL),
    avg_pol_right = mean(PR)) %>%
  ggplot(aes(x = year, y = avg_civ_lib, size = avg_pol_right, colour = factor(Region_Name))) +
  geom_point() +
#  geom_line() +
  scale_y_reverse()

by_country <- freedom %>%
  group_by(country) %>%
  mutate(avg_civ_lib = mean(CL),
            avg_pol_right = mean(PR)) %>%
  slice(which.min(avg_civ_lib)) %>%
  select(country, Status, Region_Name:avg_pol_right) %>%
  mutate(Status = case_when(
    Status == "F" ~ "Free",
    Status == "NF" ~ "Not Free",
    Status == "PF" ~ "Partially Free"
  ))

by_country <- by_country %>% 
  group_by(Region_Name) %>%
  mutate(avg = mean(avg_civ_lib))

all_civ_lib <- mean(by_country$avg_civ_lib)

url <- "https://coolors.co/175d80-b99009-009b48"

by_country %>%
  ggplot(aes(
    x = forcats::fct_reorder(Region_Name, avg_civ_lib), y = avg_civ_lib, 
    colour = Status, size = avg_pol_right)) +
  geom_jitter(alpha = 0.9, width = 0.15) +
  stat_summary(geom = "point", fun = mean, shape = 20,
               size = 8, colour = "black", alpha = 0.9) +
  geom_hline(yintercept = all_civ_lib, linetype = 2, size = 1.25, alpha = 0.8) +
  geom_segment(aes(x = Region_Name, xend = Region_Name,y = all_civ_lib, yend = avg),
               colour = "black", size = 1, linetype = 2, alpha = 0.8) +
  scale_size(trans = "reverse") + 
  scale_y_reverse() +
  scale_colour_manual(values = coolors(url)) +
  theme(legend.position = "bottom") +
  labs(
    title = "Average civil liberties of regions",
    colour = "Liberty status",
    size = "political right rank",
    x = "Region",
    y = "civil liberty rank (1 = best)"
  ) -> free

free +
  annotate(geom = "text", x = "Oceania", y = all_civ_lib+0.30, 
           label = paste0("Average civil liberty is: ",round(all_civ_lib,2)),
           size = 3, family = "Avenir") +
  annotate(geom = "text", x = "Americas", y = 6, size = 3, family = "Avenir", hjust = "right",
           label = paste0("Asia has the worst average\n civil liberties at ", round(max(by_country$avg),2))) +
  annotate(geom = "curve", x = "Americas", y = 6.01, xend = "Asia", yend = max(by_country$avg),
           curvature = 0.3) +
  annotate(geom = "text", x = "Europe", y = 4, size = 3, family = "Avenir", hjust = "centre",
           label = paste0("Bosnia and Herzegovina is the\n only not free country in Europe")) -> free

free

ggsave("freedom-2022-02-22/freedoms.png", free, width = 10, height = 7, bg = "white")
