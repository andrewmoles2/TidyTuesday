# libraries ---
library(tidyverse)
library(janitor)
library(ggtext)
library(ggimage)
library(here)
# https://twitter.com/search?q=%23TidyTuesday&src=typed_query
# images from: https://github.com/AbdoulMa/TidyTuesday/tree/main/2023/2023_w29/detectors_logos

# load data ---
detectors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-18/detectors.csv') |>
  clean_names()

research_paper <- "https://www.cell.com/patterns/fulltext/S2666-3899(23)00130-7"

head(detectors)
summary(detectors)

# prep and cleaning ---

detectors_clean <- detectors |>
  filter(!is.na(native)) |>
  mutate(native_clean = if_else(native == "Yes", "Native English", "Non-Native English")) |>
  group_by(detector, native_clean) |>
  mutate(accuracy = sum(kind == pred_class) / n()) |>
  ungroup()

accuracy_summary <- detectors_clean |>
  summarise(
    accurcy = sum(kind == pred_class) / n(),
    .by = c(detector, native_clean)
  ) |>
  arrange(detector)

accuracy_summary_wide <- pivot_wider(accuracy_summary, 
                                     names_from = native_clean,
                                     values_from = accurcy)

title <- "Are GPT detectors biased against non-native English writers?<br>
<span style = 'font-size:14pt;'>Tools designed to detect generative language models (GPT)
are less accurate<br> at detecting <span style = 'color:#538D7D;'>non-native English</span> speakers to 
<span style = 'color:#8D5364;'>native English</span> speakers</span>"

plot_text <- "<b>GPT Detectors Are Biased Against Non-Native English Writers<br> 
              Weixin Liang, Mert Yuksekgonul, Yining Mao, Eric Wu, James Zou (2023)</b><br>
              <i>The study authors carried out a series of experiments passing a number of<br> essays 
              to different GPT detection models. Juxtaposing detector predictions<br> for papers written
              by native and non-native English writers, the authors argue<br> that GPT detectors
              disproportionately classify real writing from non-native<br> English writers as AI-generated.</i>"

detectors_logos_df <- tribble(
  ~detector, ~logo,
  "ZeroGPT", "https://github.com/AbdoulMa/TidyTuesday/blob/main/2023/2023_w29/detectors_logos/zero_gpt.png?raw=true",
  "Crossplag", "https://github.com/AbdoulMa/TidyTuesday/blob/main/2023/2023_w29/detectors_logos/cross_plag.png?raw=true",
  "GPTZero", "https://github.com/AbdoulMa/TidyTuesday/blob/main/2023/2023_w29/detectors_logos/gpt_zero.png?raw=true",
  "HFOpenAI", "https://github.com/AbdoulMa/TidyTuesday/blob/main/2023/2023_w29/detectors_logos/hf.png?raw=true",
  "Sapling", "https://github.com/AbdoulMa/TidyTuesday/blob/main/2023/2023_w29/detectors_logos/sapling.png?raw=true",
  "Quil", "https://github.com/AbdoulMa/TidyTuesday/blob/main/2023/2023_w29/detectors_logos/quill.png?raw=true",
  "OriginalityAI", "https://github.com/AbdoulMa/TidyTuesday/blob/main/2023/2023_w29/detectors_logos/originality.png?raw=true"
)

accuracy_summary_wide |>
  left_join(detectors_logos_df) |>
  mutate(
    fancy_label = glue::glue("<img src='{logo}' width='50'/><br/><span>{detector}</span>")
  ) -> accuracy_summary_wide

label <- accuracy_summary_wide$fancy_label

# plot ---

detectors_plot <- ggplot(accuracy_summary_wide) +
  geom_segment(aes(x = reorder(detector, `Non-Native English`), xend = detector,
                   y = `Non-Native English`, yend = `Native English`),
               colour = "grey30", linewidth = 1.2) +
  geom_point(aes(x = reorder(detector, `Non-Native English`), y = `Non-Native English`), 
             colour = "#538D7D", size = 5) +
  geom_point(aes(x = reorder(detector, `Non-Native English`), y = `Native English`), 
             colour = "#8D5364", size = 5) +
  geom_image(aes(x = detector, y = -0.05, image = logo),
             asp = 1.3) +
  annotate("richtext",
    x = 5.5, y = 0.005, label = plot_text, vjust = 0.5, hjust = 0,
    family = "Avenir", fill = NA, label.color = NA, size = 3.5,
    label.padding = grid::unit(rep(0, 4), "pt")
  ) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.10), labels = scales::percent) +
  coord_flip() +
  labs(x = "", y = "Average Accuracy Per Detector",
       title = title,
       caption = "Source: detectors R package | Graphic: Andrew Moles") +
  theme(plot.title.position = "plot",
        plot.title = element_textbox_simple(
          size = 20, lineheight = 1, padding = margin(0, 0, 5, 0)),
        text = element_text(family = "Avenir"),
        axis.text.x = element_markdown(color = "black", size = 11),
        axis.text = element_text(size = 11),
        plot.background = element_rect(fill = "#F1EFEA"),
        panel.background = element_rect(fill = "#F1EFEA"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(colour = "grey80"),
        panel.grid.minor = element_line(colour = "grey80"),
        axis.ticks = element_blank())

detectors_plot

# save ----
ggsave(here("detectors-2023-07-18", "AI_detectors.png"), detectors_plot,
       dpi = 350, width = 13, height = 9, device = ragg::agg_png)
