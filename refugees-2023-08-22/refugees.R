# https://populationstatistics.github.io/refugees/ 
#install.packages("refugees")

library(refugees)
library(tidyverse)

ref_22 <- refugees::population |>
  filter(year >= 2022)

ref_22 |>
  group_by(coo_name) |>
  mutate(total_refugees = sum(refugees, na.rm = TRUE) + sum(oip, na.rm = TRUE)) |>
  ungroup() |>
  group_by(coa_name) |>
  mutate(total_asylum_seekers = sum(asylum_seekers, na.rm = TRUE)) |>
  ungroup() -> ref_22

summary(ref_22)

ref_22 |>
  filter(total_asylum_seekers > 14000) |>
  ggplot(aes(x = reorder(coa_name, total_asylum_seekers), y = total_asylum_seekers)) +
  geom_point() +
  coord_flip()

ref_22 |>
  filter(coa_iso == "GBR") |>
  slice_max(order_by = total_refugees, n = 20) |>
  ggplot(aes(x = reorder(coo_name, total_refugees), y = total_refugees)) +
  geom_point() +
  coord_flip()

# sankey plot of where refugees are coming from
