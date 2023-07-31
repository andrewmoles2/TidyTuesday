library(tidyverse)
library(vroom)
library(fs)

# data downloaded from https://gender-pay-gap.service.gov.uk/viewing/download

# make fs file list
pay_files <- fs::dir_ls(path = "paygap-2022-06-28/data/", 
                       glob = "*.csv")

# use vroom to read and bind files
raw_df <- vroom::vroom(pay_files)

# tidy things up a bit
clean_df <- raw_df |>
  janitor::clean_names() |>
  mutate(
    across(c(due_date, date_submitted), lubridate::as_datetime),
    employer_name = str_remove_all(employer_name, "\""),
    employer_name = str_replace_all(employer_name, ", |,", ", ")
  )

# write out dataset
write_csv(clean_df, "paygap-2022-06-28/data/paygap.csv")