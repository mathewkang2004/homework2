# Install and load tools --------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata, scales, data.table)


# Set year ----------------------------------------------------------------
year <- 2014:2019


# Stack data --------------------------------------------------------------
final_ma <- year %>%
  map_dfr(~ {
    file_path <- paste0("data/output/data-", .x, ".csv")
    read_csv(file_path, col_types = cols(.default = "c")) %>%
      mutate(year_new = as.integer(.x)) %>%
      select(-contains("year"), year_new) %>%
      rename(year = year_new)
  })


# Save data ---------------------------------------------------------------
write_csv(final_ma,"data/output/final_ma.csv")

