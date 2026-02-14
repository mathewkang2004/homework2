# Install and load tools --------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata, scales, data.table)


# Import data -------------------------------------------------------------
hhi_2018 <- read_csv("data/output/hhi_2018.csv", 
                     col_types = cols(
                       fips = col_double(),
                       hhi = col_double(),
                       avg_ffscost = col_double(),
                       bid = col_double(),
                       avg_eligibles = col_double()
                     ))

source("submission3/analysis/q5_comparison.R")

# Create buckets based on FFS cost quartiles ---------------------------
ffs_qs <- quantile(bid_comparisons$avg_ffscost, probs = c(0.25, 0.50, 0.75), na.rm = TRUE)

bid_comparisons <- bid_comparisons %>%
  mutate(
    ffs_q1 = if_else(avg_ffscost < ffs_qs[1], 1, 0),
    ffs_q2 = if_else(avg_ffscost >= ffs_qs[1] & avg_ffscost < ffs_qs[2], 1, 0),
    ffs_q3 = if_else(avg_ffscost >= ffs_qs[2] & avg_ffscost < ffs_qs[3], 1, 0),
    ffs_q4 = if_else(avg_ffscost >= ffs_qs[3], 1, 0)
  ) %>%
  filter(!is.na(treated))


# Create the summary table ------------------------------------------------
q6_summary_table <- bid_comparisons %>%
  mutate(ffs_quartile = case_when(
    ffs_q1 == 1 ~ "Quartile 1 (Low Cost)",
    ffs_q2 == 1 ~ "Quartile 2",
    ffs_q3 == 1 ~ "Quartile 3",
    ffs_q4 == 1 ~ "Quartile 4 (High Cost)"
  )) %>%
  group_by(ffs_quartile, hhi_group) %>%
  summarize(mean_bid = mean(bid, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = hhi_group, values_from = mean_bid) %>%
  mutate(across(where(is.numeric), ~ scales::dollar(.x, accuracy = 0.01)))