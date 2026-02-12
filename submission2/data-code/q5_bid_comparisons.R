# Install and load tools --------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata, scales, data.table)


# Import data -------------------------------------------------------------
final_ma <- read_csv("data/output/final_ma.csv", 
                     col_types = cols(
                       fips = col_double(),
                       year = col_double(),
                       planid = col_double(),
                       bid = col_double()
                     ))


# Filter for 2018 and calculate HHI ---------------------------------------
hhi_2018 <- final_ma %>%
  filter(year == 2018) %>%
  group_by(fips) %>%
  mutate(
    county_enrollment = sum(avg_enrollment, na.rm = TRUE),
    ma_share = (avg_enrollment/county_enrollment) * 100) %>%
  summarize(hhi = sum(ma_share^2),
            avg_bid = mean(bid, na.rm = TRUE)
  )


# Quantiles of HHI --------------------------------------------------------
q_hhi <- quantile(hhi_2018$hhi, probs = c(0.33, 0.66), na.rm = TRUE)


# Define treatment: high (33rd) vs low HHI (66th) -------------------------
bid_comparisons <- hhi_2018 %>%
  mutate(market_type = case_when(
    hhi <= q_hhi[1] ~ "Competitive (Low HHI)",
    hhi >= q_hhi[2] ~ "Uncompetitive (High HHI)",
    TRUE ~ "Middle"
  )) %>%
  filter(market_type != "Middle") %>%
  group_by(market_type) %>%
  summarize(mean_bid = mean(avg_bid, na.rm = TRUE))

# Print average bid among competitive vs. uncompetitive markets
print(bid_comparisons)
