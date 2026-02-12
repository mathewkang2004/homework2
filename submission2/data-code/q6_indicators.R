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
  group_by(fips, parent_org) %>%
  summarize(org_enroll = sum(avg_enrollment, na.rm = TRUE), .groups = "drop_last") %>%
  mutate(
    county_enrollment = sum(org_enroll, na.rm = TRUE),
    ma_share = (org_enroll/county_enrollment) * 100) %>%
  summarize(hhi = sum(ma_share^2, na.rm = TRUE), .groups = "drop")


# Merge HHI with the rest of the data -------------------------------------
final_2018 <- final_ma %>%
  filter(year == 2018) %>%
  left_join(hhi_2018, by = "fips") %>%
  filter(!is.na(bid))

# Quantiles of HHI --------------------------------------------------------
q_hhi <- quantile(hhi_2018$hhi, probs = c(0.33, 0.66), na.rm = TRUE)


# Define treatment: high (33rd) vs low HHI (66th) -------------------------
final_2018 <- final_2018 %>%
  mutate(market_type = case_when(
    hhi <= q_hhi[1] ~ "Competitive (Low HHI)",
    hhi >= q_hhi[2] ~ "Uncompetitive (High HHI)",
    TRUE ~ "Middle"
  )) %>%
  filter(market_type != "Middle")


# Create indicators based on FFS cost quartiles ---------------------------
final_2018 <- final_2018 %>%
  mutate(ffs_quartile = ntile(avg_ffscost, 4)) %>%
  mutate(
    ffs_q1 = ifelse(ffs_quartile == 1, 1, 0),
    ffs_q2 = ifelse(ffs_quartile == 2, 1, 0),
    ffs_q3 = ifelse(ffs_quartile == 3, 1, 0),
    ffs_q4 = ifelse(ffs_quartile == 4, 1, 0)
  )


# Write csv for question 7 ------------------------------------------------
write_csv(final_2018,"data/output/final_2018.csv")



# Create summary table -------------------------------------------------------------------
q6_summary_table <- final_2018 %>%
  filter(!is.na(ffs_quartile)) %>%
  group_by(ffs_quartile, market_type) %>%
  summarize(mean_bid = mean(bid, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = market_type, values_from = mean_bid) %>%
  mutate(across(c(`Competitive (Low HHI)`, `Uncompetitive (High HHI)`), 
                ~ scales::dollar(.x, accuracy = 0.01)))


# Display table -----------------------------------------------------------
knitr::kable(q6_summary_table, 
             caption = "Average Bids by FFS Cost Quartile and Competition Level (2018)",
             col.names = c("FFS Quartile", "Competitive (Low HHI)", "Uncompetitive (High HHI)"))

