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
    ma_share = if_else(avg_enrolled > 0, (avg_enrollment / avg_enrolled) * 100, NA_real_)
  ) %>%
  summarize(
    hhi = sum(ma_share^2, na.rm = TRUE),
    bid = mean(bid, na.rm = TRUE),
    avg_ffscost = mean(avg_ffscost, na.rm = TRUE),
    avg_eligibles = mean(avg_eligibles, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  filter(!is.na(bid), !is.na(avg_ffscost), !is.na(avg_eligibles))


# Save hhi_2018 for future questions --------------------------------------
write_csv(hhi_2018,"data/output/hhi_2018.csv")


# Quantiles of HHI --------------------------------------------------------
q_hhi <- quantile(hhi_2018$hhi, probs = c(0.33, 0.66), na.rm = TRUE)



# Define treatment: high (33rd) vs low HHI (66th) -------------------------
bid_comparisons <- hhi_2018 %>%
  mutate(
    hhi_group = case_when(
      hhi >= q_hhi[2] ~ "Uncompetitive (High HHI)", # treated
      hhi <= q_hhi[1] ~ "Competitive (Low HHI)", # control
      TRUE ~ NA_character_
    ),
    treated = case_when(
      hhi_group == "Uncompetitive (High HHI)" ~ 1L,
      hhi_group == "Competitive (Low HHI)" ~ 0L,
      TRUE ~ NA_integer_
    )) %>%
  filter(!is.na(hhi_group))


# Calculate means by group ----------------------------------------------
by_group <- bid_comparisons %>%
  group_by(hhi_group) %>%
  summarize(
    `Plan Bid` = mean(bid, na.rm = TRUE),
    `FFS Cost` = mean(avg_ffscost, na.rm = TRUE),
    `Medicare Eligibles` = mean(avg_eligibles, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = -hhi_group, names_to = "Variable", values_to = "Value") %>%
  pivot_wider(names_from = hhi_group, values_from = Value)


# Calculate overall means -------------------------------------------------
overall <- bid_comparisons %>%
  summarize(
    `Plan Bid` = mean(bid, na.rm = TRUE),
    `FFS Cost` = mean(avg_ffscost, na.rm = TRUE),
    `Medicare Eligibles` = mean(avg_eligibles, na.rm = TRUE)
  ) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Overall")


# Join together -----------------------------------------------------------
balance_table <- by_group %>%
  left_join(overall, by = "Variable")