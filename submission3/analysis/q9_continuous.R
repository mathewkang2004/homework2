# Install and load tools --------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata, scales, data.table, Matching, WeightIt)


# Import data -------------------------------------------------------------
hhi_2018 <- read_csv("data/output/hhi_2018.csv", 
                     col_types = cols(
                       fips = col_double(),
                       hhi = col_double(),
                       avg_ffscost = col_double(),
                       bid = col_double(),
                       avg_eligibles = col_double()
                     ))

source("submission3/analysis/q6_quartiles.R")


# De-mean interactions for OLS ATE ----------------------------------------
bid_comparisons <- bid_comparisons %>%
  mutate(
    int_q1 = treated * (ffs_q1 - mean(ffs_q1, na.rm = TRUE)),
    int_q2 = treated * (ffs_q2 - mean(ffs_q2, na.rm = TRUE)),
    int_q3 = treated * (ffs_q3 - mean(ffs_q3, na.rm = TRUE))
  )


# Simple linear regression ------------------------------------------------
ols.model <- lm(bid ~ treated + ffs_q1 + ffs_q2 + ffs_q3 + int_q1 + int_q2 + int_q3, data = bid_comparisons)
ate_ols <- coef(ols.model)["treated"]

# Re-estimate using continuous covariates ---------------------
q9.model <- lm(bid ~ treated + avg_ffscost + avg_eligibles, data = bid_comparisons)


# Extract ATE and standard error -------------------------------------------------------------
ate_q7_ols <- coef(ols.model)["treated"]
se_q7_ols  <- as.numeric(sqrt(diag(vcov(ols.model)))["treated"])

ate_q9_ols <- coef(q9.model)["treated"]
se_q9_ols  <- as.numeric(sqrt(diag(vcov(q9.model)))["treated"])

# Compare results
ols_comparison <- data.frame(
  Model = c("OLS Baseline", " ", "OLS Alternate", " "),
  ATE = c(scales::dollar(ate_q7_ols), paste0("(", round(se_q7_ols, 2), ")"),
          scales::dollar(ate_q9_ols), paste0("(", round(se_q9_ols, 2), ")"))
)