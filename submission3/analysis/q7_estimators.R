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


# Covariate matrix --------------------------------------------------------
q7_data <- bid_comparisons %>%
  dplyr::select(ffs_q1, ffs_q2, ffs_q3) # leave one out to avoid collinearity



# 1-to-1 matching with inverse variance distance --------------------------
m.nn.var <- Match(Y = bid_comparisons$bid,
                  Tr = bid_comparisons$treated,
                  X = q7_data,
                  M = 1,
                  Weight = 1,
                  estimand = "ATE",
                  ties = TRUE)

ate_inv <- m.nn.var$est



# 1-to-1 matching with Mahalanobis distance -------------------------------
m.nn.md <- Match(Y = bid_comparisons$bid,
                 Tr = bid_comparisons$treated,
                 X = q7_data,
                 M = 1,
                 Weight = 2,
                 estimand = "ATE",
                 ties = TRUE)

ate_md <- m.nn.md$est



# Inverse propensity scoring ----------------------------------------------
logit.model <- glm(treated ~ ffs_q1 + ffs_q2 + ffs_q3,
                   family = binomial,
                   data = bid_comparisons)

bid_comparisons$ps <- fitted(logit.model)

bid_comparisons <- bid_comparisons %>%
  mutate(ipw = ifelse(treated == 1, 1/ps, 1/(1-ps)))

ipw.model <- lm(bid ~ treated, data = bid_comparisons, weights = bid_comparisons$ipw)
ate_ipw <- coef(ipw.model)["treated"]



# Simple linear regression ------------------------------------------------
ols.model <- lm(bid ~ treated + ffs_q1 + ffs_q2 + ffs_q3 + int_q1 + int_q2 + int_q3, data = bid_comparisons)
ate_ols <- coef(ols.model)["treated"]


# Extract estimates and SEs -----------------------------------------------
estimates <- c(ate_inv, ate_md, ate_ipw, ate_ols)
ses <- c(as.numeric(m.nn.var$se), as.numeric(m.nn.md$se), 
         as.numeric(sqrt(diag(vcov(ipw.model)))["treated"]), 
         as.numeric(sqrt(diag(vcov(ols.model)))["treated"]))


# Comparison table --------------------------------------------------------
treatment_comparison <- data.frame(
  Estimator = c("NN Matching (Inv. Var.)", " ", "NN Matching (Mahalanobis)", " ",
                "IPW Weighting", " ", "OLS with Interactions", " "),
  ATE = c(scales::dollar(estimates[1]), paste0("(", round(ses[1], 2), ")"),
          scales::dollar(estimates[2]), paste0("(", round(ses[2], 2), ")"),
          scales::dollar(estimates[3]), paste0("(", round(ses[3], 2), ")"),
          scales::dollar(estimates[4]), paste0("(", round(ses[4], 2), ")"))
)