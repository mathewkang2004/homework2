# Install and load tools --------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata, scales, data.table)


# Import data -------------------------------------------------------------
final_2018 <- read_csv("data/output/final_2018.csv", 
                     col_types = cols(
                       fips = col_double(),
                       year = col_double(),
                       planid = col_double(),
                       bid = col_double()
                     ))


# Clean subset for question 7 ---------------------------------------------
q7_data <- final_2018 %>%
  filter(!is.na(ffs_quartile)) %>%
  mutate(treated = ifelse(market_type == "Competitive (Low HHI)", 1, 0))


# Load Matching package ---------------------------------------------------
if (!require("Matching")) install.packages("Matching")
library("Matching")


# 1-to-1 matching with inverse variance distance --------------------------
m.nn.var <- Match(Y = q7_data$bid,
                  Tr = q7_data$treated,
                  X = q7_data$ffs_quartile,
                  M = 1,
                  Weight = 1,
                  estimand = "ATE",
                  version = "fast",
                  ties = FALSE)

ate_inv <- m.nn.var$est


# 1-to-1 matching with Mahalanobis distance -------------------------------
m.nn.md <- Match(Y = q7_data$bid,
                 Tr = q7_data$treated,
                 X = q7_data$ffs_quartile,
                 M = 1,
                 Weight = 2,
                 estimand = "ATE",
                 version = "fast",
                 ties = FALSE)

ate_md <- m.nn.md$est


# Load WeightIt package ---------------------------------------------------
if (!require("WeightIt")) install.packages("WeightIt")
library(WeightIt)


# Inverse propensity scoring ----------------------------------------------
logit.model <- glm(treated ~ as.factor(ffs_quartile),
                   family = binomial,
                   data = q7_data)

q7_data$ps <- fitted(logit.model)

q7_data <- q7_data %>%
  mutate(ipw = ifelse(treated == 1, 1/ps, 1/(1-ps)))

ipw.model <- lm(bid ~ treated, data = q7_data, weights = ipw)
ate_ipw <- coef(ipw.model)["treated"]


# Simple linear regression ------------------------------------------------
ols_model <- lm(bid ~ treated * as.factor(ffs_quartile), data = q7_data)

# Predict what bids would be if every county was uncompetitive
pred_treated <- predict(ols_model, newdata = mutate(q7_data, treated = 1))

# Predict what bids would be if every county was competitive
pred_control <- predict(ols_model, newdata = mutate(q7_data, treated = 0))

# OLS-based ATE
ate_ols <- mean(pred_treated - pred_control)


# Comparison table -------------------------------------------------------
treat_comparison <- data.frame(
  Estimator = c("NN Matching (Inv. Var.)", "NN Matching (Mahalanobis)", 
                "IPW Weighting", "OLS with Interactions"),
  ATE = c(ate_inv, ate_md, ate_ipw, ate_ols)
)

treatment_comparison %>%
  mutate(ATE = scales::dollar(ATE, accuracy = 0.01)) %>%
  knitr::kable(caption = "Table for Question 7: Comparison of ATE Estimators")


# Question 9: Re-estimate using continuous covariates ---------------------
q9_model <- lm(bid ~ treated + avg_ffscost + avg_eligibles, data = q7_data)


# Extract ATE -------------------------------------------------------------
ate_q9 <- coef(q9_model)["treated"]


# Compare results ---------------------------------------------------------
print(paste("Original ATE (FFS Quartiles):", round(ate_ols, 2)))
print(paste("New ATE (Continuous):", round(ate_q9, 2)))


