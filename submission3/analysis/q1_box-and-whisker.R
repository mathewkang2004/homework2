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


# Summarize for plot ------------------------------------------------------
plot_data_q1 <- final_ma %>%
  group_by(fips, year) %>%
  summarize(plan_count = n_distinct(planid), .groups = "drop")


# Create box-and-whisker plot -------------------------------------------------------------
ggplot(plot_data_q1, aes(x = as.factor(year), y = plan_count)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Plan Counts per County (2014-2019)",
       x = "Year", y = "Number of Plans") +
  theme_minimal()


# Export the box plot -----------------------------------------------------
ggsave("data/output/q1_boxplot.png",
       width = 8,
       height = 6,
       dpi = 300)


