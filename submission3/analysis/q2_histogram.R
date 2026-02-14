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

# Filter data for 2014 and 2018 -------------------------------------------
plot_data_q2 <- final_ma %>%
  filter(year %in% c(2014, 2018)) %>%
  filter(!is.na(bid))


# Create frequency histograms side-by-side --------------------------------
ggplot(plot_data_q2, aes(x = bid, fill = as.factor(year))) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 50, color = "white") +
  scale_fill_manual(values = c("2014" = "darkblue", "2018" = "darkorange")) +
  labs(
    title = "Distribution of Medicare Advantage Bids",
    x = "Bid Amount ($)",
    y = "Frequency (Count of Plans)",
    fill = "Year"
  ) +
  theme_minimal(base_family = "sans")

# Export the plot -----------------------------------------------------
ggsave("data/output/q2_histogram.png",
       width = 8,
       height = 6,
       dpi = 300)
