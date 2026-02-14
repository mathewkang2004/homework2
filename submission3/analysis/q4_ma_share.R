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


# Calculate the MA share per year -----------------------------------------
ma_percent <- final_ma %>%
  group_by(year) %>%
  summarize(
    total_enrolled = sum(avg_enrolled, na.rm = TRUE),
    total_eligibles = sum(avg_eligibles, na.rm = TRUE)
  ) %>%
  mutate(ma_share = (total_enrolled/total_eligibles) * 100)


# Plot trend --------------------------------------------------------------
ggplot(ma_percent, aes(x = year, y = ma_share)) +
  geom_line(color = "darkblue", size = 1.2) +
  geom_point(size = 3) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(title = "Medicare Advantage Popularity Over Time",
       x = "Year", y = "MA Share of Total Eligibles") +
  theme_minimal(base_family = "sans")
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(size = 11)
  )

# Export the plot -----------------------------------------------------
ggsave("data/output/q4_ma_share.png",
       width = 8,
       height = 6,
       dpi = 300)
