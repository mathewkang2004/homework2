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

hhi_data <- final_ma %>%
  # Sum enrollment by parent org per county
  group_by(fips, year, parent_org) %>%
  summarize(org_enrollment = sum(avg_enrollment, na.rm = TRUE), .groups = "drop_last") %>%
  # MA share (%): dividing org enrollment by total county enrollment
  mutate(county_enrollment = sum(org_enrollment, na.rm = TRUE),
         market_share = (org_enrollment/county_enrollment) * 100) %>%
  # Competition: HHI = sum of squared MA shares
  summarize(hhi_ma = sum(market_share^2, na.rm = TRUE), .groups = "drop")


# Timeline of average HHI for all counties per year -----------------------
hhi_timeline <- hhi_data %>%
  group_by(year) %>%
  summarize(avg_hhi = mean(hhi_ma, na.rm = TRUE))

# Plot the data -----------------------------------------------------------
ggplot(hhi_timeline, aes(x = year, y = avg_hhi)) +
  geom_line(color = "darkblue", size = 1) +
  geom_point(size = 3) +
  labs(title = "Average Market Concentration (HHI) Over Time",
       subtitle = "Medicare Advantage 2014-2019",
       x = "Year", y = "Average HHI") +
  theme_minimal(base_family = "sans") +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(size = 11)
  )

# Export the plot -----------------------------------------------------
ggsave("data/output/q3_hhi.png",
       width = 8,
       height = 6,
       dpi = 300)
