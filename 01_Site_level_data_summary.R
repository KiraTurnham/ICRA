library(ggridges)
library(ggplot2)
library(tidyr)
load("Github/ICRA/data/SITE_DEN.RData")

#how many corals were counted density 
summary_by_year_and_total <- SITE_DEN %>%
  group_by(YEAR) %>%
  summarise(
    non_na_count = sum(!is.na(DENSITY)),
    na_count = sum(is.na(DENSITY)),
    zeros = sum(DENSITY == 0),
    .groups = "drop"  
  ) 
#YEAR  non_na_count na_count
#YEAR  non_na_count na_count zeros
#1 2015            57        0    42
#2 2018            17        0    15
#3 2023            41        0    29
#4 2025            63        0    30



# Reshape the data to long format
summary_long <- summary_by_year_and_total %>%
  gather(key = "count_type", value = "count", non_na_count, na_count, zeros)

# Ridge plot
ggplot(SITE_DEN, aes(x = DENSITY, y = YEAR, fill = YEAR)) +
  geom_density_ridges(alpha = 0.7) +
  labs(x = "Count", y = "Year", title = "Density Distribution of Counts by Year") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_fill_viridis_d(option = "C")

# Reshape the data to long format for column plot
mean_sd_den_per_year_site <- SITE_DEN %>%
  group_by(YEAR, SITE, LATITUDE, LONGITUDE) %>%
  summarise(
    mean_den = mean(DENSITY, na.rm = TRUE),
    sd_density = sd(DENSITY, na.rm = TRUE),
    .groups = "drop"
  )



# Column plot
ggplot(mean_sd_den_per_year_site, aes(x = YEAR, y = mean_den, fill=as.factor(YEAR))) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_errorbar(aes(ymin = mean_den - sd_density, ymax = mean_den + sd_density), 
                width = 0.25) +
  labs(x = "Year", y = "Mean Density ± SD", title = "Average Density per Year with SD") +
  theme_minimal()+
  scale_fill_viridis_d(option = "C")

