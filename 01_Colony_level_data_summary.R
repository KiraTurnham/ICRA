library(tidyverse)
library(hrbrthemes)
library(viridis)
library(ggridges)
library(readxl)

load("data/COLONY_SIZE_PM.RData")
SOUTH_COLONY_SIZE_PM<-read.csv("data/south_only_ICRA_Colony_level_data.csv")

#Data summaries

#how many corals were sized (this excludes Feb 20205 data where size wasn't taken)
summary_by_year_and_total <- COLONY_SIZE_PM %>%
  group_by(Data_Source, YEAR) %>%
  summarise(
    non_na_count = sum(!is.na(COLONYLENGTH)),
    na_count = sum(is.na(COLONYLENGTH)),
    .groups = "drop"  
  ) 

#how many corals were sized in south sites only (this excludes Feb 20205 data where size wasn't taken)
summary_by_year_and_total_SOUTH <- SOUTH_COLONY_SIZE_PM %>%
  group_by(Data_Source, YEAR) %>%
  summarise(
    non_na_count = sum(!is.na(COLONYLENGTH)),
    na_count = sum(is.na(COLONYLENGTH)),
    .groups = "drop"  
  ) 
#1 esa          2025          617      569
#2 ncrmp2       2015           53        0
#3 ncrmp2       2018           44        0
#4 ncrmp2       2023          179        0

#how many corals were measured PM by year and size class: NA = feb 2025 data
summary_by_year_and_totalPM <- COLONY_SIZE_PM %>%
  group_by(Data_Source, YEAR, TAIL_BINS) %>%
  summarise(non_na_count = sum(!is.na(PER_DEAD)),
            na_count = sum(is.na(PER_DEAD)) )

#how many corals were measured PM by year and size class: NA = feb 2025 data
summary_by_year_and_totalPM_south <- SOUTH_COLONY_SIZE_PM %>%
  group_by(Data_Source, YEAR, TAIL_BINS) %>%
  summarise(non_na_count = sum(!is.na(PER_DEAD)),
            na_count = sum(is.na(PER_DEAD)) )

#sample size per bleaching period
COLONY_SIZE_PM %>%
  group_by(Bleaching_Period) %>%
  summarise(non_na_count = sum(!is.na(COLONYLENGTH)),
             na_count = sum(is.na(COLONYLENGTH)) )
#1 Post-Bleaching            626      648
#2 Pre-Bleaching             282        0
#626 colonies were sized  in 2025: (648 were not: only sized colonies in March. All had PM measured.)


# Compute mean size per year
mean_size_per_year <- COLONY_SIZE_PM %>%
  group_by(YEAR) %>%
  summarise(COLONYLENGTH = mean(COLONYLENGTH, na.rm = TRUE))
#1  2015      22.4
#2  2018      26.2
#3  2023      28.0
#4  2025      36.7

#when excluding noth: 
mean_size_per_year_south <- SOUTH_COLONY_SIZE_PM %>%
  group_by(YEAR) %>%
  summarise(COLONYLENGTH = mean(COLONYLENGTH, na.rm = TRUE))
#1  2015      22.4
#2  2018      26.2
#3  2023      28.0
#4  2025      36.7

# mean PM per year
mean_PM_per_year <- COLONY_SIZE_PM %>%
  group_by(YEAR) %>%
  summarise(mean_PM = mean(PER_DEAD, na.rm = TRUE))
#1 2015     7.48
#2 2018     7.20
#3 2023     7.9 
#4 2025     30.2 
# ~ four fold increase

# mean PM per year south only
mean_PM_per_year_south <- SOUTH_COLONY_SIZE_PM %>%
  group_by(YEAR) %>%
  summarise(mean_PM = mean(PER_DEAD, na.rm = TRUE))
#1  2015    7.81
#2  2018    7.20
#3  2023    7.94
#4  2025   30.2


# calc the max size (for plotting later)
max_size <- COLONY_SIZE_PM %>%
  group_by(YEAR) %>%
  summarise(max_size = max(COLONYLENGTH, na.rm = TRUE))
#YEAR  max_size
#1 2015        73
#2 2018        70
#3 2023       116
#4 2025       170

#calculate summary stats of PM by year

summary_stats <- COLONY_SIZE_PM %>%
  group_by(YEAR)%>%
  summarise(
    Mean_PM = mean(PER_DEAD, na.rm = TRUE),
    SD_PM = sd(PER_DEAD, na.rm = TRUE),
    SE_PM = SD_PM / sqrt(n()),
    CI_Lower = Mean_PM - qt(0.975, df = n()-1) * SD_PM / sqrt(n()),
    CI_Upper = Mean_PM + qt(0.975, df = n()-1) * SD_PM / sqrt(n()),
    N = n(), 
    .groups = "drop"
  )
#calculate summary stats of PM by size and year
summary_stats_size <- COLONY_SIZE_PM %>%
  group_by(YEAR, TAIL_BINS)%>%
  summarise(
    Mean_PM = mean(PER_DEAD, na.rm = TRUE),
    SD_PM = sd(PER_DEAD, na.rm = TRUE),
    SE_PM = SD_PM / sqrt(n()),
    CI_Lower = Mean_PM - qt(0.975, df = n()-1) * SD_PM / sqrt(n()),
    CI_Upper = Mean_PM + qt(0.975, df = n()-1) * SD_PM / sqrt(n()),
    N = n(), 
    .groups = "drop"
  )
#write.csv(summary_stats, "summary_stats_PM_by_size_Year.csv", row.names = FALSE)


#double check site numbers for PM data (remove NA, but keep zeros)
COLONY_SIZE_PM %>%
  group_by(YEAR) %>%
  filter(!is.na(PER_DEAD))%>%
  summarize(n_unique_sites = n_distinct(paste(LATITUDE, LONGITUDE)))
#YEAR  n_unique_sites
#1 2015              15
#2 2018               2 #drop 2018, as only 2 sites
#3 2023              12
#4 2025              33 #should weight and bootstrap 2025 data to 12 sites

#correct order
summary_stats_size$TAIL_BINS <- factor(summary_stats_size$TAIL_BINS, 
                                  levels = c("Q20", "QMED", "Q80"))
#prepare facet labels for plotting
facet_labels <- c(
  "Q20" = "Small (5-12 cm)",
  "QMED" = "Medium (13-39 cm)",
  "Q80" = "Large (>40 cm)"
)


###################################
#####ridgeplot of PM by year#######
###################################
ggplot(COLONY_SIZE_PM, aes(x = PER_DEAD, y = as.factor(YEAR), fill = as.factor(YEAR))) +
  geom_density_ridges(alpha = 0.8) +  # Ridge plot
  geom_point(data = mean_PM_per_year, aes(x = (mean_PM), y = as.factor(YEAR)), #can change to log(mean_PM)
             color = "black", size = 3, shape = 16) +  # Means are points
  #stat_density_ridges(quantile_lines = TRUE, quantiles = 3, alpha = 0.5, color= "black", linewidth = 0.5) +  # Quantiles
  geom_text(data = n_per_year, 
            aes(x = max_size + (max_size * 0.05),  
                y = as.factor(YEAR), 
                label = paste0("N=", N)),  
            hjust = 0, vjust = -1, size = 3, color = "black") +  
  labs(
    x = "Percent partial mortality)",
    y = "Year",
    fill="Year") +
  scale_fill_viridis_d(option = "C") +  
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 12, family = "Helvetica"),
        axis.text.y = element_text(size = 12, family = "Helvetica"),
        axis.title = element_text(size = 14, family = "Helvetica"),
        plot.title = element_text(size = 16, family = "Helvetica", face = "bold"))

ggplot2::ggsave ("plots/Partial_mortality_ridge.jpeg", width = 5, height = 5, units = 'in')

###################################
###ridgeplot of size by year#######
###################################

#calculate proportions of each class per year (do not have density of PM corals)
size_props <- COLONY_SIZE_PM %>%
  group_by(YEAR, TAIL_BINS) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(YEAR) %>%
  mutate(prop = n / sum(n)) %>%
  select(YEAR, TAIL_BINS, prop) %>%
  pivot_wider(names_from = TAIL_BINS, values_from = prop) %>%
  rename(prop_q20 = Q20, prop_qmed = QMED, prop_q80 = Q80)

q20_cutoff <- 12
q80_cutoff <- 40

ggplot(COLONY_SIZE_PM, aes(x = COLONYLENGTH, y = as.factor(YEAR), fill = as.factor(YEAR))) +
  geom_density_ridges(alpha = 0.8) +
  
  # Vertical lines for Q20 and Q80 cutoffs
  geom_vline(xintercept = q20_cutoff, linetype = "dashed", color = "firebrick", linewidth = 0.7) +
  geom_vline(xintercept = q80_cutoff, linetype = "dashed", color = "blue", linewidth = 0.7) +
  
  # Proportion labels above each ridge
  geom_text(COLONY_SIZE_PM = size_props,
            aes(x = q20_cutoff - 5,  # slightly left of Q20 line
                y = as.factor(YEAR),
                label = paste0(round(prop_q20 * 100), "%")),
            color = "firebrick", size = 2, vjust = -6, inherit.aes = FALSE) +
  
  geom_text(data = size_props,
            aes(x = (q20_cutoff + q80_cutoff) / 2,  # center for QMED
                y = as.factor(YEAR),
                label = paste0(round(prop_qmed * 100), "%")),
            color = "black", size = 2, vjust = -6, inherit.aes = FALSE) +
  
  geom_text(data = size_props,
            aes(x = q80_cutoff + 5,  # slightly right of Q80 line
                y = as.factor(YEAR),
                label = paste0(round(prop_q80 * 100), "%")),
            color = "blue", size = 2, vjust = -6, inherit.aes = FALSE) +
  
  labs(x = "Coral size (cm)", y = "Year", fill="Year") +
  scale_fill_viridis_d(option = "C") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 16, face = "bold"))

ggplot2::ggsave ("plots/Colony_size_ridge.jpeg", width = 5, height = 5, units = 'in')


########################
#boxplot of PM by year##
########################

ggplot(summary_stats,
       aes(x = as.factor(YEAR), y = Mean_PM, fill = as.factor(YEAR))) +
  geom_col(alpha = 1) +  
  geom_errorbar(aes(
    ymin = pmax(0, CI_Lower),  
    ymax = CI_Upper
  ), width = 0.2) +theme_minimal() +
  theme(
    panel.border = element_rect(color = "grey", fill = NA, size = 1)
  ) +
  labs(
    x = "Survey Year",
    y = "Mean percent partial mortality (%)",
    fill="Year"
  ) +
  scale_fill_viridis_d(option = "C") + 
  theme_minimal() +
  theme(
    legend.position = "none", 
    axis.text.x = element_text(size = 8, family = "Helvetica", angle = 45, hjust = 0.7),
    axis.text.y = element_text(size = 10, family = "Helvetica"),
    axis.title = element_text(size = 12, family = "Helvetica"),
    panel.border = element_rect(color = "grey", fill = NA, size = 1)
  )
ggplot2::ggsave ("plots/Partial_mortaltiy_boxplot_by_year.jpeg", width = 5, height = 5, units = 'in')


######################################
#boxplot of PM by size class and year#
######################################
ggplot(summary_stats_size %>% filter(!is.na(TAIL_BINS)), 
       aes(x = as.factor(YEAR), y = Mean_PM, fill = as.factor(YEAR))) +
  geom_col(alpha = 1) +  
  geom_errorbar(aes(
    ymin = pmax(0, CI_Lower),  
    ymax = CI_Upper
  ), width = 0.2) +theme_minimal() +
  theme(
    panel.border = element_rect(color = "grey", fill = NA, size = 1)
  ) +
  labs(
    x = "Survey Year",
    y = "Mean percent partial mortality (%)",
    fill="Year"
  ) +
  facet_wrap(~TAIL_BINS, labeller = labeller(TAIL_BINS = facet_labels)) +  
  scale_fill_viridis_d(option = "C") + 
  theme_minimal() +
  theme(
    legend.position = "none", 
    axis.text.x = element_text(size = 8, family = "Helvetica", angle = 45, hjust = 0.7),
    axis.text.y = element_text(size = 10, family = "Helvetica"),
    axis.title = element_text(size = 12, family = "Helvetica"),
    panel.border = element_rect(color = "grey", fill = NA, size = 1)
  )
ggplot2::ggsave ("plots/Partial_mortaltiy_boxplot_by_size_class.jpeg", width = 5, height = 5, units = 'in')

###############################
#plot histograms of PM by year#
###############################
COLONY_SIZE_PM %>%
  filter(YEAR %in% c(2015, 2023, 2018, 2025)) %>%
  ggplot(aes(x = PER_DEAD, fill = factor(YEAR))) +
  geom_histogram(bins = 20, alpha = 0.6, position = "identity") +
  facet_wrap(~ YEAR, scales = "free_y") +
  theme_minimal() +
  labs(
    x = "Partial Mortality (%)",
    y = "Frequency",
    title = "Distribution of Partial Mortality (2015 vs 2023)",
    fill="Year"
  ) +
  scale_fill_viridis_d(option = "C") + 
  theme(
    axis.text.x = element_text(size = 8, family = "Helvetica", angle = 45, hjust = 0.7),
    axis.text.y = element_text(size = 10, family = "Helvetica"),
    axis.title = element_text(size = 12, family = "Helvetica"),
    text = element_text(size = 12),
    plot.title = element_text(hjust = 0.5)
  )


#############################################
# Boxplot to visualize outliers of size data#
#############################################
#test removing sizes in 2025 that are bigger than previous records
max_pre2025 <- COLONY_SIZE_PM %>%
  filter(YEAR < 2025) %>%
  summarise(max_size = max(COLONYLENGTH, na.rm = TRUE)) %>%
  pull(max_size)

filtered_data_max <- COLONY_SIZE_PM %>%
  filter(COLONYLENGTH <= max_pre2025)

#save filtered data 

#visuzlize both by renaming first line :
filtered_data_max %>%
  filter(YEAR %in% c(2015, 2023, 2018, 2025)) %>%
  ggplot(aes(x = factor(YEAR), y = COLONYLENGTH, fill = factor(YEAR))) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    x = "Year",
    y = "Colony lenth (cm)"
  ) +
  scale_fill_viridis_d(option = "C") + 
  theme(
    axis.text.x = element_text(size = 8, family = "Helvetica", angle = 45, hjust = 0.7),
    axis.text.y = element_text(size = 10, family = "Helvetica"),
    axis.title = element_text(size = 12, family = "Helvetica"),
    text = element_text(size = 12),
    plot.title = element_text(hjust = 0.5)
  ) 

#plot PM of filtered max data
filtered_data_max %>%
  filter(YEAR %in% c(2015, 2023, 2018, 2025)) %>%
  ggplot(aes(x = factor(YEAR), y = PER_DEAD, fill = factor(YEAR))) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    x = "Year",
    y = "Partial mortality (%)"
  ) +
  scale_fill_viridis_d(option = "C") + 
  theme(
    axis.text.x = element_text(size = 8, family = "Helvetica", angle = 45, hjust = 0.7),
    axis.text.y = element_text(size = 10, family = "Helvetica"),
    axis.title = element_text(size = 12, family = "Helvetica"),
    text = element_text(size = 12),
    plot.title = element_text(hjust = 0.5)
  ) 


##################################
#Plot dot + CI of PM across years#
##################################
ggplot(summary_stats, aes(x = factor(YEAR), y = Mean_PM, color = factor(YEAR))) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2) +
  theme_minimal() +
  labs(
    x = "Year",
    y = "Mean Partial Mortality (%)",
    color= "Year"
  ) +
  scale_color_viridis_d(option = "C") +
  theme(
    axis.text.x = element_text(size = 8, family = "Helvetica", angle = 45, hjust = 0.7),
    axis.text.y = element_text(size = 10, family = "Helvetica"),
    axis.title = element_text(size = 12, family = "Helvetica"),
    text = element_text(size = 12),
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_y_continuous(
    limits = c(0, 35),  # Set y-axis to start from 0
    breaks = seq(0, 30, by = 10)  # Adjust the break interval
  )
ggplot2::ggsave ("plots/Partial_mortaltiy_dotplot_CI.jpeg", width = 5, height = 5, units = 'in')


#dot plot by size class
ggplot(summary_stats_size %>% filter(!is.na(TAIL_BINS)), 
       aes(x = as.factor(YEAR), y = Mean_PM, fill = as.factor(YEAR))) +
  geom_point(shape = 21, size = 3, color = "black")+
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper, color = as.factor(YEAR)), width = 0.2 ) +
  theme(
    panel.border = element_rect(color = "grey", fill = NA, size = 1) # Adds black border around each facet
  ) +
  labs(
    x = "Survey Year",
    y = "Mean percent partial mortality (%)"
  ) +
  facet_wrap(~TAIL_BINS, labeller = labeller(TAIL_BINS = facet_labels)) +  
  scale_fill_viridis_d(option = "C") +
  scale_color_viridis_d(option = "C") +
  theme_minimal() +
  theme(
    legend.position = "none", 
    axis.text.x = element_text(size = 12, family = "Helvetica"),
    axis.text.y = element_text(size = 12, family = "Helvetica"),
    axis.title = element_text(size = 14, family = "Helvetica"),
    plot.title = element_text(size = 16, family = "Helvetica", face = "bold"),
    panel.border = element_rect(color = "grey", fill = NA, size = 1)
    # Adds black border around each facet
  )
ggplot2::ggsave ("plots/Partial_mortaltiy_by_size_dotplot_CI.jpeg", width = 5, height = 5, units = 'in')



