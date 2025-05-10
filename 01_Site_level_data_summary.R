library(ggridges)
library(ggplot2)
library(tidyr)
library(rstatix)
library(ggsignif)
library(patchwork)
library(ggtext)
load("Github/ICRA/data/SITE_DEN.RData")

filtered_density<-read.csv("data/south_only_2025_ICRA_density_TUT.csv")

# set colors
vir_colors <- viridis(n = 4, option = "C")
print(vir_colors)

custom_colors <- vir_colors
custom_colors[4] <- "gold"  # DAA520 goldenrod 

#change

#how many corals were counted density 
summary_by_year_and_total_all <- SITE_DEN %>%
  group_by(YEAR) %>%
  summarise(
    non_na_count = sum(!is.na(DENSITY)),
    na_count = sum(is.na(DENSITY)),
    zeros = sum(DENSITY == 0),
    .groups = "drop"  
  ) 

#YEAR  non_na_count na_count zeros
#1 2015            57        0    42
#2 2018            17        0    15
#3 2023            41        0    29
#4 2025            63        0    30
#how many corals were counted density in south only
summary_by_year_and_total <- filtered_density %>%
  group_by(YEAR) %>%
  summarise(
    non_na_count = sum(!is.na(DENSITY)),
    na_count = sum(is.na(DENSITY)),
    zeros = sum(DENSITY == 0),
    .groups = "drop"  
  ) 
#YEAR non_na_count na_count zeros
#1  2015           35        0    23
#2  2018           10        0     8
#3  2023           29        0    18
#4  2025           42        0    11

#Exploring density data by survey method including at absent areas
#test if size distribution is normal
shapiro.test(SITE_DEN$DENSITY)
#Non-normal, W = 0.31699, p-value < 2.2e-16
shapiro.test(filtered_density$DENSITY)
#W = 0.40459, p-value < 2.2e-16

#run Kruskal test 
kw_results <- SITE_DEN %>%
  kruskal_test(DENSITY ~ YEAR)
#DENSITY   178      9.94     3 0.0191 Kruskal-Wallis

kw_results_filtered <- filtered_density %>%
  kruskal_test(DENSITY ~ YEAR)
#DENSITY   116      9.63     3 0.022 Kruskal-Wallis

dunn_results <- SITE_DEN %>%
  dunn_test(DENSITY ~ YEAR, p.adjust.method = "bonferroni")

dunn_results_filtered <- filtered_density %>%
  dunn_test(DENSITY ~ YEAR, p.adjust.method = "bonferroni")

# Log-transform ICRA densities (log(x + 1))
filtered_density <- filtered_density %>%
  mutate(Log_ICRA_density = log1p(DENSITY))%>%  # log1p(x) is equivalent to log(x + 1)
  mutate(Sqrt_ICRA_density = sqrt(DENSITY)) #sruare root transform as it handles zeros better


# **Exploring transformed density data by survey method**
# Test if log-transformed data is normal
shapiro.test(filtered_density$DENSITY)
shapiro.test(filtered_density$Log_ICRA_density)
shapiro.test(filtered_density$Sqrt_ICRA_density)
#none of the data are normal after transformation

#visualize distributions 

# Create density plots
p1 <- ggplot(filtered_density, aes(x = DENSITY)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Original Data", x = "ICRA_raw_density")

p2 <- ggplot(filtered_density, aes(x = Log_ICRA_density)) +
  geom_density(fill = "red", alpha = 0.5) +
  labs(title = "Log-Transformed", x = "Log_ICRA_density")

p3 <- ggplot(filtered_density, aes(x = Sqrt_ICRA_density)) +
  geom_density(fill = "green", alpha = 0.5) +
  labs(title = "Square Root Transformed", x = "Sqrt_ICRA_density")

# Create Q-Q plots
qq1 <- ggplot(filtered_density, aes(sample = DENSITY)) +
  stat_qq() + stat_qq_line() + labs(title = "QQ Plot: Original Data")

qq2 <- ggplot(filtered_density, aes(sample = Log_ICRA_density)) +
  stat_qq() + stat_qq_line() + labs(title = "QQ Plot: Log-Transformed")

qq3 <- ggplot(filtered_density, aes(sample = Sqrt_ICRA_density)) +
  stat_qq() + stat_qq_line() + labs(title = "QQ Plot: Square Root Transformed")

# Arrange plots in a grid
(p1 | p2 | p3) / (qq1 | qq2 | qq3)
ggsave("south only density data transformation distributions.png")

# Ridge plot of density
ggplot(filtered_density, aes(x = Sqrt_ICRA_density, y = as.factor(YEAR), fill = as.factor(YEAR))) +
  geom_density_ridges(alpha = 0.9) +
  labs(x = "Count (Sqrt transformed)", y = "Year", title = "Density Distribution of Counts by Year") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = custom_colors)

# Reshape the data to long format for column plot, and log transform do deal with zeros
mean_sd_den_per_year_site <- filtered_density %>%
  group_by(YEAR) %>%
  summarise(
    mean_den = mean(Sqrt_ICRA_density, na.rm = TRUE),
    sd_density = sd(Sqrt_ICRA_density, na.rm = TRUE),
    .groups = "drop"
  )

# Column plot
ggplot(mean_sd_den_per_year_site, aes(x = as.factor(YEAR), y = mean_den, fill=as.factor(YEAR))) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_errorbar(aes(ymin = mean_den - sd_density, ymax = mean_den + sd_density), 
                width = 0.25) +
  labs(x = "Year", y = "Mean Density ± SD", title = "Average Density per Year with SD") +
  theme_minimal()+
  scale_fill_manual(values = custom_colors)

# Should bootstrap this data
library(dplyr)
library(boot)

# Define a bootstrap function.to estimate uncertainty for mean density per year, based on resampling


boot_mean <- function(data, indices) {
  d <- data[indices, ] # Resample rows with replacement using the boot package
  tapply(d$DENSITY, d$YEAR, mean, na.rm = TRUE) # Compute mean for each year
}

set.seed(123)
boot_out <- boot(filtered_density, statistic = boot_mean, R = 1000)

# Convert to dataframe with CI
boot_means <- data.frame(
  YEAR = sort(unique(filtered_density$YEAR)),
  mean = colMeans(boot_out$t),
  lower = apply(boot_out$t, 2, quantile, 0.025),
  upper = apply(boot_out$t, 2, quantile, 0.975)
)

# Plot
ggplot(boot_means, aes(x = as.factor(YEAR), y = mean, fill = as.factor(YEAR))) +
  geom_col() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  labs(x = "Year", y = "Mean Density (bootstrapped CI)") +
  theme_minimal()+
  scale_fill_manual(values = custom_colors)


#with log transformed data
boot_mean_log <- function(data, indices) {
  d <- data[indices, ]
  d$log_density <- log1p(d$DENSITY)
  tapply(d$log_density, d$YEAR, mean, na.rm = TRUE)
}

set.seed(123)
boot_log_out <- boot(filtered_density, statistic = boot_mean_log, R = 1000)

# Back-transform (expm1 reverses log1p)
boot_means_log <- data.frame(
  YEAR = sort(unique(filtered_density$YEAR)),
  mean = expm1(colMeans(boot_log_out$t)),
  lower = expm1(apply(boot_log_out$t, 2, quantile, 0.025)),
  upper = expm1(apply(boot_log_out$t, 2, quantile, 0.975))
)

ggplot(boot_means_log, aes(x = as.factor(YEAR), y = mean, fill = as.factor(YEAR))) +
  geom_col() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  labs(x = "Year", y = "Log (Mean Density) (colonies per m²)", fill="Year") +
  theme_minimal()+
  theme(
    panel.grid = element_blank(), 
    #panel.border = element_rect(color = "black", size = 1),  
    axis.text.x = element_text(size = 16),  
    axis.text.y = element_text(size = 16),  
   axis.title = element_text(size = 18),  
    text = element_text(size = 14),  
    plot.title = element_text(hjust = 0.5),  
    legend.position = "none",  
    legend.key.size = unit(0.6, "cm"),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 16)
  ) +
  scale_fill_manual(values = custom_colors)
#need to manually add significance from the Kruskal test above

ggsave("plots/south only density data barplot bootstrapped2.png", width = 4, height = 6, dpi = 300)
