rm(list = ls())
library(dplyr)
library(tidyr)
library(ggplot2)
library(broom)
library(purrr)

load("data/COLONY_SIZE_PM.RData")

#options for colors ?
year_colors <- c(
  "2015" = "#4b006c",
  "2018" = "#84186d",
  "2023" = "#d71b33",
  "2025" = "#f2a47c"
)
year_colors <- c(
  "2015" = "#000004",  # near black
  "2018" = "#420a68",  # purple
  "2023" = "#932667",  # red-magenta
  "2025" = "#f1605d"   # red-orange
)

year_colors <- c(
  "2015" = "#6c117b",   # deep violet (not pure black)
  "2018" = "#932667",   # rich purple
  "2023" = "#f87d13",   # magenta-pink
  "2025" = "#f5c527"    # strong orange (warm but soft, readable)
)

#double check numbers for PM data (remove NA, but keep zeros)
COLONY_SIZE_PM %>%
  group_by(YEAR) %>%
  filter(!is.na(PER_DEAD))%>%
  summarize(n_unique_sites = n_distinct(paste(LATITUDE, LONGITUDE)))

# Data preparation
# Remove 2018 due to low sample coverage
dat_sub <- COLONY_SIZE_PM %>%
  filter(YEAR != 2018) %>%
  filter(!is.na(PER_DEAD))%>%  # remove NAs but keep zeros
  mutate(YEAR = factor(YEAR))  


###########################################
#Bootstrap and run glm for PM data by year#
###########################################

# calculate weights for glm 
dat_sub <- dat_sub %>%
  group_by(YEAR) %>%
  mutate(weight = 1 / n()) %>%
  ungroup()

# check: now, 2025 samples will each have smaller weights than 2015 or 2023 samples

# Set parameters
n_boot <- 1000
target_n_sites <- 12  # to match 2023 effort

set.seed(123)  # for reproducibility

# Store results
boot_results <- vector("list", n_boot)

for (i in 1:n_boot) {
  
  # Sample sites for 2025
  sampled_sites_2025 <- dat_sub %>%
    filter(YEAR == 2025) %>%
    distinct(SITE) %>%
    slice_sample(n = target_n_sites) %>%
    pull(SITE)
  
  # Make bootstrapped dataset
  boot_data <- dat_sub %>%
    filter(
      YEAR != 2025 | (YEAR == 2025 & SITE %in% sampled_sites_2025)
    )
  
  # Recalculate weights inside bootstrap (important!)
  boot_data <- boot_data %>%
    group_by(YEAR) %>%
    mutate(weight = 1 / n()) %>%
    ungroup()
  
  # Fit GLM with weights
  model <- glm(PER_DEAD ~ factor(YEAR), 
               data = boot_data, 
               weights = weight,
               family = gaussian())
  
  # Store model summary
  boot_results[[i]] <- tidy(model)
}

# Combine bootstrap results
boot_df <- bind_rows(boot_results, .id = "bootstrap")


# Summarize across bootstraps
boot_summary <- boot_df %>%
  group_by(term) %>%
  summarise(
    estimate_mean = mean(estimate),
    estimate_sd = sd(estimate),
    lower_CI = quantile(estimate, 0.025),
    upper_CI = quantile(estimate, 0.975)
  )

boot_summary


null_model <- glm(PER_DEAD ~ 1, data = boot_data, weights = boot_data$weight, family = gaussian())
anova(model, null_model, test = "Chi")
#year is significant

model <- glm(PER_DEAD ~ factor(YEAR), data = boot_data, weights = weight, family = gaussian())
summary(model)
#Variable	Estimate	Standard Error	t Value	p-Value
#Intercept (2015)	7.4828	1.7000	4.402	< 0.001***
#2023 (vs 2015)	0.4172	2.4042	0.174	0.862
#2025 (vs 2015)	28.6471	2.4042	11.915	< 0.001***

emmeans::emmeans(model, pairwise ~ YEAR)

#YEAR emmean   SE  df lower.CL upper.CL
#2015   7.48 1.58 657     4.39     10.6
#2023   7.90 1.58 657     4.80     11.0
#2025  33.01 1.58 657    29.91     36.1

#Confidence level used: 0.95 

#$contrasts
#contrast            estimate   SE  df t.ratio p.value
#YEAR2015 - YEAR2023   -0.417 2.23 657  -0.187  0.9809
#YEAR2015 - YEAR2025  -25.527 2.23 657 -11.450  <.0001
#YEAR2023 - YEAR2025  -25.109 2.23 657 -11.263  <.0001

###########################################################
#also calc bootstrapped average PM for plots and reporting#
###########################################################
mean_PM_by_year <- map_df(1:n_boot, function(i) {
  
  sampled_sites_2025 <- dat_sub %>%
    filter(YEAR == 2025) %>%
    distinct(SITE) %>%
    slice_sample(n = target_n_sites) %>%
    pull(SITE)
  
  boot_data <- dat_sub %>%
    filter(
      YEAR != 2025 | (YEAR == 2025 & SITE %in% sampled_sites_2025)
    )
  
  boot_data %>%
    group_by(YEAR) %>%
    summarise(mean_PM = mean(PER_DEAD, na.rm = TRUE)) %>%
    mutate(bootstrap = i)
})

# Summarize mean PM
mean_PM_summary <- mean_PM_by_year %>%
  group_by(YEAR) %>%
  summarise(
    mean_PM_mean = mean(mean_PM),
    lower_CI = quantile(mean_PM, 0.025),
    upper_CI = quantile(mean_PM, 0.975)
  )

mean_PM_summary


#########################################
#also run analysuis for PM by size class#
#########################################
boot_mean_PM_by_size <- vector("list", n_boot)

set.seed(123)


boot_mean_PM_by_size <- map_df(1:n_boot, function(i) {
  
  # Get mean PM per year and size class
  summary_i <- boot_data %>%
    filter(!is.na(TAIL_BINS)) %>%
    group_by(YEAR, TAIL_BINS) %>%
    summarise(mean_PM = mean(PER_DEAD, na.rm = TRUE), .groups = "drop") %>%
    mutate(bootstrap = i)
  
  return(summary_i)
})

boot_summary_size <- boot_mean_PM_by_size %>%
  group_by(YEAR, TAIL_BINS) %>%
  summarise(
    mean_PM = mean(mean_PM),
    lower_CI = quantile(mean_PM, 0.025),
    upper_CI = quantile(mean_PM, 0.975),
    .groups = "drop"
  )

# Plot the distribution of partial mortality values for 2015 and 2023
dat_sub %>%
  filter(YEAR %in% c(2015, 2023, 2025)) %>%
  ggplot(aes(x = PER_DEAD, fill = factor(YEAR))) +
  geom_histogram(bins = 20, alpha = 0.6, position = "identity") +
  facet_wrap(~ YEAR, scales = "free_y") +
  theme_minimal() +
  labs(
    x = "Partial Mortality (%)",
    y = "Frequency",
    title = "Distribution of Partial Mortality (2015 vs 2023, bootstrapped data)"
  ) +
  scale_fill_viridis_d(option = "D") +
  theme(
    text = element_text(size = 14),
    plot.title = element_text(hjust = 0.5)
  )
ggplot2::ggsave ("plots/histogram_PM_year_bootstrapped.jpeg", width = 5, height = 2.5, units = 'in')


# Boxplot to visualize outliers in 2015 and 2023
dat_sub %>%
  filter(YEAR %in% c(2015, 2023, 2025)) %>%
  ggplot(aes(x = factor(YEAR), y = PER_DEAD, fill = factor(YEAR))) +
  geom_boxplot() +
  scale_fill_manual(values = year_colors)+
  theme_minimal() +
  labs(
    x = "Year",
    y = "Partial Mortality (%)",
    title = "Boxplot of Partial Mortality by Year (2015 vs 2023, bootstrapped)"
  ) +
  theme(
    text = element_text(size = 14),
    plot.title = element_text(hjust = 0.5)
  )
ggplot2::ggsave ("plots/boxplot_PM_year_bootstrapped.jpeg", width = 5, height = 2.5, units = 'in')

# Boxplot to visualize outliers by size class and year
#correct order
dat_sub$TAIL_BINS <- factor(dat_sub$TAIL_BINS, 
                            levels = c("Q20", "QMED", "Q80"))
dat_sub %>%
  filter(YEAR %in% c(2015, 2023, 2025)) %>% filter(!is.na(TAIL_BINS))%>%
  ggplot(aes(x = factor(YEAR), y = PER_DEAD, fill = factor(YEAR))) +
  facet_wrap(~ TAIL_BINS)+
  geom_boxplot() +
  scale_fill_viridis_d(option = "plasma") +
  theme_minimal() +
  labs(
    x = "Year",
    y = "Partial Mortality (%)",
    title = "Boxplot of Partial Mortality by Year (2015 vs 2023, bootstrapped)"
  ) +
  theme(
    text = element_text(size = 14),
    plot.title = element_text(hjust = 0.5)
  )
ggplot2::ggsave ("plots/boxplot_PM_size_class_and_year_bootstrapped.jpeg", width = 5, height = 2.5, units = 'in')


################
#dot plot#######
################
#boot_summary: parameter estimates (how much PM changed across years)
#mean_PM_summary: actual mean partial mortality (%) by year

# Plotting mean partial mortality per year.  for main text.
ggplot(mean_PM_summary, aes(x = factor(YEAR), y = mean_PM_mean, color = factor(YEAR))) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower_CI, ymax = upper_CI), width = 0.2) +
  theme_minimal() +
  labs(
    x = "Year",
    y = "Mean Partial Mortality (%)",
    title = "Average Partial Mortality by Year (Bootstrapped)",
    color="Year"
  ) +
  theme(
    text = element_text(size = 14),
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_color_viridis_d(option = "C") +
  scale_y_continuous(
    limits = c(0, NA),  # Set y-axis to start from 0
    breaks = seq(0, max(mean_PM_summary$mean_PM_mean), by = 10)  # Adjust the break interval
  )
ggplot2::ggsave ("plots/dotplot_PM_size_bootstrapped.jpeg", width = 5, height = 2.5, units = 'in')


# Plotting mean partial mortality per year.  for main text.
ggplot(boot_summary_size, aes(x = factor(YEAR), y = mean_PM, color = factor(YEAR))) +
  geom_point(size = 3) +
  facet_wrap(~ TAIL_BINS)+
  geom_errorbar(aes(ymin = lower_CI, ymax = upper_CI), width = 0.2) +
  theme_minimal() +
  labs(
    x = "Year",
    y = "Mean Partial Mortality (%)",
    title = "Average Partial Mortality by Year (Bootstrapped)",
    color="Year"
  ) +
  theme(
    text = element_text(size = 14),
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_color_viridis_d(option = "C") +
  scale_y_continuous(
    limits = c(0, NA),  
    breaks = seq(0, max(boot_summary_size$mean_PM), by = 10) 
  )
ggplot2::ggsave ("plots/dotplot_PM_size_bootstrapped.jpeg", width = 5, height = 2.5, units = 'in')



# Plotting mean partial mortality per year barplot.  for main text.
ggplot(mean_PM_summary, aes(x = factor(YEAR), y = mean_PM_mean, fill = factor(YEAR))) +
  geom_col() +
  geom_errorbar(aes(ymin = lower_CI, ymax = upper_CI), width = 0.2) +
  theme_minimal() +
  labs(
    x = "Year",
    y = "Mean Partial Mortality (%)",
    title = "Average Partial Mortality by Year (Bootstrapped)",
    fill="Year"
  ) +
  theme(
    text = element_text(size = 14),
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_color_viridis_d(option = "C") +
  scale_y_continuous(
    limits = c(0, NA),  # Set y-axis to start from 0
    breaks = seq(0, max(mean_PM_summary$mean_PM_mean), by = 10)  # Adjust the break interval
  )

ggplot2::ggsave ("plots/barplot_PM_size_by_class_bootstrapped.jpeg", width = 5, height = 2.5, units = 'in')

#add meanPM summary by size class, make dot plot xxx also add bar plots

#plotting regression coefficient. first remove intercept (2015), b/c graphing change since 2015
boot_summary_no_intercept <- boot_summary %>%
  filter(term != "(Intercept)") %>%
  mutate(
    YEAR = case_when(
      term == "factor(YEAR)2023" ~ 2023,
      term == "factor(YEAR)2025" ~ 2025
    )
  )

# Plotting regression coefficients for years relative to 2015. For supplement.
ggplot(boot_summary_no_intercept, aes(x = factor(YEAR), y = estimate_mean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower_CI, ymax = upper_CI), width = 0.2) +
  theme_minimal() +
  labs(
    x = "Year (vs 2015 Baseline)",
    y = "Change in Partial Mortality (%)",
    title = "Effect of Year on Partial Mortality (Bootstrapped Coefficients)"
  ) +
  scale_color_viridis_d(option = "C") +
  theme(
    text = element_text(size = 14),
    plot.title = element_text(hjust = 0.5)
  )


# Clean up boot_summary for reporting
boot_summary_table <- boot_summary %>%
  mutate(
    term = case_when(
      term == "(Intercept)" ~ "2015 baseline",
      term == "factor(YEAR)2023" ~ "Change in 2023 vs 2015",
      term == "factor(YEAR)2025" ~ "Change in 2025 vs 2015",
      TRUE ~ term
    )
  ) %>%
  select(
    Parameter = term,
    Mean_Estimate = estimate_mean,
    SD = estimate_sd,
    Lower_95CI = lower_CI,
    Upper_95CI = upper_CI
  )

# View table
boot_summary_table

#Parameter              Mean_Estimate       SD Lower_95CI Upper_95CI
#<chr>                          <dbl>    <dbl>      <dbl>      <dbl>
#  1 2015 baseline                  7.48  1.02e-13      7.48       7.48 
#2 Change in 2023 vs 2015         0.417 1.19e-13      0.417      0.417
#3 Change in 2025 vs 2015        22.3   5.15e+ 0     13.7       32.7 

# Plot all parameters (including intercept)
ggplot(boot_summary, aes(x = reorder(term, estimate_mean), y = estimate_mean)) +
  geom_point(size = 3, color = "darkorchid4") +
  geom_errorbar(aes(ymin = lower_CI, ymax = upper_CI), width = 0.2, color = "darkorchid4") +
  coord_flip() +
  theme_minimal() +
  labs(
    x = "Parameter",
    y = "Estimate (Partial Mortality %)",
    title = "Bootstrapped Parameter Estimates"
  ) +
  theme(
    text = element_text(size = 14),
    plot.title = element_text(hjust = 0.5)
  )
ggplot2::ggsave ("plots/bootstrap_parameters_for_2025.jpeg", width = 5, height = 2.5, units = 'in')

#mean pm increased from 7.5% (2015) to 29.9% (2025), based on bootstrapped estimates (95% CI for the 2025 increase: 9.4% to 39.1%).



#comapre bootstrapped data to non
#run model for full data
model_nonboot <- glm(PER_DEAD ~ factor(YEAR), data = dat_sub, weights = weight, family = gaussian())


#add column identifiying method
boot_summary$Method <- "Bootstrapped"
nonboot_summary$Method <- "Full data"

#combine the data
combined_summary <- bind_rows(boot_summary, nonboot_summary)

# make sure columns match
boot_part <- combined_summary %>%
  filter(Method == "Bootstrapped") %>%
  transmute(
    Parameter = case_when(
      term == "(Intercept)" ~ "2015 baseline",
      term == "factor(YEAR)2023" ~ "Change in 2023 vs 2015",
      term == "factor(YEAR)2025" ~ "Change in 2025 vs 2015"
    ),
    Mean_Estimate = estimate_mean,
    SD = estimate_sd,
    Lower_95CI = lower_CI,
    Upper_95CI = upper_CI,
    Method = "Bootstrapped"
  )

full_part <- combined_summary %>%
  filter(Method == "Full data") %>%
  select(Parameter, Mean_Estimate, SD, Lower_95CI, Upper_95CI, Method)

#combine 
estimates_clean <- bind_rows(boot_part, full_part)

ggplot(estimates_clean, aes(x = Parameter, y = Mean_Estimate, color = Method, shape = Method)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = Lower_95CI, ymax = Upper_95CI),
                position = position_dodge(width = 0.5), width = 0.2) +
  theme_minimal() +
  labs(
    x = "",
    y = "Mean Partial Mortality (%)",
    title = "Bootstrapped vs Non-Bootstrapped Parameter Estimates"
  ) +
  scale_color_viridis_d(option = "D") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.title.y = element_text(size = 14),
    plot.title = element_text(size = 16, hjust = 0.5)
  )
ggplot2::ggsave ("plots/model_comparisons_bootstrap_vs_full_PM_by_year.jpeg", width = 5, height = 2.5, units = 'in')

#exploring other than glm:
library(glmmTMB)

# Ensure PM is scaled between 0 and 1 (not 0 or 1 exactly)
dat_sub <- dat_sub %>%
  filter(!is.na(PER_DEAD)) %>%
  mutate(
    PM_prop = (PER_DEAD + 0.01) / 100.02  # scale to (0,1) range
  )

# Fit model
mod <- glmmTMB(
  PM_prop ~ YEAR + TAIL_BINS + (1 | SITE),
  data = dat_sub,
  family = beta_family(link = "logit")
)
summary(mod)

emmeans(mod, pairwise ~ YEAR)

