library(dplyr)
library(tidyr)
library(ggplot2)
library(broom)
library(purrr)


#double check site numbers for PM data (remove NA, but keep zeros)
dat %>%
  group_by(YEAR) %>%
  filter(!is.na(PER_DEAD))%>%
  summarize(n_unique_sites = n_distinct(paste(LATITUDE, LONGITUDE)))
#YEAR  n_unique_sites
#1 2015              15
#2 2018               2 #drop 2018, as only 2 sites
#3 2023              12
#4 2025              33 #should weight and bootstrap 2025 data to 12 sites


# Remove 2018 due to low sample coverage
dat_sub <- dat %>%
  filter(YEAR != 2018) %>%
  filter(!is.na(PER_DEAD))  # remove NAs but keep zeros

# calculate weights
dat_sub <- dat_sub %>%
  group_by(YEAR) %>%
  mutate(weight = 1 / n())%>%
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