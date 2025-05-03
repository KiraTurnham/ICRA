#Analyze different survey methods in 2025

load("data/COLONY_SIZE_PM.RData")
# calculate number of colonies counted for size data in first 10mx2m
ICRA_20m %>%
  summarise(
    non_na_count = sum(!is.na(COLONYLENGTH)),
    na_count = sum(is.na(COLONYLENGTH))
  )
#416 corals measured

