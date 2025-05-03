library(dplyr)
library(tidyr)
library(ggplot2)
library(broom)
library(purrr)
library(sf)
library(viridis)
library(cowplot)

load("data/COLONY_SIZE_PM.RData")
load("data/SITE_DEN.RData")
tutuila_shape <- st_read("data/Tut_shapefiles/TUT.shp") #make sure folder has all shapefiles needed

#plot PM, density across island

# mean PM per year/site
mean_PM_per_year_site <- COLONY_SIZE_PM %>%
  group_by(YEAR, SITE, LATITUDE, LONGITUDE) %>%
  summarise(mean_PM = mean(PER_DEAD, na.rm = TRUE))

# mean density per year/site
mean_den_per_year_site <- SITE_DEN %>%
  group_by(YEAR, SITE, LATITUDE, LONGITUDE) %>%
  summarise(mean_den = mean(DENSITY, na.rm = TRUE))

sites_sf_den$log_mean_den <- log10(sites_sf_den$mean_den + 0.01)

#assign coordinates for PM data. Want to keep PM=0 but not NA
sites_sf_PM <- mean_PM_per_year_site %>%
  #filter(!is.na(mean_PM)) %>%
  distinct(LATITUDE, LONGITUDE, YEAR, .keep_all = TRUE) %>%  # keep all columns
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326)  # WGS84 

#assign coordinates for density data. Want to remove NA 
sites_sf_den <- mean_den_per_year_site %>%
  filter(!is.na(mean_den)) %>%
  distinct(LATITUDE, LONGITUDE, YEAR, .keep_all = TRUE) %>%  # keep all columns
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326)  # WGS84 

#plot PM
sites_sf_PM <- sites_sf_PM %>%
  mutate(shape_code = ifelse(is.na(mean_PM), "NA", as.character(YEAR)))

ggplot() +
  geom_sf(data = tutuila_shape, fill = "gray90", color = "black") +
  geom_sf(data = sites_sf_PM, aes(color = mean_PM, shape = shape_code), 
          size = 2, alpha = 0.7) +
  scale_color_viridis(option = "C") +
  scale_shape_manual(
    values = c("2015" = 16, "2018" = 17, "2023" = 15, "2025" = 18, "NA" = 4)
  )+
  labs(
    x = "Longitude", y = "Latitude",
    color = "Mean % PM",
    shape = "Year"
  ) +
  coord_sf(expand = FALSE) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 5, angle = 90, hjust = 0),
    axis.text.y = element_text(size = 5),
    axis.title = element_text(size = 8),
    text = element_text(size = 6),
    plot.title = element_text(hjust = 0.5),
    legend.position = "top",  # options: "bottom", "top", etc.
    #legend.direction = "horizontal",
    legend.key.size = unit(0.2, "cm"),
    legend.box = "horizontal",  # places multiple legends side by side
    legend.text = element_text(size = 5),
    legend.title = element_text(size = 5)
  )

ggplot2::ggsave ("plots/Partial_mortaltiy_by_year_map.jpeg", width = 5, height = 2.5, units = 'in')

#plot density
sites_sf_den <- sites_sf_den %>%
  mutate(shape_code = ifelse(mean_den == 0, "Zero", as.character(YEAR)))

ggplot() +
  geom_sf(data = tutuila_shape, fill = "gray90", color = "black") +
  geom_sf(data = sites_sf_den, aes(color = log_mean_den, shape = shape_code), 
          size = 2, alpha = 0.7) +
  scale_color_viridis(option = "C") +
  scale_shape_manual(
    values = c("2015" = 16, "2018" = 17, "2023" = 15, "2025" = 18, "Zero" = 4)
  )+
  labs(
    x = "Longitude", y = "Latitude",
    color = "Log Mean density\n(colonies m²)",
    shape = "Year"
  ) +
    theme(
      axis.text.x = element_text(size = 5, angle = 90, hjust = 0),
      axis.text.y = element_text(size = 5),
      axis.title = element_text(size = 8),
      text = element_text(size = 6),
      plot.title = element_text(hjust = 0.5),
      legend.position = "top",  # options: "bottom", "top", etc.
      #legend.direction = "horizontal",
      legend.key.size = unit(0.4, "cm"),
      legend.box = "horizontal",  # places multiple legends side by side
      legend.text = element_text(size = 5),
      legend.title = element_text(size = 5)
    )
ggplot2::ggsave ("plots/Density_by_year_map.jpeg", width = 5, height = 2.5, units = 'in')
