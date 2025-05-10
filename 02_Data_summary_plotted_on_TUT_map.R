library(dplyr)
library(tidyr)
library(ggplot2)
library(broom)
library(purrr)
library(sf)
library(viridis)
library(cowplot)
library("svglite")

# set colors
vir_colors <- viridis(n = 4, option = "C")
print(vir_colors)

custom_colors <- vir_colors
custom_colors[4] <- "gold"  # DAA520 goldenrod 

load("data/filtered_colony_data.RData")
load("data/filtered_south_colony_data.RData")
load("data/SITE_DEN.RData")
filtered_density<-read.csv("data/south_only_2025_ICRA_density_TUT.csv")
tutuila_shape <- st_read("data/Tut_shapefiles/TUT.shp") #make sure folder has all shapefiles needed

#plot PM, density across island

# mean PM per year/site of south only
mean_PM_per_year_site_south <- filtered_south_colony_data %>%
  group_by(YEAR, SITE, LATITUDE, LONGITUDE) %>%
  summarise(mean_PM = mean(PER_DEAD, na.rm = TRUE))

# mean PM per year/site of all sites (using the size-cutoff data)
mean_PM_per_year_site <- filtered_colony_data %>%
  group_by(YEAR, SITE, LATITUDE, LONGITUDE) %>%
  summarise(mean_PM = mean(PER_DEAD, na.rm = TRUE))

#assign coordinates for PM data. Want to keep PM=0 but not NA
sites_sf_PM_south <- mean_PM_per_year_site_south %>%
  #filter(!is.na(mean_PM)) %>%
  distinct(LATITUDE, LONGITUDE, YEAR, .keep_all = TRUE) %>%  # keep all columns
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326)  # WGS84 

sites_sf_PM <- mean_PM_per_year_site %>%
  #filter(!is.na(mean_PM)) %>%
  distinct(LATITUDE, LONGITUDE, YEAR, .keep_all = TRUE) %>%  # keep all columns
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326)  # WGS84 


#plot PM
sites_sf_PM_south <- sites_sf_PM_south %>%
  mutate(shape_code = ifelse(is.na(mean_PM), "NA", as.character(YEAR)))
sites_sf_PM <- sites_sf_PM %>%
  mutate(shape_code = ifelse(is.na(mean_PM), "NA", as.character(YEAR)))

ggplot() +
  geom_sf(data = tutuila_shape, fill = "gray90", color = "black") +
  geom_sf(data = sites_sf_PM_south, aes(color = mean_PM, shape = shape_code), 
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

ggplot2::ggsave ("plots/Map_south_Partial_mortaltiy_by_year_map.jpeg", width = 5, height = 2.5, units = 'in')

##########
#Density##
##########

# mean density per year/site
mean_den_per_year_site <- SITE_DEN %>%
  group_by(YEAR, SITE, LATITUDE, LONGITUDE) %>%
  summarise(mean_den = mean(DENSITY, na.rm = TRUE))

#assign coordinates for density data. Want to remove NA 
sites_sf_den <- mean_den_per_year_site %>%
  filter(!is.na(mean_den)) %>%
  distinct(LATITUDE, LONGITUDE, YEAR, .keep_all = TRUE) %>%  # keep all columns
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326)  # WGS84 

sites_sf_den$log_mean_den <- log10(sites_sf_den$mean_den + 0.01)

#plot density
sites_sf_den <- sites_sf_den %>%
  mutate(shape_code = ifelse(mean_den == 0, "Zero", as.character(YEAR)))

ggplot() +
  geom_sf(data = tutuila_shape, fill = "gray90", color = "black") +
  geom_sf(data = sites_sf_den, aes(color = log_mean_den, shape = shape_code), 
          size = 2, alpha = 0.7) +
  scale_color_viridis_d(option = "C") +
  #scale_fill_manual(values = custom_colors) +
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


#For south sites only
mean_den_per_year_site_south <- filtered_density %>%
  group_by(YEAR, SITE, LATITUDE, LONGITUDE) %>%
  summarise(mean_den = mean(DENSITY, na.rm = TRUE))

#assign coordinates for density data. Want to remove NA 
sites_sf_den_south <- mean_den_per_year_site_south %>%
  filter(!is.na(mean_den)) %>%
  distinct(LATITUDE, LONGITUDE, YEAR, .keep_all = TRUE) %>%  # keep all columns
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326)  # WGS84 

sites_sf_den_south$log_mean_den <- log10(sites_sf_den_south$mean_den + 0.01)

sites_sf_den_south <- sites_sf_den_south %>%
  mutate(shape_code = ifelse(mean_den == 0, "Zero", as.character(YEAR)))

ggplot() +
  geom_sf(data = tutuila_shape, fill = "grey", color = "black") +
  geom_sf(data = sites_sf_den_south, 
          aes(shape = as.factor(YEAR), fill = as.factor(YEAR)),
          size = 1.5, stroke = 0.5, alpha = 0.7, color = "black") +
  scale_shape_manual(values = c("2015" = 22, "2018" = 23, "2023" = 24, "2025" = 21)) +
  #scale_fill_manual(values = custom_colors) +
  scale_fill_viridis_d(option = "C") +
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
ggplot2::ggsave ("plots/Density_by_year_map_south.jpeg", width = 5, height = 2.5, units = 'in')

#plot sample sites 
ggplot() +
  geom_sf(data = tutuila_shape, fill = "grey", color = "black") +
  geom_sf(data = sites_sf_den_south, 
          aes(shape = as.factor(YEAR), fill = as.factor(YEAR)),
          size = 1.5, stroke = 0.5, alpha=0.9, color = "black") +
  scale_shape_manual(values = c("2015" = 22, "2018" = 23, "2023" = 24, "2025" = 21)) +
  scale_fill_manual(values = custom_colors) +
  labs(
    x = "Longitude", y = "Latitude",
    fill = "Year",
    shape = "Year"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 5, angle = 90, hjust = 0),
    axis.text.y = element_text(size = 5),
    axis.title = element_text(size = 8),
    text = element_text(size = 6),
    plot.title = element_text(hjust = 0.5),
    legend.position = "top",
    legend.key.size = unit(0.4, "cm"),
    legend.box = "horizontal",
    legend.text = element_text(size = 5),
    legend.title = element_text(size = 5)
  )

ggplot2::ggsave ("plots/south_Site_map_by_year1.png", width = 5, height = 2.5, units = 'in',  bg = "transparent")


#way to plot so that 2025 data is not on top
# Make sure YEAR is a factor with the correct level order
sites_sf_den_south$YEAR <- factor(sites_sf_den_south$YEAR, levels = c("2015", "2018", "2023", "2025"))

ggplot() +
  geom_sf(data = tutuila_shape, fill = "grey", color = "black") +
  
  # Draw 2025 points first (back layer)
  geom_sf(data = subset(sites_sf_den_south, YEAR == "2025"),
          aes(fill = YEAR, shape = YEAR),
          size = 2, stroke = 0.5, alpha=0.8, color = "black") +
  
  # Then draw all others on top
  geom_sf(data = subset(sites_sf_den_south, YEAR != "2025"),
          aes(fill = YEAR, shape = YEAR),
          size = 1.5, stroke = 0.5, alpha=0.8, color = "black") +
  
  scale_shape_manual(values = c("2015" = 22, "2018" = 23, "2023" = 24, "2025" = 21)) +
  scale_fill_manual(
    values = c("2015" = custom_colors[1],
               "2018" = custom_colors[2],
               "2023" = custom_colors[3],
               "2025" = custom_colors[4])  # Yellow stays for 2025
  ) +
  labs(
    x = "Longitude", y = "Latitude",
    fill = "Year",
    shape = "Year"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 5, angle = 90, hjust = 0),
    axis.text.y = element_text(size = 5),
    axis.title = element_text(size = 8),
    text = element_text(size = 6),
    plot.title = element_text(hjust = 0.5),
    legend.position = "top",
    legend.key.size = unit(0.4, "cm"),
    legend.box = "horizontal",
    legend.text = element_text(size = 5),
    legend.title = element_text(size = 5)
  )
ggplot2::ggsave ("plots/south_Site_map_by_year3.png", width = 5, height = 2.5, units = 'in',  bg = "transparent")
