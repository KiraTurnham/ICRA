library(dplyr)
library(tidyr)
library(ggplot2)
library(broom)
library(purrr)
library(sf)
library(viridis)
library(cowplot)

tutuila_shape <- st_read("data/Tut_shapefiles/TUT.shp") #make sure folder has all shapefiles needed

#plot PM across island

# mean PM per year/site
mean_PM_per_year_site <- dat %>%
  group_by(YEAR, SITE, LATITUDE, LONGITUDE) %>%
  summarise(mean_PM = mean(PER_DEAD, na.rm = TRUE))

#assign coordinates for PM data. Want to keep PM=0 but not NA
sites_sf_PM <- mean_PM_per_year_site %>%
  filter(!is.na(mean_PM)) %>%
  distinct(LATITUDE, LONGITUDE, YEAR, .keep_all = TRUE) %>%  # keep all columns
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326)  # WGS84 

ggplot() +
  geom_sf(data = tutuila_shape, fill = "gray90", color = "black") +
  geom_sf(data = sites_sf_PM, aes(color = mean_PM, shape = as.factor(YEAR)), 
          size = 2, alpha = 0.7) +
  scale_color_viridis(option = "C") +
  scale_shape_manual(values = c(16, 17, 15, 18)) +
  labs(
    x = "Longitude", y = "Latitude",
    color = "Mean % PM",
    shape = "Year"
  ) +
  guides(
    color = guide_colorbar(barwidth = 0.4, barheight = 4),
    shape = guide_legend(override.aes = list(size = 2), 
                         keywidth = 0.5, keyheight = 0.5)
  ) +
  theme(
    axis.text.x = element_text(size = 5, angle = 90, hjust = 0),
    axis.text.y = element_text(size = 5),
    axis.title = element_text(size = 8),
    text = element_text(size = 6),
    plot.title = element_text(hjust = 0.5),
    legend.position = "right",  # options: "bottom", "top", etc.
    #legend.direction = "horizontal",
    legend.box = "horizontal",  # places multiple legends side by side
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 8)
  )

ggplot2::ggsave ("plots/Partial_mortaltiy_by_year_map.jpeg", width = 5, height = 2.5, units = 'in')
