library(dplyr)
library(tidyr)
library(ggplot2)
library(broom)
library(purrr)
library(sf)

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
  #scale_color_gradientn(colors = c("#00AFBB", "#E7B800", "#FC4E07"), name = "Total PM") +
  scale_color_viridis(option = "D")+
  scale_shape_manual(values = c(16, 17, 15, 18))+
  theme_minimal() +
  labs(title = "ICRA PM by Year on Tutuila",
       x = "Longitude", y = "Latitude")

