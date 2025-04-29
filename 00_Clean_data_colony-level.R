#Cleaning ICRA data colony level
rm(list=ls())

#load libraries
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(ggridges)
library(readxl)

#dir = Sys.info()[7]
#setwd(paste0("C:/Users/", dir, "/Documents/github/ICRA/"))
#setwd("C:/github/ICRA/data")

####PREP DATA---------------

#load data and filter data 

ncrmp <- read.csv("data/CoralBelt_Adults_raw_CLEANED_2023.csv")%>% mutate_if(is.character,as.factor) %>%  
  filter(ISLANDCODE == "TUT", REEF_ZONE == "Forereef", OBS_YEAR != "2020", DEPTH_BIN == "Mid", SPCODE %in% c("ISSP", "ICRA")) %>%
  filter(!MORPHOLOGY %in% c("Branching", "Columnar")) %>%
  mutate(PER_DEAD = OLDDEAD + RDEXTENT1 + RDEXTENT2) %>%
  filter(COLONYLENGTH > 4)%>% #only use colonies >5
rename(YEAR = OBS_YEAR)%>%
   # Lat = LATITUDE,
   # Long = LONGITUDE,
   # Site = SITE)%>%
  droplevels()

  

#Calculate quintiles of pre-bleaching year data to assign size classes 
q<- data.frame(quantile(ncrmp$COLONYLENGTH, probs =  c(20,80)/100, na.rm = FALSE, names = TRUE, type = 9, digits = 4))
#(5-12 cm = "small", >40 = "brood stock")

#add quintiles to dataframe in new column TAIL_BINS
ncrmp$TAIL_BINS=cut(ncrmp$COLONYLENGTH,c(-Inf,q[1,1],q[2,1],Inf),labels=c('Q20','QMED','Q80'))

write.csv(ncrmp, "NCRMP_COlony_level_TUT_filtered.csv")

#make sure columns are read as correct format
ncrmp$YEAR <- as.factor(ncrmp$YEAR)
ncrmp$COLONYLENGTH <- as.numeric(ncrmp$COLONYLENGTH)
ncrmp$PER_DEAD <- as.numeric(ncrmp$PER_DEAD)
ncrmp2 <- select(ncrmp, MAX_DEPTH_M, SITE, PER_DEAD, LATITUDE, LONGITUDE, YEAR)

#read in 2025 survey data

ICRA_2025 <- ICRA_2025 %>%
  mutate(ICRA_size_cm = na_if(ICRA_size_cm, "NA"))  # Convert the string "NA" to actual NA

ICRA_2025 <- ICRA_2025 %>%
  mutate(ICRA_partial_mortality = na_if(ICRA_partial_mortality, "NA"))  # Convert the string "NA" to actual NA

#rename PM column
ICRA_2025 <- ICRA_2025 %>%
  rename(ICRA_total_PM = ICRA_partial_mortality) %>%
  rename(Size_cm = ICRA_size_cm)
#mutate(ICRA_total_PM = ifelse(is.na(ICRA_total_PM), NA, ICRA_total_PM))  # keeps NAs intact

#ensure size data and PM are numeric
ICRA_2025 <- ICRA_2025 %>%
  mutate(Size_cm = as.numeric(Size_cm)) %>%
  #mutate(ICRA_size_cm = ifelse(is.na(ICRA_size_cm), NA, ICRA_size_cm))  # keeps NAs intact
  mutate(ICRA_total_PM = as.numeric(ICRA_total_PM))

#ensure only colonies 5cm or greater were sized
ICRA_2025 <- ICRA_2025 %>%
  filter(Size_cm > 4.9 | is.na(Size_cm))
esa <- read.csv("Feb2025_surveydata_raw.csv")%>% mutate_if(is.character,as.factor) %>%  
  select(MAX_depth_m, Site, ICRA_percent_partial_mortality, Lat, Long)%>%
  rename(SITE = Site, LATITUDE = Lat, LONGITUDE = Long, PER_DEAD = ICRA_percent_partial_mortality, MAX_DEPTH_M = MAX_depth_m) %>%
  drop_na()
esa$YEAR <- as.factor(esa$YEAR <-  "2025")
 


#merge ncmrp and esa data    
colnames(esa)
colnames(ncrmp2)
dat <- rbind(esa,ncrmp2)
dat$YEAR <- ordered(dat$YEAR, levels = c("2015", "2018", "2023", "2025"))

ridge <- ggplot(dat, aes(x=PER_DEAD, y = YEAR, fill=YEAR)) +
  geom_density_ridges() +
  stat_density_ridges(quantile_lines = TRUE, quantiles = 2, alpha = 0.5, color= "black", linewidth = 0.5)+
  scale_fill_viridis(discrete = TRUE) +
  theme_ipsum_pub() +
  scale_y_discrete(limits = rev)+
  xlab("ICRA partial mortality (%) over time")

ridge

setwd("C:/github/ICRA/plots")
ggplot2::ggsave ("Partial_mortality_ridge.jpeg", width = 5, height = 5, units = 'in')

ridge2 <- ggplot(dat, aes(x=PER_DEAD, y = YEAR, fill=..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "PER_DEAD", option = "plasma") +
  labs(title = 'ICRA partial mortality (%) over time') +
  theme_ipsum() +
  scale_y_discrete(limits = rev)+
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )

ridge2




