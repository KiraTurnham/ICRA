#Creating NCRMP density data at site level using filtered ICRA/ISSP colony level data
#then merging NCRMP site-level density with ESA site-level density (pulled from Dive Nav)

#load libraries
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(ggridges)

rm(list=ls())
#dir = Sys.info()[7]
#setwd(paste0("C:/Users/", dir, "/Documents/github/ICRA/"))
#setwd("C:/github/ICRA/data")

####PREP NCMRP DATA---------------

#load data 
ICRA.dat <- read.csv("NCRMP_COlony_level_TUT_filtered.csv")%>% mutate_if(is.character,as.factor)%>%
  select(YEAR, DATE_, SITE, SPCODE, COLONYID)

site <- read.csv("data/CoralBelt_Adults_raw_CLEANED_2023.csv")%>% mutate_if(is.character,as.factor)%>%
  filter(ISLANDCODE == "TUT", REEF_ZONE == "Forereef", OBS_YEAR != "2020", DEPTH_BIN == "Mid")%>%
  rename(YEAR = OBS_YEAR)%>%
  select(YEAR, DATE_, SITE, LATITUDE, LONGITUDE, MIN_DEPTH_M,MAX_DEPTH_M, TRANSECTAREA)%>%
  distinct()%>%
  droplevels()

#sum transect area per site
site <- site %>% 
  group_by(YEAR, SITE, LATITUDE, LONGITUDE, MIN_DEPTH_M,MAX_DEPTH_M)%>%
  mutate(SURVEYAREA = sum(TRANSECTAREA))%>%
  select(-TRANSECTAREA)%>%
  distinct()
#115 total sites


#colonies per site
col <-  ICRA.dat%>%
  group_by(YEAR, SITE) %>%
  summarise(COL_COUNT = n_distinct(COLONYID))

NCRMP <- left_join(site, col)%>%
  replace(is.na(.), 0)%>%
  mutate(DENSITY=COL_COUNT/SURVEYAREA)%>%
  ungroup()

NCRMP$YEAR <- as.factor(NCRMP$YEAR)


####read in and merge ESA data to NCRMP data
ESA <- read_csv("data/ESA_Corals_Site_Density_2025.csv") %>% mutate_if(is.character,as.factor)
as.factor(ESA$YEAR <- "2025")

ESA <- ESA %>% select(YEAR, Date, Site, Lat, Long, Max_depth_m, Survey_area, ICRA_density)%>%
  rename(DATE_=Date, SITE=Site, LATITUDE =Lat, LONGITUDE = Long, SURVEYAREA = Survey_area, DENSITY = ICRA_density, MAX_DEPTH_M = Max_depth_m)
NCRMP <- NCRMP %>% select(-MIN_DEPTH_M, -COL_COUNT)

colnames(NCRMP)
colnames(ESA)

SITE_DEN <- rbind(NCRMP, ESA)


#quick check of number of sites surveyed per year
tbl_sites<- SITE_DEN%>% 
  group_by(YEAR) %>%
  summarise(N =length(unique(SITE)))

write.csv(NCRMP, "NCRMP_ICRA_density_site-level.csv")
write_csv(SITE_DEN, "ICRA_Site_Density_ALL_YEAR.csv")
save(SITE_DEN, file="data/SITE_DEN.RData")
