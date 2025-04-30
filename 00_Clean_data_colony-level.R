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
  mutate(PER_DEAD = OLDDEAD + RDEXTENT1 + RDEXTENT2,
         Area_surveyed_m2 = 10) %>%
  filter(COLONYLENGTH > 4.9)%>% #only use colonies >5
rename(YEAR = OBS_YEAR)%>%
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
ncrmp2 <- select(ncrmp, COLONYLENGTH, Area_surveyed_m2, MAX_DEPTH_M, SITE, PER_DEAD, LATITUDE, LONGITUDE, YEAR, TAIL_BINS)

#read in 2025 survey data
ICRA_2025 <- read.csv("data/2025_ICRA_colony_level_TUT.csv") %>%
  mutate(ICRA_size_cm = as.numeric(ICRA_size_cm),
         ICRA_partial_mortality = as.numeric(ICRA_partial_mortality),
         Year=as.factor(Year),
         Date = as.Date(Date),
         MAX_DEPTH_M = MAX_depth_ft * 0.3048) %>%
  rename(PER_DEAD = ICRA_partial_mortality,
         COLONYLENGTH = ICRA_size_cm,
         YEAR = Year,
         LATITUDE = Lat,
         LONGITUDE = Long,
         SITE = Site)

ICRA_2025 <- ICRA_2025 %>%
  filter(COLONYLENGTH > 4.9 | is.na(COLONYLENGTH))

ICRA_2025$TAIL_BINS <- cut(
  ICRA_2025$COLONYLENGTH, 
  breaks = c(-Inf, 12, 40, Inf), 
  labels = c('Q20', 'QMED', 'Q80'))

esa <- select(ICRA_2025, COLONYLENGTH, MAX_DEPTH_M, Area_surveyed_m2, SITE, PER_DEAD, LATITUDE, LONGITUDE, YEAR, TAIL_BINS)


#Make dataframe based on corals measured w/i first 10mx2m of 2025 transects (to compare methods)
ICRA_2025 <- ICRA_2025 %>%
  mutate(First10m_YN = as.numeric(First10m_YN))

ICRA_20m <- ICRA_2025 %>%
  filter(First10m_YN == 1)

#add column for survey area
ICRA_20m$Area_surveyed_m2<- 20
ICRA_sub <- select(ICRA_20m, COLONYLENGTH, Area_surveyed_m2, MAX_DEPTH_M, SITE, PER_DEAD, LATITUDE, LONGITUDE, YEAR, TAIL_BINS)
ICRA_sub$YEAR <- ordered(dat$YEAR, levels = c("2015", "2018", "2023", "2025"))

#merge ncmrp and esa data    
colnames(esa)
colnames(ncrmp2)
dat <- rbind(esa,ncrmp2)%>%
  mutate(Bleaching_Period = ifelse(YEAR %in% c(2015, 2018, 2023), "Pre-Bleaching", "Post-Bleaching"))

dat$YEAR <- ordered(dat$YEAR, levels = c("2015", "2018", "2023", "2025"))

write.csv(dat, "data/all_ICRA_Colony_level_data.csv", row.names = FALSE)


#combine the 20m data and store as seperate dataframe to compare survey methods
colnames(ICRA_sub)
ICRA_sub$Bleaching_Period <- "Post-Bleaching"
combined_colony_data_by_method <- rbind(dat,ICRA_sub)

#name the different survey method areas
combined_colony_data_by_method <- combined_colony_data_by_method %>%
  mutate(Survey_Type = case_when(
    Area_surveyed_m2 == 20 ~ "20m",
    Area_surveyed_m2 == 60 ~ "60m",
    Area_surveyed_m2 == 10 ~ "10m",
    TRUE ~ "Other"
  ))
write.csv(combined_colony_data_by_method, "data/ICRA_combined_size_data_by_method.csv", row.names = FALSE)


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




