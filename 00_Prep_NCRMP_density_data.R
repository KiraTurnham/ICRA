#Creating NCRMP density data at site level using filtered ICRA/ISSP colony level data

#load libraries
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(ggridges)

rm(list=ls())
#dir = Sys.info()[7]
#setwd(paste0("C:/Users/", dir, "/Documents/github/ICRA/"))
setwd("C:/github/ICRA/data")

####PREP DATA---------------

#load data 
dat <- read.csv("NCRMP_COlony_level_TUT_filtered.csv")%>% mutate_if(is.character,as.factor)

#quick check of number of sites surveyed per NCRMP year
tbl_sites<- dat%>% 
  group_by(YEAR) %>%
  summarise(N =length(SITE))

ncrmp <- dat%>%
  select(YEAR, SITE, MIN_DEPTH_M,MAX_DEPTH_M, LATITUDE, LONGITUDE, TRANSECT, SEGMENT, SEGWIDTH, SEGLENGTH, COLONYID, TRANSECTAREA)%>%
    mutate(SEG_AREA = SEGWIDTH*SEGLENGTH)

den <-  ncrmp%>%
  group_by(YEAR, SITE) %>%
  summarise(COL_COUNT = n_distinct(COLONYID))
  
site_area <- ncrmp%>%
  select(SITE, TRANSECT, TRANSECTAREA)%>%
  distinct()
