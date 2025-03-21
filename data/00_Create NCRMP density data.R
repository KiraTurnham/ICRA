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
ncrmp <- read.csv("NCRMP_COlony_level_TUT_filtered.csv")%>% mutate_if(is.character,as.factor)