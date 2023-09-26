### Chinook Chapter 1 Vessel Effects Test
## author: Lilian Hart
## Last edited: 07/22/23
require(tidyverse)
require(dplyr)
require(here)
require(viridis)
require(mgcv)
require(visreg)
require(ggplot2)
require(ggmap)
require(gratia)
require(beepr)

#### Setup ####
dir.work <- here("data", "Chapter_1_RDSModels")
og <- readRDS(file.path(dir.work,"V4_basis_subset.rds"))
setwd(dir.work)
  
species <- "Chinook Salmon"
  
dat <- og %>% filter(CommonName == species) %>% drop_na(CommonName,
                                                          Effort_area_km2,
                                                          TotalCatchNum) # Save factor versions of Sample Year 
dat$fSampleYear <- as.factor(dat$SampleYear)

## Model 1: Average spatial field 
mod1 <- gam(TotalCatchNum ~ te(EQ.Longitude,EQ.Latitude, bs = "tp", 
                                 k = 9, m = 1) + s(fSampleYear, bs = "re") +
              VesselName +
              offset(log(Effort_area_km2)), 
            family = tw(link = "log"), data = dat) 

summary(mod1)
  