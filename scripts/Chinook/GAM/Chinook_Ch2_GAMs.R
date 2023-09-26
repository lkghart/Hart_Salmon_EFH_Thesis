### Chapter 2 GAMs, Chinook ###
# author: Lilian Hart
# date last modified: 06/13/23

require(dplyr)
require(tidyverse)
require(here)
require(mgcv)
require(visreg)
require(gratia)

### Workflow ###
fit <- FALSE
###
Spec <- "Chinook Salmon"
spec <- "chinook"

#Load data
dir.work <- here::here("data", "Chapter_2_RDS")
dir.dat <- here::here("data", "BASIS")
og <- readRDS(file=paste0(dir.dat, "/Ch2_dataframe.rds"))
dat <- og %>% filter(CommonName == Spec)
dat$Climate_stanza <- as.factor(dat$Climate_stanza)

# Save factor versions 
dat$fSampleYear <- as.factor(dat$SampleYear)
dat$Climate_stanza <- factor(dat$Climate_stanza)
dat$Even_odd_year <- factor(dat$Even_odd_year)

#### Global Model ####
if(fit == TRUE){
  ## Fit Model 1A, global model of in-situ covariates ##
 
  ## First testing whether covariates are nonlinear, via the edf
  mod1 <- gam(TotalCatchNum ~ te(EQ.Longitude,EQ.Latitude, bs = "tp", 
                                  k = 9, m = 1) + s(fSampleYear, bs = "re") +
                 offset(log(Effort_area_km2)) + 
                 te(surfacetemp, bs = "tp", k = 9, m = 1) + 
                 te(bottomsal, bs = "tp", k = 9, m = 1) +
                 te(MixedLayerDepth, bs = "tp", k = 9, m = 1),
               family = tw(link = "log"), method = "ML", data = dat)

  ## Notes from summary
  summary(mod1)
  # Edfs for all covariates are greater than 1. Keep smoothed model.
  AIC(mod1) 
  BIC(mod1)
  
  # Model with bottom salinity removed
  mod2 <- gam(TotalCatchNum ~ te(EQ.Longitude,EQ.Latitude, bs = "tp", 
                                 k = 9, m = 1) + s(fSampleYear, bs = "re") +
                offset(log(Effort_area_km2)) + 
                te(surfacetemp, bs = "tp", k = 9, m = 1) + 
                te(MixedLayerDepth, bs = "tp", k = 9, m = 1),
              family = tw(link = "log"), method = "ML", data = dat) 
  
  summary(mod2)
  AIC(mod2) 
  BIC(mod2)
  
  # First model is better. Save final in-situ model
  setwd(dir.work)
  mod1A <- mod1
  saveRDS(mod1A, paste0(spec,"_gam_mod1A.rds"))
  
} else{
  dir.dat<- here::here("data", "Chapter_2_RDS")
  setwd(dir.dat)
  mod1A <- readRDS(paste0(spec, "_gam_mod1A.rds"))
}

#### Model 2 - Cold Pool Extent ####
if(fit == TRUE){
  
  ## Fit Model 2A. Does CPE influence productivity? ##
  # Use best Model 1A (in-situ). 
  # Testing a smoothed CPE effect first resulted in CPE effect of 1.7.
  mod2A <- gam(TotalCatchNum ~ te(EQ.Longitude,EQ.Latitude, bs = "tp", 
                                 k = 9, m = 1) + s(fSampleYear, bs = "re") +
                offset(log(Effort_area_km2)) + 
                te(surfacetemp, bs = "tp", k = 9, m = 1) + 
                te(bottomsal, bs = "tp", k = 9, m = 1) +
                te(MixedLayerDepth, bs = "tp", k = 9, m = 1) +
                 te(CPE, bs = "tp", k = 9, m = 1),
              family = tw(link = "log"), method = "ML", data = dat)
  
  summary(mod2A)
  AIC(mod2A)
  BIC(mod2A)
  #Save to RDS
  setwd(dir.work)
  saveRDS(mod2A, paste0(spec,"_gam_mod2A.rds"))
  
  ## Fit Model 2B ##
  mod2B <- gam(TotalCatchNum ~ te(EQ.Longitude,EQ.Latitude, bs = "tp", 
                                  k = 9, m = 1) + s(fSampleYear, bs = "re") +
                 offset(log(Effort_area_km2)) + 
                 te(surfacetemp, bs = "tp", k = 9, m = 1) + 
                 te(bottomsal, bs = "tp", k = 9, m = 1) +
                 te(MixedLayerDepth, bs = "tp", k = 9, m = 1) +
                 te(CPE, bs = "tp", k = 9, m = 1) +
                 te(EQ.Longitude,EQ.Latitude, bs = "tp", k = 9, m = 1, by = CPE),
               family = tw(link = "log"), method = "ML", data = dat)
  
  summary(mod2B)
  AIC(mod2B)
  BIC(mod2B)
  saveRDS(mod2B, paste0(spec, "_gam_mod2B.rds"))
  
  ## Fit Model 2C ##
  mod2C <- gam(TotalCatchNum ~ te(EQ.Longitude,EQ.Latitude, bs = "tp", 
                                  k = 9, m = 1) + s(fSampleYear, bs = "re") +
                 offset(log(Effort_area_km2)) + 
                 te(CPE, bs = "tp", k = 9, m = 1) +
                 te(EQ.Longitude,EQ.Latitude, bs = "tp", k = 9, m = 1, by = CPE),
               family = tw(link = "log"), method = "ML", data = dat)
  
  summary(mod2C)
  AIC(mod2C)
  BIC(mod2C)
  saveRDS(mod2C, paste0(spec, "_gam_mod2C.rds"))
  
} else{
  dir.dat<- here::here("data", "Chapter_2_RDS")
  setwd(dir.dat)
  mod2A <- readRDS(paste0(spec, "_gam_mod2A.rds"))
  mod2B <- readRDS(paste0(spec, "_gam_mod2B.rds"))
  mod2C <- readRDS(paste0(spec, "_gam_mod2C.rds"))
}

#### Model 3 - Climate Stanza ####
if(fit == TRUE){
  
  ## Fit Model 3A. Does climate stanza shift distribution? ##
  # Use best Model 1A (in-situ). 
  mod3A <- gam(TotalCatchNum ~ te(EQ.Longitude,EQ.Latitude, bs = "tp", 
                                  k = 9, m = 1) + s(fSampleYear, bs = "re") +
                 offset(log(Effort_area_km2)) + 
                 te(surfacetemp, bs = "tp", k = 9, m = 1) + 
                 te(bottomsal, bs = "tp", k = 9, m = 1) +
                 te(MixedLayerDepth, bs = "tp", k = 9, m = 1) +
                 Climate_stanza,
               family = tw(link = "log"), method = "ML", data = dat)
  
  summary(mod3A)
  AIC(mod3A) 
  BIC(mod3A)
  #Save to RDS
  setwd(dir.work)
  saveRDS(mod3A, paste0(spec,"_gam_mod3A.rds"))
  
  ## Fit Model 3B ##
  mod3B <- gam(TotalCatchNum ~ te(EQ.Longitude,EQ.Latitude, bs = "tp", 
                                  k = 9, m = 1) + s(fSampleYear, bs = "re") +
                 offset(log(Effort_area_km2)) + 
                 te(surfacetemp, bs = "tp", k = 9, m = 1) + 
                 te(bottomsal, bs = "tp", k = 9, m = 1) +
                 te(MixedLayerDepth, bs = "tp", k = 9, m = 1) +
                 Climate_stanza +
                 te(EQ.Longitude,EQ.Latitude, bs = "tp", k = 9, m = 1, 
                    by = Climate_stanza),
               family = tw(link = "log"), method = "ML", data = dat)
  
  summary(mod3B)
  AIC(mod3B)
  BIC(mod3B) 
  saveRDS(mod3B, paste0(spec, "_gam_mod3B.rds"))
  
  ## Fit Model 3C ##
  mod3C <- gam(TotalCatchNum ~ te(EQ.Longitude,EQ.Latitude, bs = "tp", 
                                  k = 9, m = 1) + s(fSampleYear, bs = "re") +
                 offset(log(Effort_area_km2)) + 
                 Climate_stanza +
                 te(EQ.Longitude,EQ.Latitude, bs = "tp", k = 9, m = 1, 
                    by = Climate_stanza),
               family = tw(link = "log"), method = "ML", data = dat)
  
  summary(mod3C)
  AIC(mod3C)
  BIC(mod3C) 
  saveRDS(mod3C, paste0(spec, "_gam_mod3C.rds"))
  
} else{
  dir.dat<- here::here("data", "Chapter_2_RDS")
  setwd(dir.dat)
  mod3A <- readRDS(paste0(spec, "_gam_mod3A.rds"))
  mod3B <- readRDS(paste0(spec, "_gam_mod3B.rds"))
  mod3C <- readRDS(paste0(spec, "_gam_mod3C.rds"))
}

#### Model 4 - Even/Odd Year ####
if(fit == TRUE){
  
  ## Fit Model 4A. Does even/odd year shift distribution? ##
  # Use best Model 1A (in-situ). 
  mod4A <- gam(TotalCatchNum ~ te(EQ.Longitude,EQ.Latitude, bs = "tp", 
                                  k = 9, m = 1) + s(fSampleYear, bs = "re") +
                 offset(log(Effort_area_km2)) + 
                 te(surfacetemp, bs = "tp", k = 9, m = 1) + 
                 te(bottomsal, bs = "tp", k = 9, m = 1) +
                 te(MixedLayerDepth, bs = "tp", k = 9, m = 1) +
                 Even_odd_year,
               family = tw(link = "log"), method = "ML", data = dat)
  
  summary(mod4A)
  AIC(mod4A)
  BIC(mod4A) 
  #Save to RDS
  setwd(dir.work)
  saveRDS(mod4A, paste0(spec,"_gam_mod4A.rds"))
  
  ## Fit Model 4B ##
  mod4B <- gam(TotalCatchNum ~ te(EQ.Longitude,EQ.Latitude, bs = "tp", 
                                  k = 9, m = 1) + s(fSampleYear, bs = "re") +
                 offset(log(Effort_area_km2)) + 
                 te(surfacetemp, bs = "tp", k = 9, m = 1) + 
                 te(bottomsal, bs = "tp", k = 9, m = 1) +
                 te(MixedLayerDepth, bs = "tp", k = 9, m = 1) +
                 Even_odd_year +
                 te(EQ.Longitude,EQ.Latitude, bs = "tp", k = 9, m = 1, 
                    by = Even_odd_year),
               family = tw(link = "log"), method = "ML", data = dat)
  
  summary(mod4B)
  AIC(mod4B) 
  BIC(mod4B) 
  saveRDS(mod4B, paste0(spec, "_gam_mod4B.rds"))
  
  ## Fit Model 4C ##
  mod4C <- gam(TotalCatchNum ~ te(EQ.Longitude,EQ.Latitude, bs = "tp", 
                                  k = 9, m = 1) + s(fSampleYear, bs = "re") +
                 offset(log(Effort_area_km2)) + 
                Even_odd_year +
                 te(EQ.Longitude,EQ.Latitude, bs = "tp", k = 9, m = 1, 
                    by = Even_odd_year),
               family = tw(link = "log"), method = "ML", data = dat)
  
  summary(mod4C)
  AIC(mod4C)
  BIC(mod4C)
  saveRDS(mod4C, paste0(spec, "_gam_mod4C.rds"))
  
} else{
  dir.dat<- here::here("data", "Chapter_2_RDS")
  setwd(dir.dat)
  mod4A <- readRDS(paste0(spec, "_gam_mod4A.rds"))
  mod4B <- readRDS(paste0(spec, "_gam_mod4B.rds"))
  mod4C <- readRDS(paste0(spec, "_gam_mod4C.rds"))
}  