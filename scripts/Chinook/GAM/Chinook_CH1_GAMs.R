### Chinook Chapter 1 GAMs, fourth draft
## author: Lilian Hart
## Last edited: 02/07/2023
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
fit <- FALSE
if(fit == TRUE){
  dir.work <- here("data", "Chapter_1_RDSModels")
  og <- readRDS(file.path(dir.work,"V4_basis_subset.rds"))
  setwd(dir.work)
  
  species <- "Chinook Salmon"
  
  dat <- og %>% filter(CommonName == species) %>% drop_na(CommonName,
                                                          Effort_area_km2,
                                                          TotalCatchNum)
  # Save factor versions of Sample Year 
  dat$fSampleYear <- as.factor(dat$SampleYear)
  
  #### Four Abundance models - Chinook ####
  ## Model 1: Average spatial field 
  mod1 <- gam(TotalCatchNum ~ te(EQ.Longitude,EQ.Latitude, bs = "tp", 
                                 k = 9, m = 1) +
                offset(log(Effort_area_km2)), 
              family = tw(link = "log"), data = dat) 
  saveRDS(mod1, "chinook_gam_mod1.rds")
  summary(mod1)             
  
  ## Model 2: Average spatial field with yearly changes in abundance
  mod2 <- gam(TotalCatchNum ~ te(EQ.Longitude,EQ.Latitude, bs = "tp", k = 9, 
                                 m = 1) +
                fSampleYear + offset(log(Effort_area_km2)),
              family=tw(link = "log"), data = dat)
  saveRDS(mod2, "chinook_gam_mod2.rds")
  summary(mod2)
  
  # Model 3: Spatiotemporal model with factor year effect
  # Error message:
  # Model has more coefficients than data
  # Try reducing k to 7.
  # Google GAM and error message.
  # mod3 <- gam(TotalCatchNum ~ te(EQ.Longitude,EQ.Latitude, bs = "tp", k = 9) +
  #              fSampleYear +
  #               te(EQ.Longitude,EQ.Latitude, by = fSampleYear, bs = "tp", k = 9) +
  #               offset(log(Effort_area_km2)),
  #             family=tw(link = "log"), data = dat); beep(sound=8)
  # saveRDS(mod3, "chinook_gam_mod3.rds")

  ## Model 4: Spatiotemporal model with continuous year effect
  mod4 <- gam(TotalCatchNum  ~ te(EQ.Longitude,EQ.Latitude, bs = "tp", k = 9,
                                  m = 1) +
                fSampleYear + 
                te(EQ.Longitude,EQ.Latitude,SampleYear, d=c(2,1), bs=c("tp","cr"),
                   k = 9, m = 1) + offset(log(Effort_area_km2)),
              family=tw(link = "log"), data = dat)
  saveRDS(mod4, "chinook_gam_mod4.rds")
  summary(mod4)

} else {
  print("Models are already fitted :)")
  
  dir.data <- here("data", "BASIS")
  og <- readRDS(file.path(dir.work,"V4_basis_subset.rds"))
  dir.work <- here("data", "Chapter_1_RDSModels")
  setwd(dir.work)
  
  species <- "Chinook Salmon"
  
  dat <- og %>% filter(CommonName == species) %>% drop_na(CommonName,
                                                          Effort_area_km2,
                                                          TotalCatchNum)
  # Save factor versions of Sample Year 
  dat$fSampleYear <- as.factor(dat$SampleYear)
  
  mod1 <- readRDS("chinook_gam_mod1.rds")
  mod2 <- readRDS("chinook_gam_mod2.rds")
  # mod3 <- readRDS("chinook_gam_mod3.rds")
  mod4 <- readRDS("chinook_gam_mod4.rds")
}

# Diagnostics
#visreg()
#gam.check()

## Save residuals, then map them ##

mod1resids <- mod1$residuals
resids_map1 <- ggplot(data = dat, aes(x=EQ.Longitude,y=EQ.Latitude,color=mod1resids)) +
  geom_point() + 
  facet_wrap(~SampleYear, ncol = 7) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Model 1 Residual Map")
resids_map1 + scale_color_gradient2(low="#2986cc", mid = "white", 
                                    high="#cc0000", midpoint=0, 
                                    name = "Residual value")
mod2resids <- mod2$residuals
resids_map2 <- ggplot(data = dat, aes(x=EQ.Longitude,y=EQ.Latitude,color=mod2resids)) +
  geom_point() + 
  facet_wrap(~SampleYear, ncol = 7) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Model 2 Residual Map")
resids_map2 + scale_color_gradient2(low="#2986cc", mid = "white", 
                                    high="#cc0000", midpoint=0, 
                                    name = "Residual value")
#mod3resids <- mod3$residuals
# resids_map3 <- ggplot(data = dat, aes(x=EQ.Longitude,y=EQ.Latitude,color=mod3resids)) +
# geom_point() +
# facet_wrap(~SampleYear, ncol = 7) +
# xlab("Longitude") + ylab("Latitude") +
# ggtitle("Model 3 Residual Map")
# resids_map3 + scale_color_gradient2(low="#2986cc", mid = "white", 
# high="#cc0000", midpoint=0, 
# name = "Residual value")

mod4resids <- mod4$residuals
resids_map4 <- ggplot(data = dat, aes(x=EQ.Longitude,y=EQ.Latitude,color=mod4resids)) +
  geom_point() + 
  facet_wrap(~SampleYear, ncol = 7) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Model 4 Residual Map")
resids_map4 + scale_color_gradient2(low="#2986cc", mid = "white", 
                                    high="#cc0000", midpoint=0, 
                                    name = "Residual value")


