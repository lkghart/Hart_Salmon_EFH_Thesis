### Chapter 2 Year Effect Tests ###
# author: Lilian Hart
# date last modified: 06/21/23

require(dplyr)
require(tidyverse)
require(here)
require(mgcv)
require(ggplot2)

# Load data
dir.dat <- here::here("data", "BASIS")
dir.mods <- here::here("data", "Chapter_1_RDSModels")
dir.out <- here::here("data", "Chapter_1_RDS")
dat <- readRDS(file=paste0(dir.dat, "/Ch2_dataframe.rds"))
dat$fSampleYear <- as.factor(dat$SampleYear)

#Ch1 dataframe
og <- readRDS(file.path(dir.dat,"V4_basis_subset.rds"))
og$fSampleYear <- as.factor(og$SampleYear)

# Load Ch1 models 
mod2_chi <- readRDS(file.path(dir.mods,"chinook_gam_mod2.rds"))
mod2_chu <- readRDS(file.path(dir.mods, "chum_gam_mod2.rds"))
mod2_pin <- readRDS(file.path(dir.mods, "pink_gam_mod2.rds"))
mod2_soc <- readRDS(file.path(dir.mods, "sockeye_gam_mod2.rds"))

# Subset data by species
chinook <- dat %>% filter(CommonName == "Chinook Salmon") %>% 
  drop_na(CommonName, Effort_area_km2,TotalCatchNum)
og_chinook <- og %>% filter(CommonName == "Chinook Salmon") %>% 
  drop_na(CommonName, Effort_area_km2,TotalCatchNum)
chum <- dat %>% filter(CommonName == "Chum Salmon") %>% 
  drop_na(CommonName, Effort_area_km2,TotalCatchNum)
og_chum <- og %>% filter(CommonName == "Chum Salmon") %>% 
  drop_na(CommonName, Effort_area_km2,TotalCatchNum)
pink <- dat %>% filter(CommonName == "Pink Salmon") %>% 
  drop_na(CommonName, Effort_area_km2,TotalCatchNum)
og_pink <- og %>% filter(CommonName == "Pink Salmon") %>% 
  drop_na(CommonName, Effort_area_km2,TotalCatchNum)
sockeye <- dat %>% filter(CommonName == "Sockeye Salmon") %>% 
  drop_na(CommonName, Effort_area_km2,TotalCatchNum)
og_sockeye <- og %>% filter(CommonName == "Sockeye Salmon") %>% 
  drop_na(CommonName, Effort_area_km2,TotalCatchNum)

## Fit Model 0A: like Ch1 Mod 2, but with random year effects. ##
# Chinook
mod0A_chinook <- gam(TotalCatchNum ~ te(EQ.Longitude,EQ.Latitude, bs = "tp", k = 9, 
                               m = 1) + s(fSampleYear, bs = "re") + 
                       offset(log(Effort_area_km2)),
                     method = "ML", family=tw(link = "log"), data = chinook)

mod0A_chum <- gam(TotalCatchNum ~ te(EQ.Longitude,EQ.Latitude, bs = "tp", k = 9, 
                                        m = 1) + s(fSampleYear, bs = "re") + 
                       offset(log(Effort_area_km2)),
                  method = "ML", family=tw(link = "log"), data = chum)

mod0A_pink <- gam(TotalCatchNum ~ te(EQ.Longitude,EQ.Latitude, bs = "tp", k = 9, 
                                        m = 1) + s(fSampleYear, bs = "re") + 
                       offset(log(Effort_area_km2)),
                  method = "ML", family=tw(link = "log"), data = pink)

mod0A_sockeye <- gam(TotalCatchNum ~ te(EQ.Longitude,EQ.Latitude, bs = "tp", k = 9, 
                                        m = 1) + s(fSampleYear, bs = "re") + 
                       offset(log(Effort_area_km2)),
                     method = "ML", family=tw(link = "log"), data = sockeye)

## Residual maps ##

# Pull Ch1 Mod2 residuals
mod2_chi_resids <- mod2_chi$residuals
mod2_chu_resids <- mod2_chu$residuals
mod2_pin_resids <- mod2_pin$residuals
mod2_soc_resids <- mod2_soc$residuals

# Filter out zero catch records from Mod2 and save to dataframes
a <- which(chinook$TotalCatchNum>0)
b <- mod2_chi_resids[a]
c <- chinook$fSampleYear[a]
chi_mod2 <- data.frame(residual = b, year = c)

a <- which(chum$TotalCatchNum>0)
b <- mod2_chu_resids[a]
c <- chum$SampleYear[a]
chu_mod2 <- data.frame(residual = b, year = c)

a <- which(pink$TotalCatchNum>0)
b <- mod2_pin_resids[a]
c <- pink$SampleYear[a]
pin_mod2 <- data.frame(residual = b, year = c)

a <- which(sockeye$TotalCatchNum>0)
b <- mod2_soc_resids[a]
c <- sockeye$SampleYear[a]
soc_mod2 <- data.frame(residual = b, year = c)

# Filter out zero catch records from Random model and save to dataframes
a <- which(chinook$TotalCatchNum>0)
b <- mod0A_chinook$residuals[a]
c <- chinook$SampleYear[a]
chi_mod0 <- data.frame(residual = b, year = c)

a <- which(chum$TotalCatchNum>0)
b <- mod0A_chum$residuals[a]
c <- chum$SampleYear[a]
chu_mod0 <- data.frame(residual = b, year = c)

a <- which(pink$TotalCatchNum>0)
b <- mod0A_pink$residuals[a]
c <- pink$SampleYear[a]
pin_mod0 <- data.frame(residual = b, year = c)

a <- which(sockeye$TotalCatchNum>0)
b <- mod0A_sockeye$residuals[a]
c <- sockeye$SampleYear[a]
soc_mod0 <- data.frame(residual = b, year = c)

## Plot factor vs random year residuals
#Chinook 
ggplot(chi_mod2) + 
  geom_boxplot(aes(x = year, y = residual, group = year)) +
  ggtitle("Chinook factor year effect model")

ggplot(chi_mod0) + 
  geom_boxplot(aes(x = year, y = residual, group = year)) +
  ggtitle("Chinook random year effect model")

#Chum 
ggplot(chu_mod2) + 
  geom_boxplot(aes(x = year, y = residual, group = year)) +
  ggtitle("Chum factor year effect model")

ggplot(chu_mod0) + 
  geom_boxplot(aes(x = year, y = residual, group = year)) +
  ggtitle("Chum random year effect model")

#Pink
ggplot(pin_mod2) + 
  geom_boxplot(aes(x = year, y = residual, group = year)) +
  ggtitle("Pink factor year effect model")

ggplot(pin_mod0) + 
  geom_boxplot(aes(x = year, y = residual, group = year)) +
  ggtitle("Pink random year effect model")

#Sockeye
ggplot(soc_mod2) + 
  geom_boxplot(aes(x = year, y = residual, group = year)) +
  ggtitle("Sockeye factor year effect model")

ggplot(soc_mod0) + 
  geom_boxplot(aes(x = year, y = residual, group = year)) +
  ggtitle("Sockeye random year effect model")
