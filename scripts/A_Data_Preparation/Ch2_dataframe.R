### Chapter 2 Dataframe Building Script ###
# author: Lilian Hart
# date last modified: 05/17/23

require(dplyr)
require(tidyverse)
require(here)
require(coldpool)

# Load BASIS data
dir.dat <- here::here("data", "BASIS")
og <- readRDS(file.path(dir.dat, "V4_basis_subset.rds"))
# make copy
dat <- og

#### Add in Even/Odd year factor, and climate stanza from literature ####
dat$Even_odd_year <- NA
for (i in 1:11070){
  if((dat$SampleYear[i] %% 2) == 0) {
    dat$Even_odd_year[i] = "Even"
  } else{
    dat$Even_odd_year[i] = "Odd"
  }
}

# Source for more recent climate stanza info:
#https://apps-afsc.fisheries.noaa.gov/REFM/docs/2022/EBS-ESR-Brief.pdf
# Extended warm phase in the EBS ended, with average conditions since fall 2021
# So 2019 would be still in the warm climate stanza.
# Yasumiishi et al. 2020: Warm 1 (2002-2005), Warm 2 (2014-2019), Cool (2006-2013)
dat$Climate_stanza <- NA
cools <- c(2006,2007,2008,2009,2010,2011,2012,2013)
warm1 <- c(2002,2003,2004,2005)
for (i in 1:11070){
  if(dat$SampleYear[i] %in% cools){
    dat$Climate_stanza[i] = "Cool"
  } else{
    if(dat$SampleYear[i] %in% warm1){
      dat$Climate_stanza[i] = "Warm1"
    } else{
        dat$Climate_stanza[i] = "Warm2"
      }
}}

#### Add cold pool extent index from Alaska Fisheries Science Center ####
# https://github.com/afsc-gap-products/coldpool

#devtools::install_github("sean-rohan-NOAA/akgfmaps", build_vignettes = TRUE)
#devtools::install_github("afsc-gap-products/coldpool")
# AREA_LTE2_KM2 is the Cold Pool Index. Total area with bottom temperatures less
# than or equal to 2 celsius, in square kilometers.
cpi <- coldpool::cold_pool_index
cpi <- as.data.frame(cpi)
cpi$SampleYear <- cpi$YEAR
dat2 <- left_join(x = dat, y = cpi, unmatched = "drop")

# Make slimmer dataframe
dat2 <- dat2 %>% filter(CommonName != "Pollock") #Back down to 9,225
dat2$CPE <- dat2$AREA_LTE2_KM2
dat <- dat2 %>% dplyr::select(SampleYear, EQ.Longitude, EQ.Latitude, 
                                Effort_area_km2,CommonName,TotalCatchNum,
                              surfacetemp, bottomsal,CPE, MixedLayerDepth, 
                                Climate_stanza, Even_odd_year)
# Save as RDS
saveRDS(dat, file=paste0(dir.dat, "/Ch2_dataframe.rds"))
# Just environmental covariates for corr matrix
enviro <- dat %>% dplyr::select(surfacetemp, bottomsal, MixedLayerDepth, CPE,
                                Climate_stanza, Even_odd_year)
saveRDS(enviro, file=paste0(dir.dat, "/Ch2_enviro_covariates.rds"))
