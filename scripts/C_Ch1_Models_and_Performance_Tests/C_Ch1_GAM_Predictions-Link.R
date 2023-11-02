### Chapter 1 GAM Predictions- Link response
## Author: Lilian Hart 
## Last edited: 11/01/23

require(tidyverse)
require(dplyr)
require(here)
require(mgcv)
require(visreg)
require(beepr)
library(sf)

### Workflow ###
Spec <- "Chum"
spec <- tolower(Spec)

fit <- TRUE

if(fit == TRUE){
  dir.work <- here("data", "Chapter_1_RDS")
  setwd(dir.work)
  og <- readRDS("basis_subset.rds")
  species <- paste(Spec, "Salmon")
  dat <- og %>% filter(CommonName == species) %>% drop_na(CommonName,
                                                          Effort_area_km2,
                                                          TotalCatchNum,
                                                          SampleYear)
  # Save factor versions of Sample Year 
  dat$fSampleYear <- as.factor(dat$SampleYear)
  
  # Set the max number of GAM iterations
  gam.control(maxit=3600)
  
  # Load prediction grid, save as dataframe.
  p_grid <- readRDS("gam_prediction_grid.rds")
  p_grid <- data.frame(p_grid)
  
  # Generate list of years
  years <- unique(dat$fSampleYear)
  num_years <- unique(dat$SampleYear)
  n_years <- 18
  
  # Load models
  mod1 <- readRDS(paste0(spec,"_gam_mod1.rds"))
  mod2 <- readRDS(paste0(spec,"_gam_mod2.rds"))
  mod3 <- readRDS(paste0(spec, "_gam_mod3.rds"))
  mod4 <- readRDS(paste0(spec, "_gam_mod4.rds"))
  
  ### Predict salmon densities ###
  
  ## Model 1 - Static model, long-term average spatial variation
  temp_pred <-  predict(mod1, 
                        newdata = data.frame(EQ.Longitude=p_grid$Lon, 
                                             EQ.Latitude=p_grid$Lat,
                                             Effort_area_km2 = 1), 
                        type = "link", se=TRUE)
  temp_df <- data.frame(temp_pred)
  
  colnames(temp_df) <- c("Fit", "SE.Fit")
  temp_df$low95 <- temp_pred$fit - 1.96*temp_pred$se.fit # Approximate lower bound of 95% CI
  temp_df$up95 <- temp_pred$fit + 1.96*temp_pred$se.fit # Approximate upper bound of 95% CI
  temp_df$Lon <- p_grid$Lon
  temp_df$Lat <- p_grid$Lat
  temp_df$CV <- temp_df$SE.Fit / (mean(temp_df$Fit))
  
  pred_1 <- temp_df
  saveRDS(pred_1, file.path(dir.work, paste0(spec,"_GAM_Mod1_Predictions_Link.rds")))
  
  ## Model 2 - Static model with interannual variability in abundance
  # Create array to hold predictions
  pred_2 <- data.frame()
  
  for(i in 1:n_years){
    temp_pred <-  predict(mod2, 
                          newdata = data.frame(EQ.Longitude = p_grid$Lon, 
                                               EQ.Latitude = p_grid$Lat, 
                                               fSampleYear = years[i],
                                               SampleYear = num_years[i],
                                               Effort_area_km2 = 1),
                          type = "link", se=TRUE)
    temp_df <- data.frame(temp_pred)
    colnames(temp_df) <- c("Fit", "SE.Fit")
    temp_df$Year <- years[i] # Year
    temp_df$Fit <- temp_pred$fit 
    temp_df$SE.Fit <- temp_pred$se.fit
    temp_df$low95 <- temp_pred$fit - 1.96*temp_pred$se.fit # Approximate lower bound of 95% CI
    temp_df$up95 <- temp_pred$fit + 1.96*temp_pred$se.fit # Approximate upper bound of 95% CI
    temp_df$Lon <- p_grid$Lon
    temp_df$Lat <- p_grid$Lat
    temp_df$CV <- temp_df$SE.Fit / (mean(temp_df$Fit))
    
    print(years[i])
    pred_2 <- rbind(pred_2, temp_df)
  } 
  saveRDS(pred_2, file.path(dir.work, paste0(spec,"_GAM_Mod2_Predictions_Link.rds")))
  
  ## Model 3 - Dynamic model with autocorrelated annual spatial fields
  # Create array to hold predictions
  pred_3 <- data.frame()
  
  for(i in 1:n_years){
    temp_pred <-  predict(mod3, 
                          newdata = data.frame(EQ.Longitude = p_grid$Lon, 
                                               EQ.Latitude = p_grid$Lat, 
                                               fSampleYear = years[i],
                                               SampleYear = num_years[i],
                                               Effort_area_km2 = 1),
                          type = "link", se=TRUE)
    temp_df <- data.frame(temp_pred)
    colnames(temp_df) <- c("Fit", "SE.Fit")
    temp_df$Year <- years[i] # Year
    temp_df$Fit <- temp_pred$fit 
    temp_df$SE.Fit <- temp_pred$se.fit
    temp_df$low95 <- temp_pred$fit - 1.96*temp_pred$se.fit # Approximate lower bound of 95% CI
    temp_df$up95 <- temp_pred$fit + 1.96*temp_pred$se.fit # Approximate upper bound of 95% CI
    temp_df$Lon <- p_grid$Lon
    temp_df$Lat <- p_grid$Lat
    temp_df$CV <- temp_df$SE.Fit / (mean(temp_df$Fit))
    
    print(years[i])
    pred_3 <- rbind(pred_3, temp_df)
  } 
  saveRDS(pred_3, file.path(dir.work, paste0(Spec,"_GAM_Mod3_Predictions_Link.rds")))
  
  ## Model 4 - Dynamic model with independent annual spatial fields
  # Create array to hold predictions
  pred_4 <- data.frame()

  for(i in 1:n_years){
    temp_pred <-  predict(mod4,
                          newdata = data.frame(EQ.Longitude = p_grid$Lon,
                                               EQ.Latitude = p_grid$Lat,
                                               fSampleYear = years[i],
                                               fSampleYear = years[i],
                                               Effort_area_km2 = 1),
                          type = "link", se=TRUE)

    temp_df <- data.frame(temp_pred)
    colnames(temp_df) <- c("Fit", "SE.Fit")
    temp_df$Year <- years[i] # Year
    temp_df$Fit <- temp_pred$fit
    temp_df$SE.Fit <- temp_pred$se.fit
    temp_df$low95 <- temp_pred$fit - 1.96*temp_pred$se.fit # Approximate lower bound of 95% CI
    temp_df$up95 <- temp_pred$fit + 1.96*temp_pred$se.fit # Approximate upper bound of 95% CI
    temp_df$Lon <- p_grid$Lon
    temp_df$Lat <- p_grid$Lat
    temp_df$CV <- temp_df$SE.Fit / (mean(temp_df$Fit))

    print(years[i])
    pred_4 <- rbind(pred_4, temp_df)
  }
  saveRDS(pred_4, file.path(dir.work,paste0(spec,"_GAM_Mod4_Predictions_Link.rds")))

  } else{
    dir.work <- here("data", "Chapter_1_RDS")
    setwd(dir.work)
    pred_1 <- readRDS(file.path(dir.work, paste0(spec, "_GAM_Mod1_Predictions_Link.rds"))) 
    pred_2 <- readRDS(file.path(dir.work, paste0(spec, "_GAM_Mod2_Predictions_Link.rds")))
    pred_3 <- readRDS(file.path(dir.work, paste0(spec, "_GAM_Mod3_Predictions_Link.rds")))
    pred_4 <- readRDS(file.path(dir.work, paste0(Spec, "_GAM_Mod4_Predictions_Link.rds")))
    }
