### Chapter 2 GAM Predictions (in response space) 
## Author: Lilian Hart 
## Last edited: 11/06/23

require(tidyverse)
require(dplyr)
require(here)
require(mgcv)

### Load Data ###
Spec <- "Chum"
spec <- tolower(Spec)

dir.work <- here("data", "Chapter_2_RDS")
setwd(dir.work)
og <- readRDS("Ch2_dataframe.rds")
og$fSampleYear <- as.factor(og$SampleYear)

species <- paste(Spec,"Salmon")
dat <- og %>% filter(CommonName == species)
# Save factor versions 
dat$fSampleYear <- as.factor(dat$SampleYear)
dat$Climate_stanza <- factor(dat$Climate_stanza)
dat$Even_odd_year <- factor(dat$Even_odd_year)

### Workflow ###
fit <- TRUE

#### Model Fitting and Prediction ####
if(fit == TRUE){
  ## Controls ##
  # Set the max number of GAM iterations
  gam.control(maxit=3600)
  
  # Load prediction grid, save as dataframe.
  p_grid <- readRDS(file.path(dir.work,"gam_prediction_grid.rds"))
  p_grid <- data.frame(p_grid)
  
  ## Model Inputs ##
  # Generate list of years
  years <- unique(dat$fSampleYear)
  num_years <- unique(dat$SampleYear)
  n_years <- 18
  
  # Get averages of in-situ data #
  (m_MLD <- mean(og$MixedLayerDepth, na.rm = TRUE)) #19.4
  (m_SST <- mean(og$surfacetemp, na.rm = TRUE)) # 9.07
  (m_bsal <- mean(og$bottomsal, na.rm = TRUE)) #31.465
  
  ### Fit best-performing in-situ model using REML ###
  mod1A <- gam(TotalCatchNum ~ te(EQ.Longitude,EQ.Latitude, bs = "tp", 
                                 k = 9, m = 1) + s(fSampleYear, bs = "re") +
                offset(log(Effort_area_km2)) + 
                te(surfacetemp, bs = "tp", k = 9, m = 1) + 
                te(bottomsal, bs = "tp", k = 9, m = 1) +
                te(MixedLayerDepth, bs = "tp", k = 9, m = 1),
              family = tw(link = "log"), method = "REML", data = dat) 
  
  
  # In-Situ Model Prediction #
  temp_pred <-  predict(mod1A, 
                        newdata = data.frame(EQ.Longitude=p_grid$Lon, 
                                             EQ.Latitude=p_grid$Lat,
                                             Effort_area_km2 = 1,
                                             MixedLayerDepth = m_MLD,
                                             surfacetemp = m_SST,
                                             bottomsal = m_bsal,
                                             fSampleYear = "2018"),
                        type = "response", se=TRUE,
                        exclude = "s(fSampleYear)")
  temp_df <- data.frame(temp_pred)
  
  temp_df$low95 <- temp_pred$fit - 1.96*temp_pred$se.fit # Approximate lower bound of 95% CI
  temp_df$up95 <- temp_pred$fit + 1.96*temp_pred$se.fit # Approximate upper bound of 95% CI
  temp_df$Lon <- p_grid$Lon
  temp_df$Lat <- p_grid$Lat
  temp_df$CV <- temp_df$se.fit / (mean(temp_df$fit))
  
  pred_1 <- temp_df
  saveRDS(pred_1, file.path(dir.work, paste0(spec,"_gam_in-situ_predictions.rds")))
  
  # In-Situ Uncertainty #
  temp_pred <-  predict(mod1A, 
                        newdata = data.frame(EQ.Longitude=p_grid$Lon, 
                                             EQ.Latitude=p_grid$Lat,
                                             Effort_area_km2 = 1,
                                             MixedLayerDepth = m_MLD,
                                             surfacetemp = m_SST,
                                             bottomsal = m_bsal,
                                             fSampleYear = "2018"),
                        type = "link", se=TRUE,
                        exclude = "s(fSampleYear)")
  temp_df <- data.frame(temp_pred)
  
  temp_df$low95 <- temp_pred$fit - 1.96*temp_pred$se.fit # Approximate lower bound of 95% CI
  temp_df$up95 <- temp_pred$fit + 1.96*temp_pred$se.fit # Approximate upper bound of 95% CI
  temp_df$Lon <- p_grid$Lon
  temp_df$Lat <- p_grid$Lat
  temp_df$CV <- temp_df$se.fit / (mean(temp_df$fit))
  
  pred_1 <- temp_df
  saveRDS(pred_1, file.path(dir.work, paste0(spec,"_gam_in-situ_link_predictions.rds")))
  
  
  ### Fit best-performing CPE model with REML ###
  mod2B <- gam(TotalCatchNum ~ te(EQ.Longitude,EQ.Latitude, bs = "tp", 
                                  k = 9, m = 1) + s(fSampleYear, bs = "re") +
                 offset(log(Effort_area_km2)) + 
                 te(surfacetemp, bs = "tp", k = 9, m = 1) + 
                 te(bottomsal, bs = "tp", k = 9, m = 1) +
                 te(MixedLayerDepth, bs = "tp", k = 9, m = 1) +
                 te(CPE, bs = "tp", k = 9, m = 1) +
                 te(EQ.Longitude,EQ.Latitude, bs = "tp", k = 9, m = 1, by = CPE),
               family = tw(link = "log"), method = "REML", data = dat)
  
  # Determine the breaks to use for Cold Pool Extent maps
  # Mean plus or minus one standard deviation.
  (median <- median(og$CPE, na.rm = TRUE)) #167,875
  (stddev <- sd(og$CPE, na.rm = TRUE)) #106,960.2
  mean <- mean(og$CPE, na.rm = TRUE)
  (low <- mean - stddev) #71,503
  (high <- mean + stddev) #285,424
  
  ## CPE Uncertainty (Median CPE) ##
  temp_pred <-  predict(mod2B, 
                        newdata = data.frame(EQ.Longitude=p_grid$Lon, 
                                             EQ.Latitude=p_grid$Lat,
                                             Effort_area_km2 = 1,
                                             MixedLayerDepth = m_MLD,
                                             surfacetemp = m_SST,
                                             bottomsal = m_bsal,
                                             fSampleYear = "2018",
                                             CPE = median),
                                             type = "link", se=TRUE,
                        exclude = "s(fSampleYear)")
  temp_df <- data.frame(temp_pred)
  
  temp_df$low95 <- temp_pred$fit - 1.96*temp_pred$se.fit # Approximate lower bound of 95% CI
  temp_df$up95 <- temp_pred$fit + 1.96*temp_pred$se.fit # Approximate upper bound of 95% CI
  temp_df$Lon <- p_grid$Lon
  temp_df$Lat <- p_grid$Lat
  temp_df$CV <- temp_df$se.fit / (mean(temp_df$fit))
  
  pred_1 <- temp_df
  saveRDS(pred_1, file.path(dir.work, paste0(spec,"_gam_CPE-median_link_predictions.rds")))
  
  ## Low CPE Prediction ##
  temp_pred <-  predict(mod2B, 
                        newdata = data.frame(EQ.Longitude=p_grid$Lon, 
                                             EQ.Latitude=p_grid$Lat,
                                             Effort_area_km2 = 1,
                                             MixedLayerDepth = m_MLD,
                                             surfacetemp = m_SST,
                                             bottomsal = m_bsal,
                                             fSampleYear = "2018",
                                             CPE = low),
                        type = "response", se=TRUE,
                        exclude = "s(fSampleYear)")
  temp_df <- data.frame(temp_pred)
  
  temp_df$low95 <- temp_pred$fit - 1.96*temp_pred$se.fit # Approximate lower bound of 95% CI
  temp_df$up95 <- temp_pred$fit + 1.96*temp_pred$se.fit # Approximate upper bound of 95% CI
  temp_df$Lon <- p_grid$Lon
  temp_df$Lat <- p_grid$Lat
  temp_df$CV <- temp_df$se.fit / (mean(temp_df$fit))
  
  pred_1 <- temp_df
  saveRDS(pred_1, file.path(dir.work, paste0(spec,"_gam_CPE-low_predictions.rds")))
  
  ## Median CPE Prediction ##
  temp_pred <-  predict(mod2B, 
                        newdata = data.frame(EQ.Longitude=p_grid$Lon, 
                                             EQ.Latitude=p_grid$Lat,
                                             Effort_area_km2 = 1,
                                             MixedLayerDepth = m_MLD,
                                             surfacetemp = m_SST,
                                             bottomsal = m_bsal,
                                             fSampleYear = 2018,
                                             CPE = median),
                        type = "response", se=TRUE,
                        exclude = "s(fSampleYear)")
  temp_df <- data.frame(temp_pred)
  
  temp_df$low95 <- temp_pred$fit - 1.96*temp_pred$se.fit # Approximate lower bound of 95% CI
  temp_df$up95 <- temp_pred$fit + 1.96*temp_pred$se.fit # Approximate upper bound of 95% CI
  temp_df$Lon <- p_grid$Lon
  temp_df$Lat <- p_grid$Lat
  temp_df$CV <- temp_df$se.fit / (mean(temp_df$fit))
  
  pred_1 <- temp_df
  saveRDS(pred_1, file.path(dir.work, paste0(spec,"_gam_CPE-median_predictions.rds")))
  
  ## High CPE Prediction ##
  temp_pred <-  predict(mod2B, 
                        newdata = data.frame(EQ.Longitude=p_grid$Lon, 
                                             EQ.Latitude=p_grid$Lat,
                                             Effort_area_km2 = 1,
                                             MixedLayerDepth = m_MLD,
                                             surfacetemp = m_SST,
                                             bottomsal = m_bsal,
                                             fSampleYear = 2018,
                                             CPE = high),
                        type = "response", se=TRUE,
                        exclude = "s(fSampleYear)")
  temp_df <- data.frame(temp_pred)
  
  temp_df$low95 <- temp_pred$fit - 1.96*temp_pred$se.fit # Approximate lower bound of 95% CI
  temp_df$up95 <- temp_pred$fit + 1.96*temp_pred$se.fit # Approximate upper bound of 95% CI
  temp_df$Lon <- p_grid$Lon
  temp_df$Lat <- p_grid$Lat
  temp_df$CV <- temp_df$se.fit / (mean(temp_df$fit))
  
  pred_1 <- temp_df
  saveRDS(pred_1, file.path(dir.work, paste0(spec,"_gam_CPE-high_predictions.rds")))
  
  ### Fit best-performing Climate Stanza model with REML ###
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
               family = tw(link = "log"), method = "REML", data = dat)
  
  ## Climate Stanza Uncertainty (Warm1) ##
  temp_pred <-  predict(mod3B, 
                        newdata = data.frame(EQ.Longitude=p_grid$Lon, 
                                             EQ.Latitude=p_grid$Lat,
                                             Effort_area_km2 = 1,
                                             MixedLayerDepth = m_MLD,
                                             surfacetemp = m_SST,
                                             bottomsal = m_bsal,
                                             fSampleYear = "2018",
                                             Climate_stanza = "Warm1"),
                        type = "link", se=TRUE,
                        exclude = "s(fSampleYear)")
  temp_df <- data.frame(temp_pred)
  
  temp_df$low95 <- temp_pred$fit - 1.96*temp_pred$se.fit # Approximate lower bound of 95% CI
  temp_df$up95 <- temp_pred$fit + 1.96*temp_pred$se.fit # Approximate upper bound of 95% CI
  temp_df$Lon <- p_grid$Lon
  temp_df$Lat <- p_grid$Lat
  temp_df$CV <- temp_df$se.fit / (mean(temp_df$fit))
  
  pred_1 <- temp_df
  saveRDS(pred_1, file.path(dir.work, paste0(spec,"_gam_Warm1_link_predictions.rds")))
  
  
  ## Warm1 (2002-2005) Climate Stanza Prediction ##
  temp_pred <-  predict(mod3B, 
                        newdata = data.frame(EQ.Longitude=p_grid$Lon, 
                                             EQ.Latitude=p_grid$Lat,
                                             Effort_area_km2 = 1,
                                             MixedLayerDepth = m_MLD,
                                             surfacetemp = m_SST,
                                             bottomsal = m_bsal,
                                             fSampleYear = "2018",
                                             Climate_stanza = "Warm1"),
                        type = "response", se=TRUE,
                        exclude = "s(fSampleYear)")
  temp_df <- data.frame(temp_pred)
  
  temp_df$low95 <- temp_pred$fit - 1.96*temp_pred$se.fit # Approximate lower bound of 95% CI
  temp_df$up95 <- temp_pred$fit + 1.96*temp_pred$se.fit # Approximate upper bound of 95% CI
  temp_df$Lon <- p_grid$Lon
  temp_df$Lat <- p_grid$Lat
  temp_df$CV <- temp_df$se.fit / (mean(temp_df$fit))
  
  pred_1 <- temp_df
  saveRDS(pred_1, file.path(dir.work, paste0(spec,"_gam_Warm1_predictions.rds")))
  
  ## Cool Climate Stanza Prediction (2006-2013) ##
  temp_pred <-  predict(mod3B, 
                        newdata = data.frame(EQ.Longitude=p_grid$Lon, 
                                             EQ.Latitude=p_grid$Lat,
                                             Effort_area_km2 = 1,
                                             MixedLayerDepth = m_MLD,
                                             surfacetemp = m_SST,
                                             bottomsal = m_bsal,
                                             fSampleYear = "2018",
                                             Climate_stanza = "Cool"),
                        type = "response", se=TRUE,
                        exclude = "s(fSampleYear)")
  temp_df <- data.frame(temp_pred)
  
  temp_df$low95 <- temp_pred$fit - 1.96*temp_pred$se.fit # Approximate lower bound of 95% CI
  temp_df$up95 <- temp_pred$fit + 1.96*temp_pred$se.fit # Approximate upper bound of 95% CI
  temp_df$Lon <- p_grid$Lon
  temp_df$Lat <- p_grid$Lat
  temp_df$CV <- temp_df$se.fit / (mean(temp_df$fit))
  
  pred_1 <- temp_df
  saveRDS(pred_1, file.path(dir.work, paste0(spec,"_gam_Cool_predictions.rds")))
  
  ## Warm2 Climate Stanza Prediction (2014-2019) ##
  temp_pred <-  predict(mod3B, 
                        newdata = data.frame(EQ.Longitude=p_grid$Lon, 
                                             EQ.Latitude=p_grid$Lat,
                                             Effort_area_km2 = 1,
                                             MixedLayerDepth = m_MLD,
                                             surfacetemp = m_SST,
                                             bottomsal = m_bsal,
                                             fSampleYear = "2018",
                                             Climate_stanza = "Warm2"),
                        type = "response", se=TRUE,
                        exclude = "s(fSampleYear)")
  temp_df <- data.frame(temp_pred)
  
  temp_df$low95 <- temp_pred$fit - 1.96*temp_pred$se.fit # Approximate lower bound of 95% CI
  temp_df$up95 <- temp_pred$fit + 1.96*temp_pred$se.fit # Approximate upper bound of 95% CI
  temp_df$Lon <- p_grid$Lon
  temp_df$Lat <- p_grid$Lat
  temp_df$CV <- temp_df$se.fit / (mean(temp_df$fit))
  
  pred_1 <- temp_df
  saveRDS(pred_1, file.path(dir.work, paste0(spec,"_gam_Warm2_predictions.rds")))
  
  ### Fit Best Even/Odd Year Model with REML ###
  # AIC favored model 4B. 
  mod4B <- gam(TotalCatchNum ~ te(EQ.Longitude,EQ.Latitude, bs = "tp", 
                                  k = 9, m = 1) + s(fSampleYear, bs = "re") +
                 offset(log(Effort_area_km2)) + 
                 te(surfacetemp, bs = "tp", k = 9, m = 1) + 
                 te(bottomsal, bs = "tp", k = 9, m = 1) +
                 te(MixedLayerDepth, bs = "tp", k = 9, m = 1) +
                 Even_odd_year +
                 te(EQ.Longitude,EQ.Latitude, bs = "tp", k = 9, m = 1, 
                    by = Even_odd_year),
               family = tw(link = "log"), method = "REML", data = dat)
  
  ## Even/Odd year uncertainty (even) ##
  temp_pred <-  predict(mod4B, 
                        newdata = data.frame(EQ.Longitude=p_grid$Lon, 
                                             EQ.Latitude=p_grid$Lat,
                                             Effort_area_km2 = 1,
                                             MixedLayerDepth = m_MLD,
                                             surfacetemp = m_SST,
                                             bottomsal = m_bsal,
                                             fSampleYear = 2018,
                                             Even_odd_year = "Even"),
                        type = "link", se=TRUE,
                        exclude = "s(fSampleYear)")
  temp_df <- data.frame(temp_pred)
  
  temp_df$low95 <- temp_pred$fit - 1.96*temp_pred$se.fit # Approximate lower bound of 95% CI
  temp_df$up95 <- temp_pred$fit + 1.96*temp_pred$se.fit # Approximate upper bound of 95% CI
  temp_df$Lon <- p_grid$Lon
  temp_df$Lat <- p_grid$Lat
  temp_df$CV <- temp_df$se.fit / (mean(temp_df$fit))
  
  pred_1 <- temp_df
  saveRDS(pred_1, file.path(dir.work, paste0(spec,"_gam_Even-year_link_predictions.rds")))
  
  
  ## Even Year prediction
  temp_pred <-  predict(mod4B, 
                        newdata = data.frame(EQ.Longitude=p_grid$Lon, 
                                             EQ.Latitude=p_grid$Lat,
                                             Effort_area_km2 = 1,
                                             MixedLayerDepth = m_MLD,
                                             surfacetemp = m_SST,
                                             bottomsal = m_bsal,
                                             fSampleYear = 2018,
                                             Even_odd_year = "Even"),
                        type = "response", se=TRUE,
                        exclude = "s(fSampleYear)")
  temp_df <- data.frame(temp_pred)
  
  temp_df$low95 <- temp_pred$fit - 1.96*temp_pred$se.fit # Approximate lower bound of 95% CI
  temp_df$up95 <- temp_pred$fit + 1.96*temp_pred$se.fit # Approximate upper bound of 95% CI
  temp_df$Lon <- p_grid$Lon
  temp_df$Lat <- p_grid$Lat
  temp_df$CV <- temp_df$se.fit / (mean(temp_df$fit))
  
  pred_1 <- temp_df
  saveRDS(pred_1, file.path(dir.work, paste0(spec,"_gam_Even-year_predictions.rds")))
  
  ## Odd year prediction ##
  temp_pred <-  predict(mod4B, 
                        newdata = data.frame(EQ.Longitude=p_grid$Lon, 
                                             EQ.Latitude=p_grid$Lat,
                                             Effort_area_km2 = 1,
                                             MixedLayerDepth = m_MLD,
                                             surfacetemp = m_SST,
                                             bottomsal = m_bsal,
                                             fSampleYear = 2018,
                                             Even_odd_year = "Odd"),
                        type = "response", se=TRUE,
                        exclude = "s(fSampleYear)")
  temp_df <- data.frame(temp_pred)
  
  temp_df$low95 <- temp_pred$fit - 1.96*temp_pred$se.fit # Approximate lower bound of 95% CI
  temp_df$up95 <- temp_pred$fit + 1.96*temp_pred$se.fit # Approximate upper bound of 95% CI
  temp_df$Lon <- p_grid$Lon
  temp_df$Lat <- p_grid$Lat
  temp_df$CV <- temp_df$se.fit / (mean(temp_df$fit))
  
  pred_1 <- temp_df
  saveRDS(pred_1, file.path(dir.work, paste0(spec,"_gam_Odd-year_predictions.rds")))
  
  } else{
    in_situ <- readRDS(file.path(dir.work, paste0(spec,"_gam_in-situ_predictions.rds")))
    in_situ_CV <- readRDS(file.path(dir.work, paste0(spec,"_gam_in-situ_link_predictions.rds")))
    CPE_low <- readRDS(file.path(dir.work, paste0(spec, "_gam_CPE-low_predictions.rds"))) 
    CPE_med <- readRDS(file.path(dir.work, paste0(spec, "_gam_CPE-median_predictions.rds")))
    CPE_high <- readRDS(file.path(dir.work, paste0(spec, "_gam_CPE-high_predictions.rds")))
    CPE_CV <- readRDS(file.path(dir.work, paste0(spec,"_gam_CPE-median_link_predictions.rds")))
    Warm1 <- readRDS(file.path(dir.work, paste0(spec, "_gam_Warm1_predictions.rds")))
    Cool <- readRDS(file.path(dir.work, paste0(spec, "_gam_Cool_predictions.rds")))
    Warm2 <- readRDS(file.path(dir.work, paste0(spec, "_gam_Warm2_predictions.rds")))
    Stanza_CV <- readRDS(file.path(dir.work, paste0(spec, "_gam_Warm1_link_predictions.rds")))
    Even <- readRDS(file.path(dir.work, paste0(spec,"_gam_Even-year_predictions.rds")))
    Odd <- readRDS(file.path(dir.work, paste0(spec,"_gam_Odd-year_predictions.rds")))
    EO_CV <-     Even <- readRDS(file.path(dir.work, paste0(spec,"_gam_Even-year_link_predictions.rds")))
    }
