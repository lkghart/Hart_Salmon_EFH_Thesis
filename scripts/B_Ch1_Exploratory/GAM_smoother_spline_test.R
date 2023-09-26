#### Chapter 1: GAM smoother spline test ##
# author: Lilian Hart
# date last modified: 10/11/2022
# Notes: Trained on sockeye salmon data.
# Using predictions in normal (NOT LINK) space, for easier interpretation
# of CVs (uncertainty).
# This script will test the sensitivity and performance of GAMs when
# the smoothing term is modified. The first version, already fitted to the
# data, uses a full tensor product smooth te(), and thin plate splines for
# the lat long interaction, cubic regression for the fourth model's year 
# interaction terms. The second version will use the same params, but with
# the dimensions of the bases set to k = 10 (This sets the upper limit on the
# degrees of freedom allowed in the smooth. The third version will set the basis
# to 'ds'. The fourth version will add m=c(1,.5). 
# The fifth version will use te(), but with a Gaussian process spline.
# According to comments from Jim Thorson, they are better (*see email on why).
# According to Simpson 2018, Gaussian process splines are well-suited for
# modeling variation through time (time series).
#The fifth version will use an s() for any lat lon smooths that do not have an
#interaction term with different
# units, and also set bs=ds and m=c(1,.5).

require(tidyverse)
require(dplyr)
require(here)
require(viridis)
require(mgcv)
require(visreg)
require(ggmap)
require(gratia)
require(beepr)
require(rnaturalearth)
require(rnaturalearthhires)
require(sf)
require(rnaturalearthdata)
require(ggspatial)
require(viridis)

#### Load in data and model V.1 ####
dir.data <- here("data", "BASIS")
og <- readRDS(paste0(dir.data,"/full_basis_combo_data"))
dir.mod <- here("data","Chapter_1_RDSModels")
dir.work <- here("data", "Chapter_1_RDSModels", "GAM_test_case")
setwd(dir.work)

species <- "Sockeye Salmon"

dat <- og %>% filter(CommonName == species) %>% drop_na(CommonName,
                                                        Effort_area_km2,
                                                        TotalCatchNum)
# Save factor versions of Sample Year 
dat$fSampleYear <- as.factor(dat$SampleYear)

# Set max number of GAM iterations
gam.control(maxit=3600)

# Load in version 1 (V1) models
v1_mod1 <- readRDS(paste0(dir.mod,"/sockeye_gam_mod1.rds"))
v1_mod2 <- readRDS(paste0(dir.mod,"/sockeye_gam_mod2.rds"))
v1_mod3 <- readRDS(paste0(dir.mod,"/sockeye_gam_mod3.rds"))
v1_mod4 <- readRDS(paste0(dir.mod,"/sockeye_gam_mod4.rds"))

#### Fit or load models V2-V6 ####
fit <- FALSE
if(fit ==TRUE){
  #### Model V2, set max degrees of freedom to 10-1 via k argument ####
  ## Model 1: Average spatial field 
  v2_mod1 <- gam(TotalCatchNum ~ te(EQ.Longitude,EQ.Latitude, bs = "tp", k = 10), 
                 offset = log(Effort_area_km2), family = tw(link = "log"), data = dat) 
  saveRDS(v2_mod1, "v2_mod1.rds")
  summary(v2_mod1)             
  
  ## Model 2: Average spatial field with yearly changes in abundance
  v2_mod2 <- gam(TotalCatchNum ~ te(EQ.Longitude,EQ.Latitude, bs = "tp", k = 10) +
                   fSampleYear, offset = log(Effort_area_km2),
                 family=tw(link = "log"), data = dat)
  saveRDS(v2_mod2, "v2_mod2.rds")
  summary(v2_mod2)
  
  ## Model 3: Spatiotemporal model with factor year effect
  v2_mod3 <- gam(TotalCatchNum ~ te(EQ.Longitude,EQ.Latitude, bs = "tp", k = 10) +
                   fSampleYear + 
                   te(EQ.Longitude,EQ.Latitude, by = fSampleYear, bs = "tp", k = 10),
                 offset = log(Effort_area_km2),
                 family=tw(link = "log"), data = dat); beep(sound=8)
  saveRDS(v2_mod3, "v2_mod3.rds")
  summary(v2_mod3)
  
  
  
  ## Model 4: Spatiotemporal model with continuous year effect
  v2_mod4 <- gam(TotalCatchNum  ~ te(EQ.Longitude,EQ.Latitude, bs = "tp", k=10) +
                fSampleYear + 
                te(EQ.Longitude,EQ.Latitude,SampleYear, d=c(2,1), 
                   bs=c("tp","cr"), k=10), 
              offset = log(Effort_area_km2),
              family=tw(link = "log"), data = dat);beep(sound=8)
  saveRDS(v2_mod4, "v2_mod4.rds")
  summary(v2_mod4)
  
  #### Model V3, change basis to Duchon spline ####
  ## Model 1: Average spatial field 
  v3_mod1 <- gam(TotalCatchNum ~ te(EQ.Longitude,EQ.Latitude, bs = "ds", k = 10), 
                 offset = log(Effort_area_km2), family = tw(link = "log"), data = dat) 
  saveRDS(v3_mod1, "v3_mod1.rds")
  summary(v3_mod1)             
  
  ## Model 2: Average spatial field with yearly changes in abundance
  v3_mod2 <- gam(TotalCatchNum ~ te(EQ.Longitude,EQ.Latitude, bs = "ds", k = 10) +
                   fSampleYear, offset = log(Effort_area_km2),
                 family=tw(link = "log"), data = dat)
  saveRDS(v3_mod2, "v3_mod2.rds")
  summary(v3_mod2)
  
  ## Model 3: Spatiotemporal model with factor year effect
  v3_mod3 <- gam(TotalCatchNum ~ te(EQ.Longitude,EQ.Latitude, bs = "ds", k = 10) +
                   fSampleYear + 
                   te(EQ.Longitude,EQ.Latitude, by = fSampleYear, bs = "ds", k = 10),
                 offset = log(Effort_area_km2),
                 family=tw(link = "log"), data = dat); beep(sound=8)
  saveRDS(v3_mod3, "v3_mod3.rds")
  summary(v3_mod3)
  
  ## Model 4: Spatiotemporal model with continuous year effect
  v3_mod4 <- gam(TotalCatchNum  ~ te(EQ.Longitude,EQ.Latitude, bs = "ds", k=10) +
                   fSampleYear + 
                   te(EQ.Longitude,EQ.Latitude,SampleYear, d=c(2,1), 
                      bs=c("ds","cr"), k=10), 
                 offset = log(Effort_area_km2),
                 family=tw(link = "log"), data = dat);beep(sound=8)
  saveRDS(v3_mod4, "v3_mod4.rds")
  summary(v3_mod4)
  
  #### Model V4, restrict spline basis and penalty; add m=c(1,.5) ####
  
  ## Model 1: Average spatial field 
  v4_mod1 <- gam(TotalCatchNum ~ te(EQ.Longitude,EQ.Latitude, bs = "ds", k = 10, m=c(1,0.5)), 
                 offset = log(Effort_area_km2), family = tw(link = "log"), data = dat) 
  saveRDS(v4_mod1, "v4_mod1.rds")
  summary(v4_mod1)             
  
  ## Model 2: Average spatial field with yearly changes in abundance
  v4_mod2 <- gam(TotalCatchNum ~ te(EQ.Longitude,EQ.Latitude, bs = "ds", k = 10, m=c(1,0.5)) +
                   fSampleYear, offset = log(Effort_area_km2),
                 family=tw(link = "log"), data = dat)
  saveRDS(v4_mod2, "v4_mod2.rds")
  summary(v4_mod2)
  
  ## Model 3: Spatiotemporal model with factor year effect
  v4_mod3 <- gam(TotalCatchNum ~ te(EQ.Longitude,EQ.Latitude, bs = "ds", k = 10, m=c(1, 0.5)) +
                   fSampleYear + 
                   te(EQ.Longitude,EQ.Latitude, by = fSampleYear, bs = "ds", k = 10, m=c(1,0.5)),
                 offset = log(Effort_area_km2),
                 family=tw(link = "log"), data = dat); beep(sound=8)
  saveRDS(v4_mod3, "v4_mod3.rds")
  summary(v4_mod3)
  
  ## Model 4: Spatiotemporal model with continuous year effect
  v4_mod4 <- gam(TotalCatchNum  ~ te(EQ.Longitude,EQ.Latitude, bs = "ds", k=10, m=c(1, 0.5)) +
                   fSampleYear + 
                   te(EQ.Longitude,EQ.Latitude,SampleYear, d=c(2,1), 
                      bs=c("ds","cr"), k=10, m=c(1, 0.5)), 
                 offset = log(Effort_area_km2),
                 family=tw(link = "log"), data = dat);beep(sound=8)
  saveRDS(v4_mod4, "v4_mod4.rds")
  summary(v4_mod4)
  
  #### V5: back to original te(), but with Gaussian Process splines ####
  
  ## Model 1: Average spatial field 
  v5_mod1 <- gam(TotalCatchNum ~ te(EQ.Longitude,EQ.Latitude, bs = "gp"), 
                 offset = log(Effort_area_km2), family = tw(link = "log"), data = dat) 
  saveRDS(v5_mod1, "v5_mod1.rds")
  summary(v5_mod1)             
  
  ## Model 2: Average spatial field with yearly changes in abundance
  v5_mod2 <- gam(TotalCatchNum ~ te(EQ.Longitude,EQ.Latitude, bs = "gp") +
                   fSampleYear, offset = log(Effort_area_km2),
                 family=tw(link = "log"), data = dat)
  saveRDS(v5_mod2, "v5_mod2.rds")
  summary(v5_mod2)
  
  #### V6: Use s(), but with Gaussian Process splines ####
  ## Model 1: Average spatial field 
  v6_mod1 <- gam(TotalCatchNum ~ s(EQ.Longitude,EQ.Latitude, bs = "gp"), 
                 offset = log(Effort_area_km2), family = tw(link = "log"), data = dat) 
  saveRDS(v6_mod1, "v6_mod1.rds")
  summary(v6_mod1)             
  
  ## Model 2: Average spatial field with yearly changes in abundance
  v6_mod2 <- gam(TotalCatchNum ~ s(EQ.Longitude,EQ.Latitude, bs = "gp") +
                   fSampleYear, offset = log(Effort_area_km2),
                 family=tw(link = "log"), data = dat)
  saveRDS(v6_mod2, "v6_mod2.rds")
  summary(v6_mod2)
  
  }
else{
  v2_mod1 <- readRDS(paste0(dir.work, "/v2_mod1.rds"))
  v2_mod2 <- readRDS(paste0(dir.work, "/v2_mod2.rds"))
  v2_mod3 <- readRDS(paste0(dir.work, "/v2_mod3.rds"))
  v2_mod4 <- readRDS(paste0(dir.work, "/v2_mod4.rds"))
  v3_mod4 <- readRDS(paste0(dir.work, "/v2_mod4.rds"))
  v3_mod1 <- readRDS(paste0(dir.work, "/v3_mod1.rds"))
  v3_mod2 <- readRDS(paste0(dir.work, "/v3_mod2.rds"))
  v3_mod3 <- readRDS(paste0(dir.work, "/v3_mod3.rds"))
  v3_mod4 <- readRDS(paste0(dir.work, "/v3_mod4.rds"))
  v4_mod1 <- readRDS(paste0(dir.work, "/v4_mod1.rds"))
  v4_mod2 <- readRDS(paste0(dir.work, "/v4_mod2.rds"))
  # v4_mod3 <- readRDS(paste0(dir.work, "/v4_mod3.rds"))
  v4_mod4 <- readRDS(paste0(dir.work, "/v4_mod4.rds"))
  v5_mod1 <- readRDS(paste0(dir.work, "/v5_mod1.rds"))
  v5_mod2 <- readRDS(paste0(dir.work, "/v5_mod2.rds"))
  # v5_mod3 <- readRDS(paste0(dir.work, "/v5_mod3.rds"))
  # v5_mod4 <- readRDS(paste0(dir.work, "/v5_mod4.rds"))
  v6_mod1 <- readRDS(paste0(dir.work, "/v6_mod1.rds"))
  v6_mod2 <- readRDS(paste0(dir.work, "/v6_mod2.rds"))
  # v6_mod3 <- readRDS(paste0(dir.work, "/v6_mod3.rds"))
  # v6_mod4 <- readRDS(paste0(dir.work, "/v6_mod4.rds"))
}



#### Run AIC analyses ####
AIC(v1_mod1, v2_mod1, v3_mod1, v4_mod1, v5_mod1, v6_mod1)
AIC(v1_mod2, v2_mod2, v3_mod2, v4_mod2, v5_mod2, v6_mod2)
AIC(v1_mod3, v2_mod3, v3_mod3)
AIC(v1_mod4, v2_mod4, v3_mod4, v4_mod4)

#### Predict responses or load predictions ####
# Load prediction grid, save as dataframe.
p_grid <- readRDS(file.path(dir.mod,"prediction_grid.rds"))
p_grid <- data.frame(p_grid)

fit2 <- FALSE
if(fit2 == TRUE){
  ## V2 Predictions ##
  temp_pred <-  predict(v2_mod1, newdata = data.frame(EQ.Longitude=p_grid$Lon, 
                                                      EQ.Latitude=p_grid$Lat), 
                        se=TRUE, type = "response")
  v2m1_pred <- data.frame(temp_pred)
  v2m1_pred <- cbind(v2m1_pred, p_grid)
  saveRDS(v2m1_pred, file.path(dir.work,"v2_mod1_predictions.rds"))
  
  temp_pred <-  predict(v2_mod2, 
                        newdata = data.frame(EQ.Longitude = p_grid$Lon, 
                                             EQ.Latitude = p_grid$Lat, 
                                             fSampleYear = 2015), se=TRUE,
                        type = "response")
  v2m2_pred <- data.frame(temp_pred)
  v2m2_pred <- cbind(v2m2_pred, p_grid)
  saveRDS(v2m2_pred, file.path(dir.work, "v2_mod2_predictions.rds"))
  
  ## V3 Predictions ##
  temp_pred <-  predict(v3_mod1, newdata = data.frame(EQ.Longitude=p_grid$Lon, 
                                                      EQ.Latitude=p_grid$Lat), 
                        se=TRUE, type = "response")
  v3m1_pred <- data.frame(temp_pred)
  v3m1_pred <- cbind(v3m1_pred, p_grid)
  saveRDS(v3m1_pred, file.path(dir.work,"v3_mod1_predictions.rds"))
  
  temp_pred <-  predict(v3_mod2, 
                        newdata = data.frame(EQ.Longitude = p_grid$Lon, 
                                             EQ.Latitude = p_grid$Lat, 
                                             fSampleYear = 2015), se=TRUE,
                        type = "response")
  v3m2_pred <- data.frame(temp_pred)
  v3m2_pred <- cbind(v3m2_pred, p_grid)
  saveRDS(v3m2_pred, file.path(dir.work, "v3_mod2_predictions.rds"))
  
  ## V4 Predictions
  temp_pred <-  predict(v4_mod1, newdata = data.frame(EQ.Longitude=p_grid$Lon, 
                                                      EQ.Latitude=p_grid$Lat), 
                        se=TRUE, type = "response")
  v4m1_pred <- data.frame(temp_pred)
  v4m1_pred <- cbind(v4m1_pred, p_grid)
  saveRDS(v4m1_pred, file.path(dir.work,"v4_mod1_predictions.rds"))
  
  temp_pred <-  predict(v4_mod2, 
                        newdata = data.frame(EQ.Longitude = p_grid$Lon, 
                                             EQ.Latitude = p_grid$Lat, 
                                             fSampleYear = 2015), se=TRUE,
                        type = "response")
  v4m2_pred <- data.frame(temp_pred)
  v4m2_pred <- cbind(v4m2_pred, p_grid)
  saveRDS(v4m2_pred, file.path(dir.work, "v4_mod2_predictions.rds"))
  
  ## V5 Predictions
  temp_pred <-  predict(v5_mod1, newdata = data.frame(EQ.Longitude=p_grid$Lon, 
                                                      EQ.Latitude=p_grid$Lat), 
                        se=TRUE, type = "response")
  v5m1_pred <- data.frame(temp_pred)
  v5m1_pred <- cbind(v5m1_pred, p_grid)
  saveRDS(v5m1_pred, file.path(dir.work,"v5_mod1_predictions.rds"))
  
  temp_pred <-  predict(v5_mod2, 
                        newdata = data.frame(EQ.Longitude = p_grid$Lon, 
                                             EQ.Latitude = p_grid$Lat, 
                                             fSampleYear = 2015), se=TRUE,
                        type = "response")
  v5m2_pred <- data.frame(temp_pred)
  v5m2_pred <- cbind(v5m2_pred, p_grid)
  saveRDS(v5m2_pred, file.path(dir.work, "v5_mod2_predictions.rds"))
  
  ## V6 Predictions
  temp_pred <-  predict(v6_mod1, newdata = data.frame(EQ.Longitude=p_grid$Lon, 
                                                      EQ.Latitude=p_grid$Lat), 
                        se=TRUE, type = "response")
  v6m1_pred <- data.frame(temp_pred)
  v6m1_pred <- cbind(v6m1_pred, p_grid)
  saveRDS(v6m1_pred, file.path(dir.work,"v6_mod1_predictions.rds"))
  
  temp_pred <-  predict(v6_mod2, 
                        newdata = data.frame(EQ.Longitude = p_grid$Lon, 
                                             EQ.Latitude = p_grid$Lat, 
                                             fSampleYear = 2015), se=TRUE,
                        type = "response")
  v6m2_pred <- data.frame(temp_pred)
  v6m2_pred <- cbind(v6m2_pred, p_grid)
  saveRDS(v6m2_pred, file.path(dir.work, "v6_mod2_predictions.rds"))
}else{
  ## V1 Predictions ##
  v1m1_pred <- readRDS(paste0(dir.mod,"/Sockeye_GAM_Mod1_Predictions.rds"))
  v1m2_pred <- readRDS(paste0(dir.mod,"/Sockeye_GAM_Mod2_Predictions.rds"))
  v1m3_pred <- readRDS(paste0(dir.mod,"/Sockeye_GAM_Mod3_Predictions.rds"))
  v1m4_pred <- readRDS(paste0(dir.mod,"/Sockeye_GAM_Mod4_Predictions.rds"))
  ## V2-V6 ##
  v2m1_pred <- readRDS(paste0(dir.work,"/v2_mod1_predictions.rds"))
  v2m2_pred <- readRDS(paste0(dir.work,"/v2_mod2_predictions.rds"))
  v3m1_pred <- readRDS(paste0(dir.work,"/v3_mod1_predictions.rds"))
  v3m2_pred <- readRDS(paste0(dir.work,"/v3_mod2_predictions.rds"))
  v4m1_pred <- readRDS(paste0(dir.work,"/v4_mod1_predictions.rds"))
  v4m2_pred <- readRDS(paste0(dir.work,"/v4_mod2_predictions.rds"))
  v5m1_pred <- readRDS(paste0(dir.work,"/v5_mod1_predictions.rds"))
  v5m2_pred <- readRDS(paste0(dir.work,"/v5_mod2_predictions.rds"))
  v6m1_pred <- readRDS(paste0(dir.work,"/v6_mod1_predictions.rds"))
  v6m2_pred <- readRDS(paste0(dir.work,"/v6_mod2_predictions.rds"))
}
#### Assess variability in predictions ####
## Summary statistics
summary(v1m1_pred$fit)
summary(v1m2_pred$fit)
summary(v2m1_pred$fit)
summary(v2m2_pred$fit)
summary(v3m1_pred$fit)
summary(v3m2_pred$fit)
summary(v4m1_pred$fit)
summary(v4m2_pred$fit)
summary(v5m1_pred$fit)
summary(v5m2_pred$fit)
summary(v6m1_pred$fit)
summary(v6m2_pred$fit)

## Plot predictions and uncertainty ##
# Load in mapping attributes
usa <- ne_states("United States of America", returnclass = "sf")
ak <- subset(usa, name == "Alaska")
ak <- st_set_crs(ak, 4326)
russia <-rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")%>%
  filter(name=="Russia")
russia <- st_set_crs(russia, 4326)

# Model 1, versions 1-6
v1m1_pred <- st_as_sf(v1m1_pred, coords = c("Lon", "Lat"), crs = 4326)
v2m1_pred <- st_as_sf(v2m1_pred, coords = c("Lon", "Lat"), crs = 4326)
v3m1_pred <- st_as_sf(v3m1_pred, coords = c("Lon", "Lat"), crs = 4326)
v4m1_pred <- st_as_sf(v4m1_pred, coords = c("Lon", "Lat"), crs = 4326)
v5m1_pred <- st_as_sf(v5m1_pred, coords = c("Lon", "Lat"), crs = 4326)
v6m1_pred <- st_as_sf(v6m1_pred, coords = c("Lon", "Lat"), crs = 4326)
#V1M1
v1m1_plot <- ggplot() +
  geom_sf(data=russia) + 
  geom_sf(data = v1m1_pred, mapping = aes(color = fit), 
          alpha = 1/(v1m1_pred$se.fit/v1m1_pred$fit)) +
  geom_sf(data = ak) +
  coord_sf(crs = st_crs(3338), xlim = c(-2280000, 180000), 
           ylim = c(50000, 2800000), expand = FALSE, 
           datum = st_crs(4326))+
  scale_x_continuous(breaks = c(seq(-180 , -125, by = 7)))+
  scale_color_viridis(name = "Count") +
  labs(x = "Longitude", y = "Latitude", 
       title = "Juvenile sockeye salmon predicted abundance\n Model 1, V1",
       fill = "Density")
v1m1_plot

#V2M1
v2m1_plot <- ggplot() +
  geom_sf(data=russia) + 
  geom_sf(data = v2m1_pred, mapping = aes(color = fit), 
          alpha = 1/(v2m1_pred$se.fit/v2m1_pred$fit)) +
  geom_sf(data = ak) +
  coord_sf(crs = st_crs(3338), xlim = c(-2280000, 180000), 
           ylim = c(50000, 2800000), expand = FALSE, 
           datum = st_crs(4326))+
  scale_x_continuous(breaks = c(seq(-180 , -125, by = 7)))+
  scale_color_viridis(name = "Count") +
  labs(x = "Longitude", y = "Latitude", 
       title = "Juvenile sockeye salmon predicted abundance\n Model 1, V2",
       fill = "Density")
v2m1_plot

#V3M1
v3m1_plot <- ggplot() +
  geom_sf(data=russia) + 
  geom_sf(data = v3m1_pred, mapping = aes(color = fit), 
          alpha = 1/(v3m1_pred$se.fit/v3m1_pred$fit)) +
  geom_sf(data = ak) +
  coord_sf(crs = st_crs(3338), xlim = c(-2280000, 180000), 
           ylim = c(50000, 2800000), expand = FALSE, 
           datum = st_crs(4326))+
  scale_x_continuous(breaks = c(seq(-180 , -125, by = 7)))+
  scale_color_viridis(name = "Count") +
  labs(x = "Longitude", y = "Latitude", 
       title = "Juvenile sockeye salmon predicted abundance\n Model 1, V3",
       fill = "Density")
v3m1_plot

#V4M1
v4m1_plot <- ggplot() +
  geom_sf(data=russia) + 
  geom_sf(data = v4m1_pred, mapping = aes(color = fit), 
          alpha = 1/(v4m1_pred$se.fit/v4m1_pred$fit)) +
  geom_sf(data = ak) +
  coord_sf(crs = st_crs(3338), xlim = c(-2280000, 180000), 
           ylim = c(50000, 2800000), expand = FALSE, 
           datum = st_crs(4326))+
  scale_x_continuous(breaks = c(seq(-180 , -125, by = 7)))+
  scale_color_viridis(name = "Count") +
  labs(x = "Longitude", y = "Latitude", 
       title = "Juvenile sockeye salmon predicted abundance\n Model 1, V4",
       fill = "Density")
v4m1_plot

#V5M1
v5m1_plot <- ggplot() +
  geom_sf(data=russia) + 
  geom_sf(data = v5m1_pred, mapping = aes(color = fit), 
          alpha = 1/(v5m1_pred$se.fit/v5m1_pred$fit)) +
  geom_sf(data = ak) +
  coord_sf(crs = st_crs(3338), xlim = c(-2280000, 180000), 
           ylim = c(50000, 2800000), expand = FALSE, 
           datum = st_crs(4326))+
  scale_x_continuous(breaks = c(seq(-180 , -125, by = 7)))+
  scale_color_viridis(name = "Count") +
  labs(x = "Longitude", y = "Latitude", 
       title = "Juvenile sockeye salmon predicted abundance\n Model 1, V5",
       fill = "Density")
v5m1_plot

#V6M1
v6m1_plot <- ggplot() +
  geom_sf(data=russia) + 
  geom_sf(data = v6m1_pred, mapping = aes(color = fit), 
          alpha = 1/(v6m1_pred$se.fit/v6m1_pred$fit)) +
  geom_sf(data = ak) +
  coord_sf(crs = st_crs(3338), xlim = c(-2280000, 180000), 
           ylim = c(50000, 2800000), expand = FALSE, 
           datum = st_crs(4326))+
  scale_x_continuous(breaks = c(seq(-180 , -125, by = 7)))+
  scale_color_viridis(name = "Count") +
  labs(x = "Longitude", y = "Latitude", 
       title = "Juvenile sockeye salmon predicted abundance\n Model 1, V6",
       fill = "Density")
v6m1_plot

# Model 2 (Year 2015), versions 1-6
v1m2_pred <- st_as_sf(v1m2_pred, coords = c("Lon", "Lat"), crs = 4326)
v2m2_pred <- st_as_sf(v2m2_pred, coords = c("Lon", "Lat"), crs = 4326)
v3m2_pred <- st_as_sf(v3m2_pred, coords = c("Lon", "Lat"), crs = 4326)
v4m2_pred <- st_as_sf(v4m2_pred, coords = c("Lon", "Lat"), crs = 4326)
v5m2_pred <- st_as_sf(v5m2_pred, coords = c("Lon", "Lat"), crs = 4326)
v6m2_pred <- st_as_sf(v6m2_pred, coords = c("Lon", "Lat"), crs = 4326)

#V1M2
v1m2_plot <- ggplot() +
  geom_sf(data=russia) + 
  geom_sf(data = v1m2_pred, mapping = aes(color = fit), 
          alpha = 1/(v1m2_pred$se.fit/v1m2_pred$fit)) +
  geom_sf(data = ak) +
  coord_sf(crs = st_crs(3338), xlim = c(-2280000, 180000), 
           ylim = c(50000, 2800000), expand = FALSE, 
           datum = st_crs(4326))+
  scale_x_continuous(breaks = c(seq(-180 , -125, by = 7)))+
  scale_color_viridis(name = "Count") +
  labs(x = "Longitude", y = "Latitude", 
       title = "Juvenile sockeye salmon predicted abundance\n Model 2, V1 (2015)",
       fill = "Density")
v1m2_plot

#V2M2
v2m2_plot <- ggplot() +
  geom_sf(data=russia) + 
  geom_sf(data = v2m2_pred, mapping = aes(color = fit), 
          alpha = 1/(v2m2_pred$se.fit/v2m2_pred$fit)) +
  geom_sf(data = ak) +
  coord_sf(crs = st_crs(3338), xlim = c(-2280000, 180000), 
           ylim = c(50000, 2800000), expand = FALSE, 
           datum = st_crs(4326))+
  scale_x_continuous(breaks = c(seq(-180 , -125, by = 7)))+
  scale_color_viridis(name = "Count") +
  labs(x = "Longitude", y = "Latitude", 
       title = "Juvenile sockeye salmon predicted abundance\n Model 2, V2 (2015)",
       fill = "Density")
v2m2_plot

#V3M2
v3m2_plot <- ggplot() +
  geom_sf(data=russia) + 
  geom_sf(data = v3m2_pred, mapping = aes(color = fit), 
          alpha = 1/(v3m2_pred$se.fit/v3m2_pred$fit)) +
  geom_sf(data = ak) +
  coord_sf(crs = st_crs(3338), xlim = c(-2280000, 180000), 
           ylim = c(50000, 2800000), expand = FALSE, 
           datum = st_crs(4326))+
  scale_x_continuous(breaks = c(seq(-180 , -125, by = 7)))+
  scale_color_viridis(name = "Count") +
  labs(x = "Longitude", y = "Latitude", 
       title = "Juvenile sockeye salmon predicted abundance\n Model 2, V3 (2015)",
       fill = "Density")
v3m2_plot

#V4M2
v4m2_plot <- ggplot() +
  geom_sf(data=russia) + 
  geom_sf(data = v4m2_pred, mapping = aes(color = fit), 
          alpha = 1/(v4m2_pred$se.fit/v4m2_pred$fit)) +
  geom_sf(data = ak) +
  coord_sf(crs = st_crs(3338), xlim = c(-2280000, 180000), 
           ylim = c(50000, 2800000), expand = FALSE, 
           datum = st_crs(4326))+
  scale_x_continuous(breaks = c(seq(-180 , -125, by = 7)))+
  scale_color_viridis(name = "Count") +
  labs(x = "Longitude", y = "Latitude", 
       title = "Juvenile sockeye salmon predicted abundance\n Model 2, V4 (2015)",
       fill = "Density")
v4m2_plot

#V5M2
v5m2_plot <- ggplot() +
  geom_sf(data=russia) + 
  geom_sf(data = v5m2_pred, mapping = aes(color = fit), 
          alpha = 1/(v5m2_pred$se.fit/v5m2_pred$fit)) +
  geom_sf(data = ak) +
  coord_sf(crs = st_crs(3338), xlim = c(-2280000, 180000), 
           ylim = c(50000, 2800000), expand = FALSE, 
           datum = st_crs(4326))+
  scale_x_continuous(breaks = c(seq(-180 , -125, by = 7)))+
  scale_color_viridis(name = "Count") +
  labs(x = "Longitude", y = "Latitude", 
       title = "Juvenile sockeye salmon predicted abundance\n Model 2, V5 (2015)",
       fill = "Density")
v5m2_plot

#V6M2
v6m2_plot <- ggplot() +
  geom_sf(data=russia) + 
  geom_sf(data = v6m2_pred, mapping = aes(color = fit), 
          alpha = 1/(v6m2_pred$se.fit/v6m2_pred$fit)) +
  geom_sf(data = ak) +
  coord_sf(crs = st_crs(3338), xlim = c(-2280000, 180000), 
           ylim = c(50000, 2800000), expand = FALSE, 
           datum = st_crs(4326))+
  scale_x_continuous(breaks = c(seq(-180 , -125, by = 7)))+
  scale_color_viridis(name = "Count") +
  labs(x = "Longitude", y = "Latitude", 
       title = "Juvenile sockeye salmon predicted abundance\n Model 2, V6 (2015)",
       fill = "Density")
v6m2_plot

## Try for better plots by using rbind
v1m1_pred$Version <- "v1"
v1m2_pred$Version <- "v1"
v2m1_pred$Version <- "v2"
v2m2_pred$Version <- "v2"
v3m1_pred$Version <- "v3"
v3m2_pred$Version <- "v3"
v4m1_pred$Version <- "v4"
v4m2_pred$Version <- "v4"
v5m1_pred$Version <- "v5"
v5m2_pred$Version <- "v5"
v6m1_pred$Version <- "v6"
v6m2_pred$Version <- "v6"

mod1 <- rbind(v1m1_pred,v2m1_pred,v3m1_pred,v4m1_pred,v5m1_pred,v6m1_pred)
mod2 <- rbind(v1m2_pred,v2m2_pred,v3m2_pred,v4m2_pred,v5m2_pred,v6m2_pred)

mod1_plot <- ggplot() +
  geom_sf(data=russia) + 
  geom_sf(data = mod1, mapping = aes(color = fit), 
          alpha = 1/(mod1$se.fit/mod1$fit)) +
  geom_sf(data = ak) +
  coord_sf(crs = st_crs(3338), xlim = c(-2280000, 180000), 
           ylim = c(50000, 2800000), expand = FALSE, 
           datum = st_crs(4326))+
  scale_x_continuous(breaks = c(seq(-180 , -125, by = 7)))+
  scale_color_viridis(name = "Count") +
  labs(x = "Longitude", y = "Latitude", 
       title = "Juvenile sockeye salmon predicted abundance\n Model 1",
       fill = "Density")
mod1_plot + facet_wrap(~mod1$Version)

#### Plots of parameter estimates of year effects for model 2  ####
years <- c(as.character(2003:2019))
years <- c(2003:2019)
est1 <- as.vector(v1_mod2$coefficients[2:18])
est2 <- as.vector(v2_mod2$coefficients[2:18])
est3 <- as.vector(v3_mod2$coefficients[2:18])
est4 <- as.vector(v4_mod2$coefficients[2:18])
est5 <- as.vector(v5_mod2$coefficients[2:18])
est6 <- as.vector(v6_mod2$coefficients[2:18])
d1 <- data.frame(Year = years, Estimate = est1, Version = "v1")
d2 <- data.frame(Year = years, Estimate = est2, Version = "v2")
d3 <- data.frame(Year = years, Estimate = est3, Version = "v3")
d4 <- data.frame(Year = years, Estimate = est4, Version = "v4")
d5 <- data.frame(Year = years, Estimate = est5, Version = "v5")
d6 <- data.frame(Year = years, Estimate = est6, Version = "v6")
params <- rbind(d1,d2,d3,d4,d5,d6)

plot1 <- ggplot(data = params, aes(x=Year, y=Estimate, col = Version)) + 
  geom_point() + 
  xlab("Sample Year") + ylab("Parameter Estimate)") +
  scale_x_discrete(breaks = c(2002,2005, 2008, 2011, 2014, 2017)) + 
  geom_line(aes(x=Year)) +
  ggtitle("Estimates of Year Effects - Model 2")
plot1

#Intercepts
intercepts <- data.frame(Version = c("v1","v2","v3","v4","v5","v6"),
                         Estimate = c(v1_mod2$coefficients[1],
                                      v2_mod2$coefficients[1],
                                      v3_mod2$coefficients[1],
                                      v4_mod2$coefficients[1],
                                      v5_mod2$coefficients[1],
                                      v6_mod2$coefficients[1]))

ggplot(data = intercepts, aes(x=Version, y=Estimate, col = Version)) + 
  geom_point() + 
  xlab("Version") + ylab("Parameter Estimate)") +
  ggtitle("Estimates of Model Intercept - Model 2")


#### Setup for bs = tp vs tp,cr test ####
dirData <- here("data", "BASIS")
og <- readRDS(paste0(dirData,"/full_basis_combo_data"))
dat <- og %>% filter(CommonName != "Pollock") %>% 
  filter(CommonName != "Coho Salmon") %>% drop_na(CommonName, 
                                                  Effort_area_km2,
                                                  TotalCatchNum)
species <- unique(dat$CommonName)
# Save factor versions of Sample Year 
dat$fSampleYear <- as.factor(dat$SampleYear)

for(i in species){
  subfish <- dat %>% filter(CommonName == i)
  print(i)
  mod4 <- gam(TotalCatchNum  ~ te(EQ.Longitude,EQ.Latitude, bs = "tp") +
                fSampleYear + 
                te(EQ.Longitude,EQ.Latitude,SampleYear, d=c(2,1), bs=c("tp","cr")) +
                offset(log(Effort_area_km2)),family=tw(link = "log"), 
              data = subfish)
  print("This is the tp cr summary for")
  print(i)
  print(summary(mod4))
  
  mod4b <- gam(TotalCatchNum  ~ te(EQ.Longitude,EQ.Latitude, bs = "tp") +
                 fSampleYear + 
                 te(EQ.Longitude,EQ.Latitude,SampleYear, d=c(2,1), bs=c("tp")) +
                 offset(log(Effort_area_km2)),family=tw(link = "log"), 
               data = subfish)
  print("This is the tp-only summary for")
  print(i)
  print(summary(mod4b))
}
