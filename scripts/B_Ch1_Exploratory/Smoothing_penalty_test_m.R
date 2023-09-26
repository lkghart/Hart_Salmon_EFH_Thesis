### Smoothing penalty test ###
## author: Lilian Hart
# Last edited: 01/26/23
# Note: testing the m = 1 parameter in GAMs to see if it 
# 1) Removes extreme predictions along the margins,
# 2) Improves AIC, and 3) Improves % Deviance explained

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
og <- readRDS(file.path(dir.work,"v3_basis_subset.rds"))
setwd(dir.work)
  
species <- "Sockeye Salmon"
Spec <- "Sockeye"
  
dat <- og %>% filter(CommonName == species) %>% drop_na(CommonName,
                                                          Effort_area_km2,
                                                          TotalCatchNum)
# Save factor versions of Sample Year 
dat$fSampleYear <- as.factor(dat$SampleYear)

##### Fit Model 3: Spatiotemporal model with factor year effect ####
mod3 <- gam(TotalCatchNum ~ te(EQ.Longitude,EQ.Latitude, bs = "tp", k = 9, m = 1) +
              fSampleYear +
              te(EQ.Longitude,EQ.Latitude, by = fSampleYear, bs = "tp", k = 9, 
                 m= 1) +
              offset(log(Effort_area_km2)),
            family=tw(link = "log"), data = dat); beep(sound=8)
saveRDS(mod3, "sockeye_gam_mod3_V4.rds")

##### Load model ####
mod <- readRDS(file.path(dir.work, "sockeye_gam_mod3_V4.rds"))
summary(mod)
AIC(mod)
#### Predict from 1st derivative penalty model ####
# Set the max number of GAM iterations
gam.control(maxit=3600)

# Load prediction grid, save as dataframe.
p_grid <- readRDS(file.path(dir.work,"prediction_grid2.rds"))
p_grid <- data.frame(p_grid)

# Generate list of years
years <- unique(dat$fSampleYear)
num_years <- unique(dat$SampleYear)
n_years <- 18

pred_v4 <- data.frame()
for(i in 1:n_years){
  temp_pred <-  predict(mod, 
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
  pred_v4 <- rbind(pred_v4, temp_df)
} 
saveRDS(pred_v4, file.path(dir.work,paste0(Spec,"_V4_Test_Predictions_Link.rds")))

#### Map predictions ####
require(viridis)
require(visreg)
require(gratia)
require(beepr)
library(remote)
library(rnaturalearth)
library(rnaturalearthhires)
library(gganimate)
library(sf)
library(rnaturalearthdata)
require(ggspatial)

dir.shelf <- file.path("~/Documents/AK_Shapefiles")
dir.fig <- file.path("~/Documents/Salmon_EFH_Deliverables/Chapter1_Figures/V3_Figs/Comparisons")

# Load in mapping attributes
usa <- ne_states("United States of America", returnclass = "sf")
ak <- subset(usa, name == "Alaska")
ak <- st_set_crs(ak, 4326)
russia <-rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")%>%
  filter(name=="Russia")
russia <- st_set_crs(russia, 4326)
shelf <- st_read(file.path(dir.shelf, "arctic_coast_bathy/arctic_coast_bathy.shp"))
shelfb <- select(shelf, elevation, geometry) %>% 
  filter(elevation == -105 | elevation == -55)
shelfb <- st_transform(shelfb, 4326)
cshelf <- st_read(file.path(dir.shelf, "AK_CSB.gdb"))
rivers <- st_read(file.path(dir.shelf, "USA_Major_Rivers/v10/rivers.gdb"))
riv <- rivers %>% filter(NAME == "Kuskokwim" | NAME == "Yukon" |
                           NAME == "Koyukuk" | NAME == "Stikine")

# Convert predictions to shapefiles for mapping
pred_v4_df <- data.frame(Fit = pred_v4$Fit, Lon = pred_v4$Lon, Lat = pred_v4$Lat, 
                         CV = pred_v4$CV, Year = pred_v4$Year)
pred_v4_sf <- st_as_sf(pred_v4_df, coords = c("Lon", "Lat"), crs = 4326)

# Filter down to years of interest (extreme predictions along margins)
v4_2008 <- pred_v4_sf %>% filter(Year == 2008)
v4_2012 <- pred_v4_sf %>% filter(Year == 2012)
v4_2013 <- pred_v4_sf %>% filter(Year == 2013)

## Map of Gaussian Process Spline predictions ##
# 2008
ggplot() +
  geom_sf(data = v4_2008, mapping = aes(color = Fit)) +
  geom_sf(data = russia, fill = "#CCCCCC") +
  scale_color_viridis(name = "Log(count)") +
  geom_sf(data = shelfb, color = "white", fill = NA) +
  geom_sf(data = cshelf, color = "white") +
  geom_sf(data = ak, fill = "#CCCCCC") +
  geom_sf(data = riv, color = "#666262") +
  coord_sf(crs = st_crs(3338), xlim = c(-2280000, 180000), 
           ylim = c(50000, 2800000), expand = FALSE, 
           datum = st_crs(4326))+
  scale_x_continuous(breaks = c(seq(-180 , -125, by = 7))) +
  facet_wrap(~Year) +
  annotate("text", label = "100 m", x=-900000, y=750000, color = "white",
           size = 2.6) +
  annotate("text", label = "50 m", x=-550000, y=770000, color = "white",
           size = 2.6) +
  annotate("text", label = "Continental Shelf", x=-1550000, y=1250000, 
           color = "white", size = 3, angle = 25) +
  annotate("text", label = "Russia", x=-1700000, y=2300000, color = "white",
           size = 4) +
  annotate("text", label = "Yukon", x=-240000, y=1500000, color = "black",
           size = 2, angle = 57) +
  annotate("text", label = "Kuskokwim", x=-270000, y=1240000, color = "black",
           size = 2, angle = 22) +
  annotate("text", label = "Bristol\n Bay", x=-340000, y=870000, color = "black",
           size = 2) +
  annotate("text", label = "Unimak\n Pass", x=-650000, y=450000, color = "black",
           size = 2) +
  labs(x = "Longitude", y = "Latitude") +
  theme(axis.title=element_text(size=14), 
        strip.text=element_text(size=14))
#Save to output folder
ggsave(device = "jpeg", height = 5, width = 8,
       path = dir.fig, filename = paste0(Spec,"_V4_test_2008.jpeg"))

#2012
ggplot() +
  geom_sf(data = v4_2012, mapping = aes(color = Fit)) +
  geom_sf(data = russia, fill = "#CCCCCC") +
  scale_color_viridis(name = "Log(count)") +
  geom_sf(data = shelfb, color = "white", fill = NA) +
  geom_sf(data = cshelf, color = "white") +
  geom_sf(data = ak, fill = "#CCCCCC") +
  geom_sf(data = riv, color = "#666262") +
  coord_sf(crs = st_crs(3338), xlim = c(-2280000, 180000), 
           ylim = c(50000, 2800000), expand = FALSE, 
           datum = st_crs(4326))+
  scale_x_continuous(breaks = c(seq(-180 , -125, by = 7))) +
  facet_wrap(~Year) +
  annotate("text", label = "100 m", x=-900000, y=750000, color = "white",
           size = 2.6) +
  annotate("text", label = "50 m", x=-550000, y=770000, color = "white",
           size = 2.6) +
  annotate("text", label = "Continental Shelf", x=-1550000, y=1250000, 
           color = "white", size = 3, angle = 25) +
  annotate("text", label = "Russia", x=-1700000, y=2300000, color = "white",
           size = 4) +
  annotate("text", label = "Yukon", x=-240000, y=1500000, color = "black",
           size = 2, angle = 57) +
  annotate("text", label = "Kuskokwim", x=-270000, y=1240000, color = "black",
           size = 2, angle = 22) +
  annotate("text", label = "Bristol\n Bay", x=-340000, y=870000, color = "black",
           size = 2) +
  annotate("text", label = "Unimak\n Pass", x=-650000, y=450000, color = "black",
           size = 2) +
  labs(x = "Longitude", y = "Latitude") +
  theme(axis.title=element_text(size=14), 
        strip.text=element_text(size=14))
#Save to output folder
ggsave(device = "jpeg", height = 5, width = 8,
       path = dir.fig, filename = paste0(Spec,"_V4_test_2012.jpeg"))

#2013
ggplot() +
  geom_sf(data = v4_2013, mapping = aes(color = Fit)) +
  geom_sf(data = russia, fill = "#CCCCCC") +
  scale_color_viridis(name = "Log(count)") +
  geom_sf(data = shelfb, color = "white", fill = NA) +
  geom_sf(data = cshelf, color = "white") +
  geom_sf(data = ak, fill = "#CCCCCC") +
  geom_sf(data = riv, color = "#666262") +
  coord_sf(crs = st_crs(3338), xlim = c(-2280000, 180000), 
           ylim = c(50000, 2800000), expand = FALSE, 
           datum = st_crs(4326))+
  scale_x_continuous(breaks = c(seq(-180 , -125, by = 7))) +
  facet_wrap(~Year) +
  annotate("text", label = "100 m", x=-900000, y=750000, color = "white",
           size = 2.6) +
  annotate("text", label = "50 m", x=-550000, y=770000, color = "white",
           size = 2.6) +
  annotate("text", label = "Continental Shelf", x=-1550000, y=1250000, 
           color = "white", size = 3, angle = 25) +
  annotate("text", label = "Russia", x=-1700000, y=2300000, color = "white",
           size = 4) +
  annotate("text", label = "Yukon", x=-240000, y=1500000, color = "black",
           size = 2, angle = 57) +
  annotate("text", label = "Kuskokwim", x=-270000, y=1240000, color = "black",
           size = 2, angle = 22) +
  annotate("text", label = "Bristol\n Bay", x=-340000, y=870000, color = "black",
           size = 2) +
  annotate("text", label = "Unimak\n Pass", x=-650000, y=450000, color = "black",
           size = 2) +
  labs(x = "Longitude", y = "Latitude") +
  theme(axis.title=element_text(size=14), 
        strip.text=element_text(size=14))
#Save to output folder
ggsave(device = "jpeg", height = 5, width = 8,
       path = dir.fig, filename = paste0(Spec,"_V4_test_2013.jpeg"))

