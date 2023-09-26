### Juvenile Chinook prediction maps with uncertainty ###
# author: Lilian Hart
# date last edited: 02/06/23

require(tidyverse)
require(dplyr)
require(here)
require(viridis)
require(mgcv)
require(ggmap)
require(beepr)
require(ggplot2)
require(rnaturalearth)
require(rnaturalearthhires)
require(sf)
require(rnaturalearthdata)
require(ggspatial)
require(VAST)
require(FishStatsUtils)

spec <- "Chinook"
spec2 <- "chinook"

dir.data <- here("data", "BASIS")
dir.work <- here("data", "Chapter_1_RDSModels")
dir.shelf <- file.path("~/Documents/AK_Shapefiles")
dir.fig <- file.path("~/Documents/Salmon_EFH_Deliverables/Chapter1_Figures/V4_Figs")

years <- as.character(2002:2019)

#### Load in models ####
## VAST model 1 ##
tV <- readRDS(file.path(dir.work,paste0(spec2,"_VAST_mod1.rds")))
tV <- reload_model(tV)
sites <- as.data.frame(tV$extrapolation_list$Data_Extrap) %>% 
  drop_na(Include) %>% select(Lon, Lat)
tV <- as.data.frame(tV$Report$D_gct) %>% drop_units()
colnames(tV) <- years
tV <- tV[,1]
tV_CV <- readRDS(file.path(dir.work, paste0(spec2, "_VAST_mod1_CV.rds")))
colnames(tV_CV) <- years
test <- rowMeans(tV_CV)

# Reformat for better plotting, and add CV
est1_V <- data.frame(Fit = tV, CV = test, Lat = sites$Lat, Lon = sites$Lon)

## GAM model 1 ##
est1_G <- readRDS(file.path(dir.work,paste0(spec,"_GAM_Mod1_Predictions_Link.rds")))

## VAST model 3 ##
tempV <- readRDS(file.path(dir.work,paste0(spec2,"_VAST_mod3.rds")))
tempV <- reload_model(tempV)
sites <- as.data.frame(tempV$extrapolation_list$Data_Extrap) %>% 
  drop_na(Include) %>% select(Lon, Lat)
tempV <- as.data.frame(tempV$Report$D_gct) %>% drop_units()
colnames(tempV) <- years
tempV_CV <- readRDS(file.path(dir.work, paste0(spec2, "_VAST_CV.rds")))
# Reformat for better plotting, and add CV
est3_V <- data.frame()
for (i in 1:18){
  x <- years[i]
  a <- tempV %>% select(all_of(x))
  b <- tempV_CV %>% select(all_of(x))
  c <- data.frame(Fit = a, CV = b, Year = x, Lat = sites$Lat, Lon = sites$Lon)
  colnames(c) <- c("Fit", "CV", "Year", "Lat", "Lon")
  est3_V <- rbind(est3_V, c)
  }

## GAM model 3 ##
# GAM model 4 (number three did not converge)
est4_G <- readRDS(file.path(dir.work,paste0(spec,"_GAM_Mod4_Predictions_Link.rds")))

#### Load in mapping attributes ####
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
# Major AK rivers
rivers <- st_read(file.path(dir.shelf, "USA_Major_Rivers/v10/rivers.gdb"))
riv <- rivers %>% filter(NAME == "Kuskokwim" | NAME == "Yukon" |
                           NAME == "Koyukuk" | NAME == "Stikine")

## Convert predictions' data format for better mapping ##
# Step 1: convert df to sf object
pred1V <- st_as_sf(est1_V, coords = c("Lon", "Lat"), crs = 4326)
pred1G <- st_as_sf(est1_G, coords = c("Lon", "Lat"), crs = 4326)
predV <- st_as_sf(est3_V, coords = c("Lon", "Lat"), crs = 4326)
predG <- st_as_sf(est4_G, coords = c("Lon", "Lat"), crs = 4326)

# Step 2: convert coordinate system of sf objects, 
# then convert back to dataframe for alpha plotting
# VAST model 1
a <- st_transform(pred1V, crs=3338)
b <- data.frame(st_coordinates(a))
pred1V <- data.frame(Longitude = b$X, 
                    Latitude = b$Y,
                    Fit = a$Fit,
                    CV = a$CV)
# GAM model 1
a <- st_transform(pred1G, crs=3338)
b <- data.frame(st_coordinates(a))
pred1G <- data.frame(Longitude = b$X, 
                     Latitude = b$Y,
                     Fit = a$Fit,
                     CV = a$CV)

# VAST model 3
a <- st_transform(predV, crs=3338)
b <- data.frame(st_coordinates(a))
predV <- data.frame(Longitude = b$X, 
                 Latitude = b$Y,
                 Fit = a$Fit,
                 CV = a$CV,
                 Year = a$Year)

# GAM model 3
a <- st_transform(predG, crs=3338)
b <- data.frame(st_coordinates(a))
predG <- data.frame(Longitude = b$X,
                    Latitude = b$Y,
                    Fit = a$Fit,
                    CV = a$CV,
                    Year = a$Year)

#### Mapping ####
## VAST Model 1 Map ##
ggplot() +
  geom_point(data = pred1V, mapping = aes(x=Longitude, y=Latitude,
                                         color = log(Fit)), alpha = (1-pred1V$CV)/2) +
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
ggsave(device = "jpeg", height = 10.5, width = 8,
       path = dir.fig, filename = paste0(spec,"_VAST_mod1_combo.jpeg"))

## GAM Model 1 Map ##
ggplot() +
  geom_point(data = pred1G, mapping = aes(x=Longitude, y=Latitude,
                                          color = Fit, alpha = (1-CV)/2)) +
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
ggsave(device = "jpeg", height = 10.5, width = 8,
       path = dir.fig, filename = paste0(spec,"_GAM_mod1_combol.jpeg"))

# VAST Model 3 map
ggplot() +
  geom_point(data = predV, mapping = aes(x=Longitude, y=Latitude,
                                      color = log(Fit)), alpha = (1-predV$CV)/2) +
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
  facet_wrap(~Year, ncol = 4) +
  labs(x = "Longitude", y = "Latitude") +
  theme(axis.title=element_text(size=14), 
        strip.text=element_text(size=14))

#Save to output folder
ggsave(device = "jpeg", height = 10.5, width = 8,
       path = dir.fig, filename = paste0(spec,"_VAST_mod3_all.jpeg"))

# GAM model 3 map
ggplot() +
  geom_point(data = predG, mapping = aes(x=Longitude, y=Latitude,
                                         color = Fit), alpha = (1-predG$CV)/2) +
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
  facet_wrap(~Year, ncol = 4) +
  labs(x = "Longitude", y = "Latitude") +
  theme(axis.title=element_text(size=14), 
        strip.text=element_text(size=14))

#Save to output folder
ggsave(device = "jpeg", height = 10.5, width = 8,
       path = dir.fig, filename = paste0(spec,"_GAM_mod4_all.jpeg"))


