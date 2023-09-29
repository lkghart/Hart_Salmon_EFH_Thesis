#### Converting EFH prediction points to polygons for mapping ####
# author: Lilian Hart
# date last edited: 09/11/23

#Problem: To create multipart polygons of 50% core EFH quickly from prediction 
# point dataframes. Dynamic model output includes independent yearly spatial fields
# representing years 2002-2019.

# Question: How to create multiple polygons (concave hulls) from point data
# more efficiently than described in script below?

require(tidyverse)
require(dplyr)
require(here)
require(viridis)
require(ggmap)
require(ggplot2)
require(rnaturalearth)
require(rnaturalearthhires)
require(sf)
require(rnaturalearthdata)
require(ggspatial)
require(concaveman)
require(geosphere)
require(sp)
require(rgdal)

spec <- "Sockeye"
spec2 <- "sockeye"

## Set directories
dir.data <- here("data", "Chapter_1_EFH")
dir.work <- here("data", "Chapter_1_RDSModels")

#### Load model predictions of EFH and core EFH ####
# Model 1 
G_1 <- readRDS(file.path(dir.data, paste0(spec, "_GAM_Mod1_EFH.rds")))
G_150 <- readRDS(file.path(dir.data, paste0(spec, "_GAM_Mod1_EFH_50.rds")))
V_1 <- readRDS(file.path(dir.data, paste0(spec, "_VAST_Mod1_EFH.rds")))
V_150 <- readRDS(file.path(dir.data, paste0(spec, "_VAST_Mod1_EFH_50.rds")))
# Model 4 (independent yearly fields)
G_4 <- readRDS(file.path(dir.data, paste0(spec, "_GAM_Mod3_EFH.rds")))
G_450 <- readRDS(file.path(dir.data, paste0(spec, "_GAM_Mod3_EFH_50.rds")))
V_4 <- readRDS(file.path(dir.data, paste0(spec, "_VAST_Mod3_EFH.rds")))
V_450 <- readRDS(file.path(dir.data, paste0(spec, "_VAST_Mod3_EFH_50.rds")))

# Convert to shapefile, then plot basic maps
G_1_sf <- st_as_sf(G_1, coords = c("Lon", "Lat"), crs = 4326)
G_150_sf <- st_as_sf(G_150, coords = c("Lon", "Lat"), crs = 4326)
V_1_sf <- st_as_sf(V_1, coords = c("Lon", "Lat"), crs = 4326)
V_150_sf <- st_as_sf(V_150, coords = c("Lon", "Lat"), crs = 4326)
#G_4_sf <- st_as_sf(G_4, coords = c("Lon", "Lat"), crs = 4326)
#G_450_sf <- st_as_sf(G_450, coords = c("Lon", "Lat"), crs = 4326)
V_4_sf <- st_as_sf(V_4, coords = c("Lon", "Lat"), crs = 4326)
V_450_sf <- st_as_sf(V_450, coords = c("Lon", "Lat"), crs = 4326)

#### Optional: Find out number of clusters per year in Model 4 predictions ####
ggplot() +
  geom_sf(data = V_450_sf, color = alpha("#d7191c",0.3), size = 0.2) +
  coord_sf(crs = st_crs(3338), xlim = c(-1250000, -150000), 
           ylim = c(400000, 1900000), expand = FALSE, 
           datum = st_crs(4326))+
  facet_wrap(~Year, ncol=6) +
  scale_x_continuous(breaks = c(seq(-180 , -125, by = 7))) +
  theme(axis.title=element_text(size=18),
        axis.text=element_text(size=15),
        strip.text=element_text(size=18),
        title = element_text(size = 18)) 

#### 1: Specify GAM or VAST dataset ####
# Subset prediction dataframe by sample year. Run clustering algorithm on 
# each dataframe, setting cluster number high to capture fine scale
# differences across variety of conditions.
# Export cluster data out to prediction subset.
dat <- G_450
title <- "G_450"
#now <- 2019
k <- 5
n <- 18
years <- 2002:2019

#### 2: Classify clusters ####
for (i in 1:n){
  now <- years[i]
  #### Delineate clusters ####
  pred <- dat %>% filter(Year == now) %>% drop_na()
  locs <- pred %>% select(Lat,Lon)
  # Convert data to a SpatialPointsDataFrame object
  xy <- SpatialPointsDataFrame(
    matrix(c(locs$Lon,locs$Lat), ncol=2), data.frame(ID=seq(1:length(locs$Lon))),
    proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
  # Use the distm function to generate a geodesic distance matrix in meters
  mdist <- distm(xy)
  # Cluster all points using a hierarchical clustering approach
  hc <- hclust(as.dist(mdist), method="complete")
  # define the distance threshold, in this case 40 m
  d=40
  # Define clusters based on a tree "height" cutoff "d", number of clusters "k"
  xy$clust <- cutree(hc, h=d, k = k)
  # Add clusters info to prediction data frame
  pred$clust <- as.factor(xy$clust)
  # Convert to sf object
  pred_sf <- st_as_sf(pred, coords = c("Lon", "Lat"), crs = 4326)
  # Test cluster classification by mapping
  ggplot() +
    geom_sf(data = pred_sf, aes(col = clust)) +
    coord_sf(crs = st_crs(3338), xlim = c(-1250000, -150000), 
             ylim = c(400000, 1900000), expand = FALSE, 
             datum = st_crs(4326))+
    scale_x_continuous(breaks = c(seq(-180 , -125, by = 7))) +
    theme(axis.title=element_text(size=18),
          axis.text=element_text(size=15),
          strip.text=element_text(size=18),
          title = element_text(size = 18)) 
  
  #### Split clusters then run concave hull function to make polygon ####
  split <- split(pred_sf, pred_sf$clust)
  split_sf <- lapply(split, st_as_sf, coords = c("Lon", "Lat"))
  concave <- lapply(split_sf, concaveman, concavity = 7)
  concave_bound <- do.call('rbind', concave)
  # Inspect polygons
  ggplot()+
    geom_sf(data = pred_sf, aes(col = clust)) +
    geom_sf(data = concave_bound$polygons, fill = "blue", alpha = 0.3) +
    coord_sf(crs = st_crs(3338), xlim = c(-1250000, -150000), 
             ylim = c(400000, 1900000), expand = FALSE, 
             datum = st_crs(4326))+
    scale_x_continuous(breaks = c(seq(-180 , -125, by = 7))) +
    theme(axis.title=element_text(size=18),
          axis.text=element_text(size=15),
          strip.text=element_text(size=18),
          title = element_text(size = 18))
  #### Save out ####
  setwd(dir.data)
  saveRDS(concave_bound, file = paste0(spec,"_", title, "_", now, ".rds")) 
}

#### Check work ####
test <- readRDS(file.path(dir.data, paste0(spec, "_", title, "_", "2002.rds")))
ggplot() +
  geom_sf(data = test, fill = "red", alpha = 0.5, col = "transparent") +
  coord_sf(crs = st_crs(3338), xlim = c(-1250000, -150000), 
           ylim = c(400000, 1900000), expand = FALSE, 
           datum = st_crs(4326))+
  scale_x_continuous(breaks = c(seq(-180 , -125, by = 7))) +
  theme(axis.title=element_text(size=18),
        axis.text=element_text(size=15),
        strip.text=element_text(size=18),
        title = element_text(size = 18)) 
