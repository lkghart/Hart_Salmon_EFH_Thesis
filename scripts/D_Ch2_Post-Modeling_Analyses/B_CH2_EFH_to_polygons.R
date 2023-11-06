#### Converting static EFH (Model 1) prediction points to polygons for mapping ####
# author: Lilian Hart
# date last edited: 11/06/23

require(tidyverse)
require(dplyr)
require(here)
require(viridis)
require(ggmap)
require(ggplot2)
require(rnaturalearth)
require(rnaturalearthhires)
require(sf)
require(sp)
require(rnaturalearthdata)
require(ggspatial)
require(concaveman)
require(geosphere)

Spec <- "Chum"
spec <- tolower(Spec)

## Set directories
dir.data <- here("data", "Chapter_2_EFH")
dir.work <- here("data", "Chapter_2_RDSModels")

#### Load model predictions of 95% EFH and core EFH ####
# Model 1 
G_1 <- readRDS(file.path(dir.data, paste0(spec, "_gam_in-situ_EFH.rds")))
G_150 <- readRDS(file.path(dir.data, paste0(spec, "_gam_in-situ_EFH_50.rds")))

# Convert to shapefile
G_1_sf <- st_as_sf(G_1, coords = c("Lon", "Lat"), crs = 4326)
G_150_sf <- st_as_sf(G_150, coords = c("Lon", "Lat"), crs = 4326)

## Polygonize 95% EFH ##
setwd(dir.data)
efhpol <- concaveman(G_1_sf, concavity = 2)
saveRDS(concaveman, file = paste0(spec,"_in-situ_EFH95_poly.rds") )

# Test concavity by mapping
ggplot() +
  geom_sf(data = efhpol) +
  geom_sf(data = G_1_sf, color = "blue", size = 0.2) +
  coord_sf(crs = st_crs(3338), xlim = c(-1250000, -150000), 
           ylim = c(400000, 1900000), expand = FALSE, 
           datum = st_crs(4326)) + 
  scale_x_continuous(breaks = c(seq(-180 , -125, by = 7))) +
  theme(axis.title=element_text(size=18),
        axis.text=element_text(size=15),
        strip.text=element_text(size=18),
        title = element_text(size = 18)) 

#### 2: Polygonize core 50% EFH ####
# Subset prediction dataframe by sample year. Run clustering algorithm on 
# each dataframe, setting cluster number high to capture fine scale
# differences across variety of conditions.
# Export cluster data out to prediction subset.
dat <- G_150
title <- "G_150"
k <- 2
pred <- dat
locs <- dat %>% select(Lat,Lon)
# Convert data to a SpatialPointsDataFrame object
xy <- SpatialPointsDataFrame(
  matrix(c(locs$Lon,locs$Lat), ncol=2), data.frame(ID=seq(1:length(locs$Lon))),
  proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
# Use the distm function to generate a geodesic distance matrix in meters
mdist <- distm(xy)
# Cluster all points using a hierarchical clustering approach
hc <- hclust(as.dist(mdist), method="complete")
# define the distance threshold, in this case 40 m
d=10
# Define clusters based on a tree "height" cutoff "d", number of clusters "k"
xy$clust <- cutree(hc, h=d, k = k)
# Add clusters info to prediction data frame
pred$clust <- as.factor(xy$clust)
# Convert to sf object
pred_sf <- st_as_sf(pred, coords = c("Lon", "Lat"), crs = 4326)
  
## Split clusters then run concave hull function to make polygon ##
split <- split(pred_sf, pred_sf$clust)
split_sf <- lapply(split, st_as_sf, coords = c("Lon", "Lat"))
concave <- lapply(split_sf, concaveman, concavity = 3)
concave_bound <- do.call('rbind', concave)
# Inspect polygons
ggplot()+ geom_sf(data = pred_sf, aes(col = clust)) +
  geom_sf(data = concave_bound$polygons, fill = "blue", alpha = 0.3) +
  coord_sf(crs = st_crs(3338), xlim = c(-1250000, -150000), 
             ylim = c(400000, 1900000), expand = FALSE, 
             datum = st_crs(4326))+
  scale_x_continuous(breaks = c(seq(-180 , -125, by = 7))) +
  theme(axis.title=element_text(size=18),
          axis.text=element_text(size=15),
          strip.text=element_text(size=18),
          title = element_text(size = 18))
## Save out ##
setwd(dir.data)
saveRDS(concave_bound, file = paste0(spec,"_", title, "_poly.rds")) 
