#### Converting EFH prediction points to polygons for mapping ####
# author: Lilian Hart
# date last edited: 09/11/23

#QProblem: To create multipart polygons of 50% core EFH quickly from prediction 
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
require(stars)
require(rnaturalearthdata)
require(ggspatial)
require(concaveman)
require(geosphere)
require(raster)

spec <- "Chinook"
spec2 <- "chinook"

## Set directories
dir.data <- here("data", "Chapter_1_EFH")
dir.work <- here("data", "Chapter_1_RDSModels")

## Load model predictions of EFH and core EFH
# Model 1 
G_1 <- readRDS(file.path(dir.data, paste0(spec, "_GAM_Mod1_EFH.rds")))
G_150 <- readRDS(file.path(dir.data, paste0(spec, "_GAM_Mod1_EFH_50.rds")))
V_1 <- readRDS(file.path(dir.data, paste0(spec, "_VAST_Mod1_EFH.rds")))
V_150 <- readRDS(file.path(dir.data, paste0(spec, "_VAST_Mod1_EFH_50.rds")))
# Model 4 (independent yearly fields)
#G_4 <- readRDS(file.path(dir.data, paste0(spec, "_GAM_Mod3_EFH.rds")))
#G_450 <- readRDS(file.path(dir.data, paste0(spec, "_GAM_Mod3_EFH_50.rds")))
V_4 <- readRDS(file.path(dir.data, paste0(spec, "_VAST_Mod3_EFH.rds")))
V_450 <- readRDS(file.path(dir.data, paste0(spec, "_VAST_Mod3_EFH_50.rds")))

# Convert to shapefiles
G_1_sf <- st_as_sf(G_1, coords = c("Lon", "Lat"), crs = 4326)
G_150_sf <- st_as_sf(G_150, coords = c("Lon", "Lat"), crs = 4326)
V_1_sf <- st_as_sf(V_1, coords = c("Lon", "Lat"), crs = 4326)
V_150_sf <- st_as_sf(V_150, coords = c("Lon", "Lat"), crs = 4326)
#G_4_sf <- st_as_sf(G_4, coords = c("Lon", "Lat"), crs = 4326)
#G_450_sf <- st_as_sf(G_450, coords = c("Lon", "Lat"), crs = 4326)
V_4_sf <- st_as_sf(V_4, coords = c("Lon", "Lat"), crs = 4326)
V_450_sf <- st_as_sf(V_450, coords = c("Lon", "Lat"), crs = 4326)

#### Turn point files into rasters ####
G_150_rast <- st_rasterize(G_150_sf %>% dplyr::select(Fit, geometry))
setwd(dir.data)
write_stars(G_150_rast, "G_150_rast.tif")

#### Turn raster boundaries into polygons ####
test <- st_contour(G_150_rast, contour_lines = T)
pre <- G_150_sf %>% drop_na()
test1 <- raster(pre)
test2 <- rasterToContour(test1)
test3 <- concaveman(test2, c = 2)
#### Compute cluster groups manually by species ####

#Red is GAM, Blue is VAST
ggplot() +
  geom_sf(data = G_150_sf, color = alpha("#d7191c",0.3), size = 0.2) +
  geom_sf(data = V_150_sf, color = alpha("#2b83ba",0.6), size = 0.2) +
  coord_sf(crs = st_crs(3338), xlim = c(-1250000, -150000), 
           ylim = c(400000, 1900000), expand = FALSE, 
           datum = st_crs(4326))+
  scale_x_continuous(breaks = c(seq(-180 , -125, by = 7))) +
  theme(axis.title=element_text(size=18),
        axis.text=element_text(size=15),
        strip.text=element_text(size=18),
        title = element_text(size = 18)) +
  labs(x = "Longitude", y = "Latitude", 
       title = paste(spec,"salmon"))

# Dynamic (independent yearly field) model
# Plot one set at a time, then note how many clusters per year
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

# Clustering procedure: 
# Subset prediction dataframe by sample year. Run clustering algorithm on 
# each dataframe.Export cluster data out to prediction subset.
dat <- V_450
now <- 2002

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
d=10
# Define clusters based on a tree "height" cutoff "d", number of clusters "k"
xy$clust <- cutree(hc, h=d, k = 3)
# Add clusters info to prediction dataframe
pred$clust <- as.factor(xy$clust)
# Convert to sf object
pred_sf <- st_as_sf(pred, coords = c("Lon", "Lat"), crs = 4326)

# Test clustering by mapping
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

# Use concaveman to compute concave hull polygons from each sf object
n <- 3
for (i in 1:n){
  roup <- as.factor(i)
  print(roup)
  luster <- pred_sf %>% filter(clust == i)
  l_out <- concaveman(luster, concavity = 2)
}
