### Clip official EFH to user region ###
# author: Lilian Hart
# date last edited: 02/17/23

require(sp)
require(tidyverse)
require(dplyr)
require(sf)
require(here)
require(ggspatial)
require(ggplot2)
require(rgdal)

library(rnaturalearth)
library(rnaturalearthhires)
library(rnaturalearthdata)

dir.data <- file.path("~/Documents/AK_Shapefiles/WGS_1984")
dir.work <- here("data", "Chapter_1_RDSModels")
dir.efh <- here("data", "Chapter_1_EFH")

Species <- c("Chinook", "Chum", "Pink", "Sockeye")
species <- c("chinook", "chum", "pink", "sockeye")
# Load in user region polygon 
aoi <- readRDS(file.path(dir.work, "region2_polygon.rds"))

setwd(dir.efh)

for (i in 1:4){
  Spec <- Species[i]
  spec <- species[i]
  # Load in EFH shapefiles
  # Be sure to NOT include ".shp" in the layer name.
  spec_shp <- read_sf(dsn = dir.data, layer = paste0(Spec,"_juvenile_EFH_Level1"))
  # Set/reaffirm CRS the modern way for EFH shapefiles
  st_crs(spec_shp) <- 3857
  # Set CRS or transform from geographic coordinate system (WGS84) to projected
  # (flat) coordinate system of UTM zone 33 N
  x <- st_transform(spec_shp, 32633)
  aoi <- st_transform(aoi, 32633)
  
  # Clip using the sf intersect method
  spec_subset <- st_intersection(x, aoi)
  # Save to RDS
  saveRDS(spec_subset, paste0("official_", spec, "_EFH_clipped.rds"))
}

# Examine clipped official EFH sf in space
usa <- ne_states("United States of America", returnclass = "sf")
ak <- subset(usa, name == "Alaska")
ak <- st_set_crs(ak, 4326)

chinook <- readRDS("official_chinook_EFH_clipped.rds")
chum <- readRDS("official_chum_EFH_clipped.rds")
pink<- readRDS("official_pink_EFH_clipped.rds")
sockeye <- readRDS("official_sockeye_EFH_clipped.rds")

# Map
ggplot() +
  geom_sf(data = ak) +
  # geom_sf(data = chinook, color = "blue", fill = NA) +
  # geom_sf(data = chum, color = "white", fill = NA) +
  geom_sf(data = spec_shp, color = "red", fill = "red") +
  # geom_sf(data = sockeye, color = "orange", fill = NA) +
  # geom_sf(data = aoi, color = "red", fill = NA) 
  coord_sf(crs = st_crs(3338), xlim = c(-1250000, -100000),
           ylim = c(400000, 2000000), expand = FALSE,
           datum = st_crs(4326))
