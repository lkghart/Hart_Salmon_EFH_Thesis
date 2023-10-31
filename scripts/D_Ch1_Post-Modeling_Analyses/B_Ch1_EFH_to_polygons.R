#### Convert Static EFH prediction points to polygons for mapping ####
# author: Lilian Hart
# date last edited: 10/31/23

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

spec <- "Chinook"
spec2 <- "chinook"

## Set directories
dir.data <- here("data", "Chapter_1_EFH")
dir.work <- here("data", "Chapter_1_RDS")

#### Load model predictions of EFH and core EFH ####
# Model 1 
G_1 <- readRDS(file.path(dir.data, paste0(spec, "_GAM_Mod1_EFH.rds")))
V_1 <- readRDS(file.path(dir.data, paste0(spec, "_VAST_Mod1_EFH.rds")))

# Convert to shapefile
G_1_sf <- st_as_sf(G_1, coords = c("Lon", "Lat"), crs = 4326)
V_1_sf <- st_as_sf(V_1, coords = c("Lon", "Lat"), crs = 4326)

#### Convert to polygons and save ####
G1_poly <- concaveman(G_1_sf, concavity = 7)
saveRDS(G1_poly, file = file.path(dir.data, paste0(spec2,"_gam_mod1_EFH_poly.rds")))
V1_poly <- concaveman(V_1_sf, concavity = 7)
saveRDS(V1_poly, file = file.path(dir.data, paste0(spec2,"_vast_mod1_EFH_poly.rds")))
