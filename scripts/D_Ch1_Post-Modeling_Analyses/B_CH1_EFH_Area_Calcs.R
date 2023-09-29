### Chapter 1 Static EFH Area calculations ###
# author: Lilian Hart
# date last modified: 09/29/23
# This script calculates the differences in area between official EFH shapefiles
# and concave hull polygons estimated from static (Model 1) model predictions.

require(tidyverse)
require(dplyr)
require(here)
require(viridis)
require(mgcv)
require(visreg)
require(beepr)
require(rnaturalearth)
require(rnaturalearthhires)
require(gganimate)
require(sf)
require(sp)
require(rgdal)
require(rnaturalearthdata)
require(ggspatial)
require(concaveman)
require(units)

### Workflow ###
dir.work <- here("data", "Chapter_1_EFH")
setwd(dir.work)
Species <- c("Chinook", "Chum", "Pink", "Sockeye")
species <- c("chinook", "chum", "pink", "sockeye")

EFH_areas <- data.frame()

for (i in 1:4) {
  Spec <- Species[i]
  spec <- species[i]
  
  # Load in EFH predictions and EFH shapefiles
  gam1 <- readRDS(file.path(dir.work, paste0(spec, "_GAM_Mod1_EFH.rds"))) 
  vast1 <- readRDS(file.path(dir.work, paste0(spec, "_VAST_Mod1_EFH.rds")))
  offic <- readRDS(file.path(dir.work, paste0("official_", spec, "_EFH_clipped.rds")))
  
  # Convert to sf objects
  gam1_sf <- st_as_sf(gam1, coords = c("Lon", "Lat"), crs = 4326)
  vast1_sf <- st_as_sf(vast1, coords = c("Lon", "Lat"), crs = 4326)
  
  # Use concaveman to compute concave polygon from sf object
  gam_poly <- concaveman( points = gam1_sf, concavity = 2)
  vast_poly <- concaveman( points = vast1_sf, concavity = 2)
  # Calculate area of the polygons
  gam_area <- st_area(gam_poly)
  vast_area <- st_area(vast_poly)
  off_area <- st_area(offic)
  
  # Create data frame to organize results
  results <- data.frame(Species = species[i], GAM_EFH_Area_km2 = gam_area/1000000,
                        VAST_EFH_Area_km2 = vast_area/1000000, 
                        Official_Clipped_EFH_Area_km2 = off_area/1000000) %>%
    drop_units()
  
  # Calculate percent change
  results$change_GAM <- (results$GAM_EFH_Area_km2/results$Official_Clipped_EFH_Area_km2) - 1
  results$change_VAST <- (results$VAST_EFH_Area_km2/results$Official_Clipped_EFH_Area_km2) - 1
  # Save out
  EFH_areas <- rbind(EFH_areas, results)
}

print(EFH_areas)
