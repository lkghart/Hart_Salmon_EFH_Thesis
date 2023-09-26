### Step C: Clip BASIS point data to AOI ###
# author: Lilian Hart
# date last edited: 09/22/23

require(sp)
require(tidyverse)
require(dplyr)
require(sf)
require(here)
require(ggspatial)
require(ggplot2)
require(rgdal) # This package is key for clipping!

# Check that GDAL and proj are updated to at least 3 and 6, respectively
sf::sf_extSoftVersion()

dir.data <- here("data", "BASIS")
dir.mod <- here("data", "Chapter_1_RDS")

# Load in survey data
basis <- readRDS(file.path(dir.data, "full_basis_combo_data"))
# Clean data
basis <- basis %>% drop_na(CommonName,
                           Effort_area_km2,
                           TotalCatchNum)
# Convert to shapefile
bpoints <- st_as_sf(basis, coords = c("EQ.Longitude", "EQ.Latitude"))

# Load in user region polygon and convert to shapefile
aoi <- readRDS(file.path(dir.mod, "AOI_polygon.rds"))
aoi <- st_as_sf(aoi)

# Set CRS the modern way (proj4 string is outdated)
st_crs(bpoints) <- 4326
st_crs(aoi) <- 4326

# Clip using the indexing method 
basis_subset <- bpoints[aoi,]
plot(basis_subset)

# Revert to dataframe. First retrieve coordinates from geometry.
coords <- as.data.frame(st_coordinates(basis_subset))
basisfin <- basis_subset
basisfin$EQ.Longitude <-  coords$X
basisfin$EQ.Latitude <- coords$Y

# Remove geometry, then save this clipped BASIS subset for modeling and predicting
basisfin <- as.data.frame(basisfin)
basisfin <- subset(basisfin, select = -geometry)
setwd(dir.mod)
saveRDS(basisfin, "basis_subset.rds")
