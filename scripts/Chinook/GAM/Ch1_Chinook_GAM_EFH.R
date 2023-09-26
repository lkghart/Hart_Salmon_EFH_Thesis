### Chapter 1 Chinook GAM Essential Fish Habitat Maps ###
# author: Lilian Hart
# last edited: 10/20/2022

require(tidyverse)
require(dplyr)
require(ggthemes)
require(here)
require(units)
require(ggplot2)
require(ggmap)
require(rnaturalearth)
require(rnaturalearthhires)
require(sf)
require(rnaturalearthdata)
require(ggspatial)
require(viridis)

## Setup and read in data ##
dir.data <- here("data", "Chapter_1_RDSModels")
dir.work <- here("figs")

species <- "Chinook"

#Loading static model
pred1 <- readRDS(file.path(dir.data,paste0(species,"_GAM_Mod1_Predictions.rds")))

## Categorize prediction points based on 95th percentile
# Examine distribution of predicted values
hist(pred1$Fit)

# Calculate 5th percentile/quantile
tile <- quantile(pred1$Fit,0.05) 
quantile(pred1$Fit, 0.95) 

# Create dataframe with a new column that says NA if below the 5th 
# percentile, or gives the estimated Fit if above.
dat <- pred1 %>% mutate(core = ifelse(Fit > tile, Fit, NA))

## Plot 95% Occurrence map
# Load in mapping attributes
usa <- ne_states("United States of America", returnclass = "sf")
ak <- subset(usa, name == "Alaska")
ak <- st_set_crs(ak, 4326)
russia <-rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")%>%
  filter(name=="Russia")
russia <- st_set_crs(russia, 4326)
# Save data as sf object
dat <- st_as_sf(dat, coords = c("Lon", "Lat"), crs = 4326)

ggplot() +
  geom_sf(data = dat, mapping = aes(color = core)) +
  geom_sf(data = ak) +
  geom_sf(data = russia) +
  coord_sf(crs = st_crs(3338), xlim = c(-2280000, 180000), 
           ylim = c(50000, 2800000), expand = FALSE, 
           datum = st_crs(4326))+
  scale_x_continuous(breaks = c(seq(-180 , -125, by = 7)))+
  scale_color_viridis(name = "count") +
  labs(x = "Longitude", y = "Latitude",
       title = paste("Juvenile", species, "- 95th Quantile"))

# Weeding out numbers smaller than 1
dat2 <- pred1 %>% mutate(core = ifelse(Fit > tile & Fit >= exp(0), Fit, NA))
dat2 <- st_as_sf(dat2, coords = c("Lon", "Lat"), crs = 4326)
ggplot() +
  geom_sf(data = dat2, mapping = aes(color = core)) +
  geom_sf(data = ak) +
  coord_sf(crs = st_crs(3338), xlim = c(-2280000, 180000), 
           ylim = c(50000, 2800000), expand = FALSE, 
           datum = st_crs(4326))+
  scale_x_continuous(breaks = c(seq(-180 , -125, by = 7)))+
  scale_color_viridis(name = "log(count)") +
  labs(x = "Longitude", y = "Latitude", 
       title = paste("Juvenile", species, "- 95% Occurrence",
                     fill = "Density"))
                        