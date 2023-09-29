### Chapter 2 GAM Prediction Surface Maps
## Author: Lilian Hart 
## Last edited: 08/21/23

require(tidyverse)
require(dplyr)
require(here)
require(viridis)
require(mgcv)
require(visreg)
require(beepr)
library(rnaturalearth)
library(rnaturalearthhires)
library(gganimate)
library(sf)
library(rnaturalearthdata)
require(ggspatial)

#### Setup ####
Spec <- "Coho"
spec <- "coho"
dir.mod <- here("data", "Chapter_2_RDS")
setwd(dir.work)
dir.fig <- file.path("~/Documents/Salmon_EFH_Deliverables/Chapter2_Figures")
dir.shelf <- file.path("~/Documents/AK_Shapefiles")

## Load predictions
# In-situ Model (1A)
setwd(dir.mod)
in_situ <- readRDS(file.path(dir.mod, paste0(spec,"_gam_in-situ_link_predictions.rds")))

## Set working directory to figures folder
setwd(dir.fig)

## Load in mapping attributes
usa <- ne_states("United States of America", returnclass = "sf")
ak <- subset(usa, name == "Alaska")
ak <- st_set_crs(ak, 4326)
russia <-rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")%>%
  filter(name=="Russia")
russia <- st_set_crs(russia, 4326)
rivers <- st_read(file.path(dir.shelf, "USA_Major_Rivers/v10/rivers.gdb"))
riv <- rivers %>% filter(NAME == "Kuskokwim" | NAME == "Yukon" |
                           NAME == "Koyukuk" | NAME == "Stikine")
shelf <- st_read(file.path(dir.shelf, "arctic_coast_bathy/arctic_coast_bathy.shp"))
shelfb <- select(shelf, elevation, geometry) %>% 
  filter(elevation == -105 | elevation == -55)
shelfb <- st_transform(shelfb, 4326)
cshelf <- st_read(file.path(dir.shelf, "AK_CSB.gdb"))

## Convert predictions to sf objects

# In-situ model (1A)
in_situ_df <- data.frame(Fit = in_situ$fit, Lon = in_situ$Lon, Lat = in_situ$Lat, 
                         CV = in_situ$CV)
in_situ_sf <- st_as_sf(in_situ_df, coords = c("Lon", "Lat"), crs = 4326)

#### In-situ Maps ####
ggplot() +
  geom_sf(data = in_situ_sf, aes(color = Fit)) +
  geom_sf(data = russia, fill = "#CCCCCC") +
  scale_color_viridis(name = "Log(count)") +
  geom_sf(data = ak, fill = "#CCCCCC") +
  geom_sf(data = shelfb, color = "white", fill = NA) +
  geom_sf(data = cshelf, color = "white") +
  geom_sf(data = riv, color = "#666262") +
  coord_sf(crs = st_crs(3338), xlim = c(-1250000, -100000), 
           ylim = c(400000, 2000000), expand = FALSE, 
           datum = st_crs(4326))+
  scale_x_continuous(breaks = c(seq(-180 , -125, by = 7)))+
  annotate("text", label = "100 m", x=-900000, y=750000, color = "white",
           size = 4) +
  annotate("text", label = "50 m", x=-550000, y=770000, color = "white",
           size = 4) +
  annotate("text", label = "Continental Shelf", x=-1550000, y=1250000, 
           color = "white", size = 4, angle = 25) +
  annotate("text", label = "Yukon", x=-240000, y=1500000, color = "black",
           size = 4, angle = 57) +
  annotate("text", label = "Kuskokwim", x=-270000, y=1240000, color = "black",
           size = 4, angle = 22) +
  annotate("text", label = "Bristol\n Bay", x=-340000, y=870000, color = "black",
           size = 4) +
  annotate("text", label = "Unimak Pass", x=-650000, y=450000, color = "black",
           size = 4) +
  theme(axis.title=element_text(size=18),
        axis.text=element_text(size=15),
        strip.text=element_text(size=18),
        title = element_text(size = 18)) +
  labs(x = "Longitude", y = "Latitude", 
       title = paste("Juvenile",spec,"salmon\n In-Situ Covar. Model"))

ggsave(paste0(Spec,"_prediction_surface.jpeg"), width = 5, height = 5, units = "in")

