### Chapter 1 EFH Maps ###
# author: Lilian Hart
# date last modified: 03/03/23

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

dir.work <- here("data", "Chapter_1_EFH")
setwd(dir.work)

#### Workflow ####
Spec <- "Sockeye"
spec <- "sockeye"

#### Load in predictions ####
gam1 <- readRDS(file.path(dir.work, paste0(Spec, "_GAM_Mod1_EFH.rds"))) 
gam3 <- readRDS(file.path(dir.work, paste0(Spec, "_GAM_Mod3_EFH.rds")))
vast1 <- readRDS(file.path(dir.work, paste0(Spec, "_VAST_Mod1_EFH.rds"))) 
vast3 <- readRDS(file.path(dir.work, paste0(Spec, "_VAST_Mod3_EFH.rds")))
gam1b <- readRDS(file.path(dir.work, paste0(Spec, "_GAM_Mod1_EFH_50.rds"))) 
gam3b <- readRDS(file.path(dir.work, paste0(Spec, "_GAM_Mod3_EFH_50.rds")))
vast1b <- readRDS(file.path(dir.work, paste0(Spec, "_VAST_Mod1_EFH_50.rds"))) 
vast3b <- readRDS(file.path(dir.work, paste0(Spec, "_VAST_Mod3_EFH_50.rds")))

# Convert to sf objects
gam1 <- st_as_sf(gam1, coords = c("Lon", "Lat"), crs = 4326)
gam3 <- st_as_sf(gam3, coords = c("Lon", "Lat"), crs = 4326)
vast1 <- st_as_sf(vast1, coords = c("Lon", "Lat"), crs = 4326)
vast3 <- st_as_sf(vast3, coords = c("Lon", "Lat"), crs = 4326)
gam1b <- st_as_sf(gam1b, coords = c("Lon", "Lat"), crs = 4326)
gam3b <- st_as_sf(gam3b, coords = c("Lon", "Lat"), crs = 4326)
vast1b <- st_as_sf(vast1b, coords = c("Lon", "Lat"), crs = 4326)
vast3b <- st_as_sf(vast3b, coords = c("Lon", "Lat"), crs = 4326)

dir.fig <- file.path("~/Documents/Salmon_EFH_Deliverables/Chapter1_Figures/V4_Figs")
dir.shelf <- file.path("~/Documents/AK_Shapefiles")
setwd(dir.fig)

#### Load in mapping attributes ####
usa <- ne_states("United States of America", returnclass = "sf")
ak <- subset(usa, name == "Alaska")
ak <- st_set_crs(ak, 4326)
russia <-rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")%>%
  filter(name=="Russia")
russia <- st_set_crs(russia, 4326)
rivers <- st_read(file.path(dir.shelf, "USA_Major_Rivers/v10/rivers.gdb"))
riv <- rivers %>% filter(NAME == "Kuskokwim" | NAME == "Yukon" |
                           NAME == "Koyukuk" | NAME == "Stikine")

#### Model 1 Maps ####
# Mod 1 GAM
ggplot() +
  geom_sf(data = gam1, mapping = aes(color = log(Fit))) +
  geom_sf(data = russia, fill = "#CCCCCC") +
  scale_color_viridis(name = "Log(count)") +
  geom_sf(data = ak, fill = "#CCCCCC") +
  geom_sf(data = riv, color = "#666262") +
  coord_sf(crs = st_crs(3338), xlim = c(-1250000, -100000), 
           ylim = c(400000, 2000000), expand = FALSE, 
           datum = st_crs(4326))+
  scale_x_continuous(breaks = c(seq(-180 , -125, by = 7)))+
  theme(axis.title=element_text(size=14), 
        strip.text=element_text(size=14)) +
  labs(x = "Longitude", y = "Latitude", 
       title = paste("GAM juvenile",spec,"salmon average spatial field EFH"))

ggsave(paste0(Spec,"_GAM_Mod1_EFH.jpeg"), width = 5, height = 5, units = "in")


# Mod 1 VAST
ggplot() +
  geom_sf(data = vast1, mapping = aes(color = log(Fit))) +
  geom_sf(data = russia, fill = "#CCCCCC") +
  scale_color_viridis(name = "Log(count)") +
  geom_sf(data = ak, fill = "#CCCCCC") +
  geom_sf(data = riv, color = "#666262") +
  coord_sf(crs = st_crs(3338), xlim = c(-1250000, -100000), 
           ylim = c(400000, 2000000), expand = FALSE, 
           datum = st_crs(4326))+
  scale_x_continuous(breaks = c(seq(-180 , -125, by = 7)))+
  theme(axis.title=element_text(size=14), 
        strip.text=element_text(size=14)) +
  labs(x = "Longitude", y = "Latitude",
       title = paste("VAST juvenile",spec,"salmon average spatial field EFH"))

ggsave(paste0(Spec,"_VAST_Mod1_EFH.jpeg"), width = 5, height = 5, units = "in")

## Model 3/4 (best spatiotemporal model)
# GAM Mod 3
ggplot() +
  geom_sf(data = gam3, mapping = aes(color = log(Fit))) +
  geom_sf(data = russia, fill = "#CCCCCC") +
  scale_color_viridis(name = "Log(count)") +
  geom_sf(data = ak, fill = "#CCCCCC") +
  geom_sf(data = riv, color = "#666262") +
  coord_sf(crs = st_crs(3338), xlim = c(-1250000, -100000), 
           ylim = c(400000, 2000000), expand = FALSE, 
           datum = st_crs(4326))+
  scale_x_continuous(breaks = c(seq(-180 , -125, by = 7)))+
  facet_wrap(~Year, ncol = 5) +
  theme(axis.title=element_text(size=14), 
        strip.text=element_text(size=14)) +
  labs(x = "Longitude", y = "Latitude", 
       title = paste("GAM juvenile",Spec,"salmon model 3 EFH")) 

ggsave(paste0(Spec,"_GAM_Mod3_EFH.jpeg"), width = 10, height = 11,
       units = "in")

# VAST Mod 3
ggplot() +
  geom_sf(data = vast3, mapping = aes(color = log(Fit))) +
  geom_sf(data = russia, fill = "#CCCCCC") +
  scale_color_viridis(name = "Log(count)") +
  geom_sf(data = ak, fill = "#CCCCCC") +
  geom_sf(data = riv, color = "#666262") +
  coord_sf(crs = st_crs(3338), xlim = c(-1250000, -100000), 
           ylim = c(400000, 2000000), expand = FALSE, 
           datum = st_crs(4326))+
  scale_x_continuous(breaks = c(seq(-180 , -125, by = 7)))+
  facet_wrap(~Year, ncol = 5) +
  theme(axis.title=element_text(size=14), 
        strip.text=element_text(size=14)) +
  labs(x = "Longitude", y = "Latitude", 
       title = paste("VAST juvenile",Spec,"salmon model 3 EFH")) 

ggsave(paste0(Spec,"_VAST_Mod3_EFH.jpeg"), width = 10, height = 11,
       units = "in")

#### 50 % core habitat maps ####
## Model 1 Maps
# Mod 1 GAM
ggplot() +
  geom_sf(data = gam1b, mapping = aes(color = log(Fit))) +
  geom_sf(data = russia, fill = "#CCCCCC") +
  scale_color_viridis(name = "Log(count)") +
  geom_sf(data = ak, fill = "#CCCCCC") +
  geom_sf(data = riv, color = "#666262") +
  coord_sf(crs = st_crs(3338), xlim = c(-1250000, -100000), 
           ylim = c(400000, 2000000), expand = FALSE, 
           datum = st_crs(4326))+
  scale_x_continuous(breaks = c(seq(-180 , -125, by = 7)))+
  theme(axis.title=element_text(size=14), 
        strip.text=element_text(size=14)) +
  labs(x = "Longitude", y = "Latitude", 
       title = paste("GAM juvenile",spec,"salmon EFH core habitat"))

ggsave(paste0(Spec,"_GAM_Mod1_EFH_50.jpeg"), width = 5, height = 5, units = "in")


# Mod 1 VAST
ggplot() +
  geom_sf(data = vast1b, mapping = aes(color = log(Fit))) +
  geom_sf(data = russia, fill = "#CCCCCC") +
  scale_color_viridis(name = "Log(count)") +
  geom_sf(data = ak, fill = "#CCCCCC") +
  geom_sf(data = riv, color = "#666262") +
  coord_sf(crs = st_crs(3338), xlim = c(-1250000, -100000), 
           ylim = c(400000, 2000000), expand = FALSE, 
           datum = st_crs(4326))+
  scale_x_continuous(breaks = c(seq(-180 , -125, by = 7)))+
  theme(axis.title=element_text(size=14), 
        strip.text=element_text(size=14)) +
  labs(x = "Longitude", y = "Latitude",
       title = paste("VAST juvenile",spec,"salmon core EFH"))

ggsave(paste0(Spec,"_VAST_Mod1_EFH_50.jpeg"), width = 5, height = 5, units = "in")

## Model 3/4 (best spatiotemporal model)
# GAM Mod 3
ggplot() +
  geom_sf(data = gam3b, color = "#15A3A3") +
  geom_sf(data = russia, fill = "#CCCCCC") +
  geom_sf(data = ak, fill = "#CCCCCC") +
  geom_sf(data = riv, color = "#666262") +
  coord_sf(crs = st_crs(3338), xlim = c(-1250000, -100000), 
           ylim = c(400000, 2000000), expand = FALSE, 
           datum = st_crs(4326))+
  scale_x_continuous(breaks = c(seq(-180 , -125, by = 7)))+
  facet_wrap(~Year, ncol = 5) +
  theme(axis.title=element_text(size=14), 
        strip.text=element_text(size=14)) +
  labs(x = "Longitude", y = "Latitude", 
       title = paste("GAM juvenile",Spec,"salmon core EFH")) 

ggsave(paste0(Spec,"_GAM_Mod3_EFH_50.jpeg"), width = 10, height = 11,
       units = "in")

# VAST Mod 3
ggplot() +
  geom_sf(data = vast3b, color = "#15A3A3") +
  geom_sf(data = russia, fill = "#CCCCCC") +
  geom_sf(data = ak, fill = "#CCCCCC") +
  geom_sf(data = riv, color = "#666262") +
  coord_sf(crs = st_crs(3338), xlim = c(-1250000, -100000), 
           ylim = c(400000, 2000000), expand = FALSE, 
           datum = st_crs(4326))+
  scale_x_continuous(breaks = c(seq(-180 , -125, by = 7)))+
  facet_wrap(~Year, ncol = 5) +
  theme(axis.title=element_text(size=14), 
        strip.text=element_text(size=14)) +
  labs(x = "Longitude", y = "Latitude", 
       title = paste("VAST juvenile",Spec,"salmon core EFH")) 

ggsave(paste0(Spec,"_VAST_Mod3_EFH_50.jpeg"), width = 10, height = 11,
       units = "in")

#### Model-based EFH vs official EFH maps ####

# Load in official EFH shapefile
off_spec <- readRDS(file.path(dir.work, paste0("official_", spec, "_EFH_clipped.rds")))

# Map the GAM-based EFH (these are best static according to % Deviance explained)
ggplot() +
  geom_sf(data = gam1, color = "#17322e") +
  geom_sf(data = russia, fill = "#CCCCCC") +
  geom_sf(data = ak, fill = "#CCCCCC") +
  geom_sf(data = riv, color = "#666262") +
  geom_sf(data = off_spec, fill = NA, color = "#846246", linewidth = 1) +
  coord_sf(crs = st_crs(3338), xlim = c(-1250000, -100000), 
           ylim = c(400000, 2000000), expand = FALSE, 
           datum = st_crs(4326))+
  scale_x_continuous(breaks = c(seq(-180 , -125, by = 7)))+
  theme(axis.title=element_text(size=14), 
        strip.text=element_text(size=14)) +
  labs(x = "Longitude", y = "Latitude", 
       title = paste("Juvenile",spec,"EFH comparison"))

ggsave(paste0(Spec,"_EFH_comparison.jpeg"), width = 5, height = 5, units = "in")



