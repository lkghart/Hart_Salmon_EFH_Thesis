#### Chapter 1 EFH Maps ####
# author: Lilian Hart
# date last edited: 11/02/23

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
require(beepr)
require(akgfmaps)

Spec <- "Chum"
spec <- tolower(Spec)

# Set directories
dir.data <- here("data", "Chapter_1_EFH")
dir.work <- here("data", "Chapter_1_RDS")
dir.shelf <- file.path("~/Documents/AK_Shapefiles")
dir.fig <- file.path("~/Documents/Salmon_EFH_Deliverables/Chapter1_Figures/Manuscript")

# Load official EFH shapefiles
official <- readRDS(file.path(dir.data, paste0("official_", spec, "_EFH_clipped.rds")))
# Load model prediction polygons of static 95% EFH
gam1 <- readRDS(file.path(dir.data, paste0(spec,"_gam_mod1_EFH_poly.rds")))
vast1 <- readRDS(file.path(dir.data, paste0(spec,"_vast_mod1_EFH_poly.rds")))

# Load point predictions 
EFH50_1_G <- readRDS(file.path(dir.data, paste0(spec, "_GAM_Mod1_EFH_50.rds")))
EFH50_1_V <- readRDS(file.path(dir.data, paste0(spec, "_VAST_Mod1_EFH_50.rds")))
EFH_4_G <- readRDS(file.path(dir.data, paste0(spec, "_GAM_Mod4_EFH.rds")))
EFH50_4_G <- readRDS(file.path(dir.data, paste0(spec, "_GAM_Mod4_EFH_50.rds")))
EFH_4_V <- readRDS(file.path(dir.data, paste0(spec, "_VAST_Mod4_EFH.rds")))
EFH50_4_V <- readRDS(file.path(dir.data, paste0(spec, "_VAST_Mod4_EFH_50.rds")))
# Convert VAST Year from int to factor
EFH_4_V$Year <- as.factor(EFH_4_V$Year)
EFH50_4_V$Year <- as.factor(EFH50_4_V$Year)

# Convert point predictions to shapefiles
G_150_sf <- st_as_sf(EFH50_1_G, coords = c("Lon", "Lat"), crs = 4326)
V_150_sf <- st_as_sf(EFH50_1_V, coords = c("Lon", "Lat"), crs = 4326)
G_4_sf <- st_as_sf(EFH_4_G, coords = c("Lon", "Lat"), crs = 4326)
G_450_sf <- st_as_sf(EFH50_4_G, coords = c("Lon", "Lat"), crs = 4326)
V_4_sf <- st_as_sf(EFH_4_V, coords = c("Lon", "Lat"), crs = 4326)
V_450_sf <- st_as_sf(EFH50_4_V, coords = c("Lon", "Lat"), crs = 4326)

#### Mapping attributes ####
usa <- ne_states("United States of America", returnclass = "sf")
ak <- subset(usa, name == "Alaska")
ak <- st_set_crs(ak, 4326)
russia <-rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")%>%
  filter(name=="Russia")
russia <- st_set_crs(russia, 4326)
shelf <- get_survey_bathymetry(select.region = "ebs", set.crs = 4326)
ebs_layers <- akgfmaps::get_base_layers(select.region = "ebs", set.crs = 4326)
shelfb <- ebs_layers$bathymetry
cshelf <- st_read(file.path(dir.shelf, "AK_CSB.gdb"))
# Major AK rivers
rivers <- st_read(file.path(dir.shelf, "USA_Major_Rivers/v10/rivers.gdb"))
riv <- rivers %>% filter(NAME == "Kuskokwim" | NAME == "Yukon" |
                           NAME == "Koyukuk" | NAME == "Stikine")

#### Plot Static EFH ####
# GAM is red, VAST is blue
ggplot() +
  geom_sf(data = gam1, fill = "#d7191c", alpha = 0.5, col = NA) +
  geom_sf(data = vast1, fill = "#2b83ba", alpha = 0.5, col = NA) +
  geom_sf(data = russia, fill = "#CCCCCC") +
  geom_sf(data = shelfb, color = "white", fill = NA) +
  geom_sf(data = cshelf, color = "white") +
  geom_sf(data = ak, fill = "#CCCCCC") +
  geom_sf(data = riv, color = "#666262") +
  geom_sf(data = official, fill = NA, col = "#abdda4", linewidth = 1, alpha = 0.5) +
  coord_sf(crs = st_crs(3338), xlim = c(-1250000, -100000), 
           ylim = c(400000, 2000000), expand = FALSE, 
           datum = st_crs(4326))+
  scale_x_continuous(breaks = c(seq(-180 , -125, by = 7))) +
  annotate("text", label = "50 m", x=-760000, y=1100000, color = "white",
           size = 3, angle = 310) +
  annotate("text", label = "100 m", x=-940000, y=980000, color = "white",
           size = 3, angle = 310) +
  annotate("text", label = "200 m", x=-1080000, y=930000, color = "white",
           size = 3, angle = 310) +
  annotate("text", label = "Norton Sound", x=-500000, y=1600000,
           color = "black", size = 3) +
  annotate("text", label = "St. Lawrence Isl.", x=-810000, y=1665000,
           color = "black", size = 3) +
  annotate("text", label = "St. Matthew Isl.", x=-980000, y=1350000,
           color = "black", size = 3) +
  annotate("text", label = "Nunivak Isl.", x=-750000, y=1240000,
           color = "black", size = 3) +
  annotate("text", label = "Yukon", x=-240000, y=1500000, color = "black",
           size = 3, angle = 57) +
  annotate("text", label = "Kuskokwim", x=-270000, y=1240000, color = "black",
           size = 3, angle = 22) +
  annotate("text", label = "Bristol\n Bay", x=-340000, y=870000, color = "black",
           size = 3) +
  annotate("text", label = "Unimak P.", x=-615000, y=480000, color = "black",
           size = 3, angle = 330) +
  annotate("text", label = "False Pass", x=-500000, y=495000,
           color = "black", size = 3, angle = 330) +
  theme(axis.title=element_text(size=18),
        axis.text=element_text(size=15),
        strip.text=element_text(size=18),
        title = element_text(size = 18)) +
  labs(x = "Longitude", y = "Latitude", 
       title = paste(Spec,"salmon"))

# Save figure 
ggsave(device = "jpeg", height = 5, width = 5,
       path = dir.fig, filename = paste0(spec,"_mod1_EFH_comparison.jpeg"))

#### Plot Static Core (50%) EFH ####
ggplot() +
  geom_sf(data = G_150_sf, color = alpha("#d7191c", 0.3), size = 0.3) +
  geom_sf(data = V_150_sf, color = alpha("#2b83ba", 0.3), size = 0.3) +
  geom_sf(data = gam1, fill = NA, col = "#d7191c", linewidth = 0.4, alpha = 0.5) +
  geom_sf(data = vast1, fill = NA, col = "#2b83ba", linewidth = 0.4, alpha = 0.5) +
  geom_sf(data = russia, fill = "#CCCCCC") +
  geom_sf(data = shelfb, color = "white", fill = NA) +
  geom_sf(data = cshelf, color = "white") +
  geom_sf(data = ak, fill = "#CCCCCC") +
  geom_sf(data = riv, color = "#666262") +
  coord_sf(crs = st_crs(3338), xlim = c(-1250000, -150000), 
           ylim = c(400000, 1900000), expand = FALSE, 
           datum = st_crs(4326))+
  scale_x_continuous(breaks = c(seq(-180 , -125, by = 7))) +
  annotate("text", label = "50 m", x=-760000, y=1100000, color = "white",
           size = 3, angle = 310) +
  annotate("text", label = "100 m", x=-940000, y=980000, color = "white",
           size = 3, angle = 310) +
  annotate("text", label = "200 m", x=-1080000, y=930000, color = "white",
           size = 3, angle = 310) +
  annotate("text", label = "Norton Sound", x=-500000, y=1600000,
           color = "black", size = 3) +
  annotate("text", label = "St. Lawrence Isl.", x=-810000, y=1665000,
           color = "black", size = 3) +
  annotate("text", label = "St. Matthew Isl.", x=-980000, y=1350000,
           color = "black", size = 3) +
  annotate("text", label = "Nunivak Isl.", x=-750000, y=1240000,
           color = "black", size = 3) +
  annotate("text", label = "Yukon", x=-240000, y=1500000, color = "black",
           size = 3, angle = 57) +
  annotate("text", label = "Kuskokwim", x=-270000, y=1240000, color = "black",
           size = 3, angle = 22) +
  annotate("text", label = "Bristol\n Bay", x=-340000, y=870000, color = "black",
           size = 3) +
  annotate("text", label = "Unimak P.", x=-615000, y=480000, color = "black",
           size = 3, angle = 330) +
  annotate("text", label = "False Pass", x=-500000, y=495000,
           color = "black", size = 3, angle = 330) +
  theme(axis.title=element_text(size=18),
        axis.text=element_text(size=15),
        strip.text=element_text(size=18),
        title = element_text(size = 18)) +
  labs(x = "Longitude", y = "Latitude", 
       title = paste(Spec,"salmon"))

# Save figure 
ggsave(device = "jpeg", height = 5, width = 5,
       path = dir.fig, filename = paste0(spec,"_mod150_EFH_comparison.jpeg"))

#### Plot Dynamic EFH (as points) ####
# GAM is red, VAST is blue
ggplot() +
  geom_sf(data = G_4_sf, color = alpha("#d7191c", 0.2), size = 0.3) +
  geom_sf(data = V_4_sf, color = alpha("#2b83ba", 0.3), size = 0.3) +
  facet_wrap(~Year) +
  geom_sf(data = gam1, fill = NA, col = "#d7191c", linewidth = 0.4, alpha = 0.5) +
  geom_sf(data = vast1, fill = NA, col = "#2b83ba", linewidth = 0.4, alpha = 0.5) +
  geom_sf(data = russia, fill = "#CCCCCC") +
  geom_sf(data = shelfb, color = "white", fill = NA) +
  geom_sf(data = cshelf, color = "white") +
  geom_sf(data = ak, fill = "#CCCCCC") +
  geom_sf(data = riv, color = "#666262") +
  coord_sf(crs = st_crs(3338), xlim = c(-1250000, -150000), 
           ylim = c(400000, 1900000), expand = FALSE, 
           datum = st_crs(4326))+
  scale_x_continuous(breaks = c(seq(-180 , -125, by = 7))) +
  annotate("text", label = "50 m", x=-760000, y=1100000, color = "white",
           size = 3, angle = 310) +
  annotate("text", label = "100 m", x=-940000, y=980000, color = "white",
           size = 3, angle = 310) +
  annotate("text", label = "200 m", x=-1080000, y=930000, color = "white",
           size = 3, angle = 310) +
  theme(axis.title=element_text(size=18),
        axis.text=element_text(size=15),
        strip.text=element_text(size=18),
        title = element_text(size = 18)) +
  labs(x = "Longitude", y = "Latitude", 
       title = paste(Spec,"salmon"))

ggsave(device = "jpeg", height = 12, width = 10,
       path = dir.fig, filename = paste0(spec,"_mod4_EFH_comparison.jpeg")); beep()

#### Plot Dynamic Core (50%) EFH (as points) ####
# GAM is red, VAST is blue
ggplot() +
  geom_sf(data = G_450_sf, color = alpha("#d7191c", 0.2), size = 0.3) +
  geom_sf(data = V_450_sf, color = alpha("#2b83ba", 0.3), size = 0.3) +
  facet_wrap(~Year) +
  geom_sf(data = gam1, fill = NA, col = "#d7191c", linewidth = 0.4, alpha = 0.5) +
  geom_sf(data = vast1, fill = NA, col = "#2b83ba", linewidth = 0.4, alpha = 0.5) +
  geom_sf(data = russia, fill = "#CCCCCC") +
  geom_sf(data = shelfb, color = "white", fill = NA) +
  geom_sf(data = cshelf, color = "white") +
  geom_sf(data = ak, fill = "#CCCCCC") +
  geom_sf(data = riv, color = "#666262") +
  coord_sf(crs = st_crs(3338), xlim = c(-1250000, -150000), 
           ylim = c(400000, 1900000), expand = FALSE, 
           datum = st_crs(4326))+
  scale_x_continuous(breaks = c(seq(-180 , -125, by = 7))) +
  annotate("text", label = "50 m", x=-760000, y=1100000, color = "white",
           size = 3, angle = 310) +
  annotate("text", label = "100 m", x=-940000, y=980000, color = "white",
           size = 3, angle = 310) +
  annotate("text", label = "200 m", x=-1080000, y=930000, color = "white",
           size = 3, angle = 310) +
  theme(axis.title=element_text(size=18),
        axis.text=element_text(size=15),
        strip.text=element_text(size=18),
        title = element_text(size = 18)) +
  labs(x = "Longitude", y = "Latitude", 
       title = paste(Spec,"salmon"))

ggsave(device = "jpeg", height = 12, width = 10,
       path = dir.fig, filename = paste0(spec,"_mod450_EFH_comparison.jpeg")); beep()

