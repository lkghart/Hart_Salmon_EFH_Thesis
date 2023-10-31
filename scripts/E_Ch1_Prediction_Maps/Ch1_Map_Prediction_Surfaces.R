#### Map VAST and GAM prediction surfaces ####
# author: Lilian Hart
# date last edited: 10/31/23

require(tidyverse)
require(dplyr)
require(here)
require(viridis)
require(mgcv)
require(visreg)
require(ggmap)
require(beepr)
require(ggplot2)
require(rnaturalearth)
require(rnaturalearthhires)
require(sf)
require(rnaturalearthdata)
require(ggspatial)
require(VAST)
require(FishStatsUtils)
require(akgfmaps)

spec <- "Chinook"
spec2 <- "chinook"

#### Setup ####
dir.data <- here("data", "BASIS")
dir.work <- here("data", "Chapter_1_RDS")
dir.shelf <- file.path("~/Documents/AK_Shapefiles")
dir.fig <- file.path("~/Documents/Salmon_EFH_Deliverables/Chapter1_Figures/V4_Figs/Comparisons")

### Load in mapping attributes ###
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

#### Load in models ####
# VAST Model 1
mod1_V <- readRDS(file.path(dir.work,paste0(spec2,"_VAST_mod1.rds")))
mod1_V <- reload_model(mod1_V)
est1_V <- as.data.frame(mod1_V$Report$D_gct) 
est1_V$sd <- sd(est1_V$Fit)
means <- mean(est1_V$Fit)
# Calculate min and max, then standardize prediction points
mina <- min(est1_V$Fit)
maxa <- max(est1_V$Fit)
est1_V$Fit_standard <- (est1_V$Fit - mina) /  (maxa - mina)

#VAST model 3
mod3_V <- readRDS(file.path(dir.work,paste0(spec2,"_VAST_mod3.rds")))
mod3_V <- reload_model(mod3_V)
est3_V <- as.data.frame(mod3_V$Report$D_gct) 

# VAST model 4
mod4_V <- readRDS(file.path(dir.work,paste0(spec2,"_VAST_mod4.rds")))
mod4_V <- reload_model(mod4_V)
est4_V <- as.data.frame(mod4_V$Report$D_gct) 

# Get extrapolation grid locations
sites <- as.data.frame(mod1_V$extrapolation_list$Data_Extrap) %>% 
  drop_na(Include) %>% dplyr::select(Lon, Lat)

# Load GAM predictions
mod1_G <- readRDS(file.path(dir.work,paste0(spec,"_GAM_Mod1_Predictions_Link.rds")))
minb <- min(mod1_G$Fit)
maxb <- max(mod1_G$Fit)
mod1_G$Fit_standard <- (mod1_G$Fit - minb) /  (maxb - minb)

# GAM model 4
mod4_G <- readRDS(file.path(dir.work,paste0(spec,"_GAM_Mod4_Predictions_Link.rds")))
mod4_G <- readRDS(file.path(dir.work,paste0(spec,"_GAM_Mod4_Predictions_Link.rds")))

### Convert dataframes to sf objects ###
est1_G_sf <- st_as_sf(mod1_G, coords = c("Lon", "Lat"), crs = 4326)
est1_V_sf <- st_as_sf(est1_V, coords = c("Lon", "Lat"), crs = 4326)
est3_V_sf <- st_as_sf(est3_V, coords = c("Lon", "Lat"), crs = 4326)
est3_G_sf <- st_as_sf(est3_G, coords = c("Lon", "Lat"), crs = 4326)
est4_V_sf <- st_as_sf(est4_V, coords = c("Lon", "Lat"), crs = 4326)
est4_G_sf <- st_as_sf(est4_G, coords = c("Lon", "Lat"), crs = 4326)

#### Plot Models ####
# Model 1 (Static model) #
# Plot GAM
ggplot() +
  geom_sf(data = est1_G_sf, mapping = aes(color = Fit)) +
  geom_sf(data = russia, fill = "#CCCCCC") +
  scale_color_viridis(name = "Log(count)") +
  geom_sf(data = shelfb, color = "white", fill = NA) +
  geom_sf(data = cshelf, color = "white") +
  geom_sf(data = ak, fill = "#CCCCCC") +
  geom_sf(data = riv, color = "#666262") +
  coord_sf(crs = st_crs(3338), xlim = c(-1250000, -100000), 
           ylim = c(400000, 2000000), expand = FALSE, 
           datum = st_crs(4326))+
  scale_x_continuous(breaks = c(seq(-180 , -125, by = 7))) +
  annotate("text", label = "50 m", x=-760000, y=1100000, color = "white",
           size = 2.6, angle = 310) +
  annotate("text", label = "100 m", x=-940000, y=980000, color = "white",
           size = 2.6, angle = 310) +
  annotate("text", label = "200 m", x=-1080000, y=930000, color = "white",
           size = 2.6, angle = 310) +
  annotate("text", label = "Continental Shelf", x=-1550000, y=1250000, 
           color = "white", size = 3, angle = 25) +
  annotate("text", label = "Yukon", x=-240000, y=1500000, color = "black",
           size = 2, angle = 57) +
  annotate("text", label = "Kuskokwim", x=-270000, y=1240000, color = "black",
           size = 2, angle = 22) +
  annotate("text", label = "Bristol\n Bay", x=-340000, y=870000, color = "black",
           size = 2) +
  annotate("text", label = "Unimak Pass", x=-615000, y=480000, color = "black",
           size = 2, angle = 330) +
  annotate("text", label = "False Pass", x=-500000, y=495000,
           color = "black", size = 2, angle = 330) +
  annotate("text", label = "Norton Sound", x=-500000, y=1600000,
           color = "black", size = 2) +
  annotate("text", label = "St. Lawrence Isl.", x=-810000, y=1665000,
           color = "black", size = 2) +
  annotate("text", label = "St. Matthew Isl.", x=-980000, y=1350000,
           color = "black", size = 2) +
  annotate("text", label = "Nunivak Isl.", x=-750000, y=1240000,
           color = "black", size = 2) +
  labs(x = "Longitude", y = "Latitude", 
       title = paste("GAM juvenile",spec,"salmon model 1")) +
  theme(axis.title=element_text(size=14), 
        strip.text=element_text(size=14))
#Save to output folder
ggsave(device = "jpeg", height = 5.5, width = 11,
       path = dir.fig, filename = paste0(spec,"_GAM_mod1.jpeg"))

# VAST model 1
ggplot() +
  geom_sf(data = est1_V_sf, mapping = aes(color = Fit)) +
  geom_sf(data = russia, fill = "#CCCCCC") +
  scale_color_viridis(name = "Log(count)") +
  geom_sf(data = shelfb, color = "white", fill = NA) +
  geom_sf(data = cshelf, color = "white") +
  geom_sf(data = ak, fill = "#CCCCCC") +
  geom_sf(data = riv, color = "#666262") +
  coord_sf(crs = st_crs(3338), xlim = c(-1250000, -100000), 
           ylim = c(400000, 2000000), expand = FALSE, 
           datum = st_crs(4326))+
  scale_x_continuous(breaks = c(seq(-180 , -125, by = 7))) +
  annotate("text", label = "50 m", x=-760000, y=1100000, color = "white",
           size = 2.6, angle = 310) +
  annotate("text", label = "100 m", x=-940000, y=980000, color = "white",
           size = 2.6, angle = 310) +
  annotate("text", label = "200 m", x=-1080000, y=930000, color = "white",
           size = 2.6, angle = 310) +
  annotate("text", label = "Continental Shelf", x=-1550000, y=1250000, 
           color = "white", size = 3, angle = 25) +
  annotate("text", label = "Yukon", x=-240000, y=1500000, color = "black",
           size = 2, angle = 57) +
  annotate("text", label = "Kuskokwim", x=-270000, y=1240000, color = "black",
           size = 2, angle = 22) +
  annotate("text", label = "Bristol\n Bay", x=-340000, y=870000, color = "black",
           size = 2) +
  annotate("text", label = "Unimak Pass", x=-615000, y=480000, color = "black",
           size = 2, angle = 330) +
  annotate("text", label = "False Pass", x=-500000, y=495000,
           color = "black", size = 2, angle = 330) +
  annotate("text", label = "Norton Sound", x=-500000, y=1600000,
           color = "black", size = 2) +
  annotate("text", label = "St. Lawrence Isl.", x=-810000, y=1665000,
           color = "black", size = 2) +
  annotate("text", label = "St. Matthew Isl.", x=-980000, y=1350000,
           color = "black", size = 2) +
  annotate("text", label = "Nunivak Isl.", x=-750000, y=1240000,
           color = "black", size = 2) +
  labs(x = "Longitude", y = "Latitude", 
       title = paste("VAST juvenile",spec,"salmon model 1")) +
  theme(axis.title=element_text(size=14), 
        strip.text=element_text(size=14))
#Save to output folder
ggsave(device = "jpeg", height = 5.5, width = 11,
       path = dir.fig, filename = paste0(spec,"_VAST_mod1.jpeg"))

### Model 3 ###
# Plot GAM
ggplot() +
  geom_sf(data = est3_G_sf, mapping = aes(color = Fit)) +
  geom_sf(data = russia, fill = "#CCCCCC") +
  scale_color_viridis(name = "Log(count)") +
  geom_sf(data = shelfb, color = "white", fill = NA) +
  geom_sf(data = cshelf, color = "white") +
  geom_sf(data = ak, fill = "#CCCCCC") +
  geom_sf(data = riv, color = "#666262") +
  coord_sf(crs = st_crs(3338), xlim = c(-1250000, -100000), 
           ylim = c(400000, 2000000), expand = FALSE, 
           datum = st_crs(4326))+
  scale_x_continuous(breaks = c(seq(-180 , -125, by = 7))) +
  facet_wrap(~Year) +
  annotate("text", label = "50 m", x=-760000, y=1100000, color = "white",
           size = 2.6, angle = 310) +
  annotate("text", label = "100 m", x=-940000, y=980000, color = "white",
           size = 2.6, angle = 310) +
  annotate("text", label = "200 m", x=-1080000, y=930000, color = "white",
           size = 2.6, angle = 310) +
  annotate("text", label = "Continental Shelf", x=-1550000, y=1250000, 
           color = "white", size = 3, angle = 25) +
  annotate("text", label = "Yukon", x=-240000, y=1500000, color = "black",
           size = 2, angle = 57) +
  annotate("text", label = "Kuskokwim", x=-270000, y=1240000, color = "black",
           size = 2, angle = 22) +
  annotate("text", label = "Bristol\n Bay", x=-340000, y=870000, color = "black",
           size = 2) +
  annotate("text", label = "Unimak Pass", x=-615000, y=480000, color = "black",
           size = 2, angle = 330) +
  annotate("text", label = "False Pass", x=-500000, y=495000,
           color = "black", size = 2, angle = 330) +
  annotate("text", label = "Norton Sound", x=-500000, y=1600000,
           color = "black", size = 2) +
  annotate("text", label = "St. Lawrence Isl.", x=-810000, y=1665000,
           color = "black", size = 2) +
  annotate("text", label = "St. Matthew Isl.", x=-980000, y=1350000,
           color = "black", size = 2) +
  annotate("text", label = "Nunivak Isl.", x=-750000, y=1240000,
           color = "black", size = 2) +
  labs(x = "Longitude", y = "Latitude", 
       title = paste("GAM juvenile",spec,"salmon model 3")) +
  theme(axis.title=element_text(size=14), 
        strip.text=element_text(size=14))
#Save to output folder
ggsave(device = "jpeg", height = 5.5, width = 11,
       path = dir.fig, filename = paste0(spec,"_GAM_mod3.jpeg"))

# Plot VAST
ggplot() +
  geom_sf(data = est3_V_sf, mapping = aes(color = log(Fit))) +
  geom_sf(data = russia, fill = "#CCCCCC") +
  scale_color_viridis(name = "Log(count)") +
  geom_sf(data = shelfb, color = "white", fill = NA) +
  geom_sf(data = cshelf, color = "white") +
  geom_sf(data = ak, fill = "#CCCCCC") +
  geom_sf(data = riv, color = "#666262") +
  coord_sf(crs = st_crs(3338), xlim = c(-1250000, -100000), 
           ylim = c(400000, 2000000), expand = FALSE, 
           datum = st_crs(4326))+
  scale_x_continuous(breaks = c(seq(-180 , -125, by = 7)))+
  facet_wrap(~Year) +
  annotate("text", label = "50 m", x=-760000, y=1100000, color = "white",
           size = 2.6, angle = 310) +
  annotate("text", label = "100 m", x=-940000, y=980000, color = "white",
           size = 2.6, angle = 310) +
  annotate("text", label = "200 m", x=-1080000, y=930000, color = "white",
           size = 2.6, angle = 310) +
  annotate("text", label = "Continental Shelf", x=-1550000, y=1250000, 
           color = "white", size = 3, angle = 25) +
  annotate("text", label = "Yukon", x=-240000, y=1500000, color = "black",
           size = 2, angle = 57) +
  annotate("text", label = "Kuskokwim", x=-270000, y=1240000, color = "black",
           size = 2, angle = 22) +
  annotate("text", label = "Bristol\n Bay", x=-340000, y=870000, color = "black",
           size = 2) +
  annotate("text", label = "Norton Sound", x=-500000, y=1600000,
           color = "black", size = 2) +
  annotate("text", label = "St. Lawrence Isl.", x=-810000, y=1665000,
           color = "black", size = 2) +
  annotate("text", label = "St. Matthew Isl.", x=-980000, y=1350000,
           color = "black", size = 2) +
  annotate("text", label = "Nunivak Isl.", x=-750000, y=1240000,
           color = "black", size = 2) +
  annotate("text", label = "Unimak Pass", x=-615000, y=480000, color = "black",
           size = 2, angle = 330) +
  annotate("text", label = "False Pass", x=-500000, y=495000,
           color = "black", size = 2, angle = 330) +
  labs(x = "Longitude", y = "Latitude", 
       title = paste("VAST juvenile",spec,"salmon model 3")) +
  theme(axis.title=element_text(size=14), 
        strip.text=element_text(size=14))

#Save to output folder
ggsave(device = "jpeg", height = 5.5, width = 11,
       path = dir.fig, filename = paste0(spec,"_VAST_mod3.jpeg"))

### Model 4 ###
# Plot VAST
ggplot() +
  geom_sf(data = est4_V_sf, mapping = aes(color = log(Fit))) +
  geom_sf(data = russia, fill = "#CCCCCC") +
  scale_color_viridis(name = "Log(count)") +
  geom_sf(data = shelfb, color = "white", fill = NA) +
  geom_sf(data = cshelf, color = "white") +
  geom_sf(data = ak, fill = "#CCCCCC") +
  geom_sf(data = riv, color = "#666262") +
  coord_sf(crs = st_crs(3338), xlim = c(-1250000, -100000), 
           ylim = c(400000, 2000000), expand = FALSE, 
           datum = st_crs(4326))+
  scale_x_continuous(breaks = c(seq(-180 , -125, by = 7)))+
  facet_wrap(~Year) +
  annotate("text", label = "50 m", x=-760000, y=1100000, color = "white",
           size = 2.6, angle = 310) +
  annotate("text", label = "100 m", x=-940000, y=980000, color = "white",
           size = 2.6, angle = 310) +
  annotate("text", label = "200 m", x=-1080000, y=930000, color = "white",
           size = 2.6, angle = 310) +
  annotate("text", label = "Continental Shelf", x=-1550000, y=1250000, 
           color = "white", size = 3, angle = 25) +
  # annotate("text", label = "Russia", x=-1700000, y=2300000, color = "white",
  #          size = 4) +
  annotate("text", label = "Yukon", x=-240000, y=1500000, color = "black",
           size = 2, angle = 57) +
  annotate("text", label = "Kuskokwim", x=-270000, y=1240000, color = "black",
           size = 2, angle = 22) +
  annotate("text", label = "Bristol\n Bay", x=-340000, y=870000, color = "black",
           size = 2) +
  annotate("text", label = "Unimak Pass", x=-615000, y=480000, color = "black",
           size = 2, angle = 330) +
  annotate("text", label = "False Pass", x=-500000, y=495000,
           color = "black", size = 2, angle = 330) +
  annotate("text", label = "Norton Sound", x=-500000, y=1600000,
           color = "black", size = 2) +
  annotate("text", label = "St. Lawrence Isl.", x=-810000, y=1665000,
           color = "black", size = 2) +
  annotate("text", label = "St. Matthew Isl.", x=-980000, y=1350000,
           color = "black", size = 2) +
  annotate("text", label = "Nunivak Isl.", x=-750000, y=1240000,
           color = "black", size = 2) +
  labs(x = "Longitude", y = "Latitude", 
       title = paste("VAST juvenile",spec,"salmon model 4")) +
  theme(axis.title=element_text(size=14), 
        strip.text=element_text(size=14))

#Save to output folder
ggsave(device = "jpeg", height = 5.5, width = 11,
       path = dir.fig, filename = paste0(spec,"_VAST_mod4.jpeg"))

# Plot GAM
ggplot() +
  geom_sf(data = est4_G_sf, mapping = aes(color = Fit)) +
  geom_sf(data = russia, fill = "#CCCCCC") +
  scale_color_viridis(name = "Log(count)") +
  geom_sf(data = shelfb, color = "white", fill = NA) +
  geom_sf(data = cshelf, color = "white") +
  geom_sf(data = ak, fill = "#CCCCCC") +
  geom_sf(data = riv, color = "#666262") +
  coord_sf(crs = st_crs(3338), xlim = c(-1250000, -100000), 
           ylim = c(400000, 2000000), expand = FALSE, 
           datum = st_crs(4326))+
  scale_x_continuous(breaks = c(seq(-180 , -125, by = 7))) +
  facet_wrap(~Year) +
  annotate("text", label = "50 m", x=-760000, y=1100000, color = "white",
           size = 2.6, angle = 310) +
  annotate("text", label = "100 m", x=-940000, y=980000, color = "white",
           size = 2.6, angle = 310) +
  annotate("text", label = "200 m", x=-1080000, y=930000, color = "white",
           size = 2.6, angle = 310) +
  annotate("text", label = "Continental Shelf", x=-1550000, y=1250000, 
           color = "white", size = 3, angle = 25) +
  annotate("text", label = "Yukon", x=-240000, y=1500000, color = "black",
           size = 2, angle = 57) +
  annotate("text", label = "Kuskokwim", x=-270000, y=1240000, color = "black",
           size = 2, angle = 22) +
  annotate("text", label = "Bristol\n Bay", x=-340000, y=870000, color = "black",
           size = 2) +
  annotate("text", label = "Unimak Pass", x=-615000, y=480000, color = "black",
           size = 2, angle = 330) +
  annotate("text", label = "False Pass", x=-500000, y=495000,
           color = "black", size = 2, angle = 330) +
  annotate("text", label = "Norton Sound", x=-500000, y=1600000,
           color = "black", size = 2) +
  annotate("text", label = "St. Lawrence Isl.", x=-810000, y=1665000,
           color = "black", size = 2) +
  annotate("text", label = "St. Matthew Isl.", x=-980000, y=1350000,
           color = "black", size = 2) +
  annotate("text", label = "Nunivak Isl.", x=-750000, y=1240000,
           color = "black", size = 2) +
  labs(x = "Longitude", y = "Latitude", 
       title = paste("GAM juvenile",spec,"salmon model 4")) +
  theme(axis.title=element_text(size=14), 
        strip.text=element_text(size=14))

#Save to output folder
ggsave(device = "jpeg", height = 5.5, width = 11,
       path = dir.fig, filename = paste0(spec,"_GAM_mod4.jpeg"))