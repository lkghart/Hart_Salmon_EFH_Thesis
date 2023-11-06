### Chapter 2 GAM Top 50% of Predictions Maps (response) 
## Author: Lilian Hart 
## Last edited: 11/06/23

require(tidyverse)
require(dplyr)
require(here)
require(viridis)
require(mgcv)
require(visreg)
require(beepr)
library(rnaturalearth)
library(rnaturalearthhires)
library(sf)
library(rnaturalearthdata)
require(ggspatial)
require(akgfmaps)

#### Setup ####
Spec <- "Chum"
spec <- tolower(Spec)

dir.work <- here("data", "Chapter_2_EFH")
dir.mod <- here("data", "Chapter_2_RDS")
setwd(dir.work)
dir.fig <- file.path("~/Documents/Salmon_EFH_Deliverables/Chapter2_Figures")
dir.shelf <- file.path("~/Documents/AK_Shapefiles")

#### Load data ####
## Load predictions
# In-situ Model (1A)
ispoly <- readRDS(file.path(dir.work, file = paste0(spec,"_G_150_poly.rds")))
in_situ <- readRDS(file.path(dir.work, paste0(spec,"_gam_in-situ_EFH_50.rds")))
is_CV <- readRDS(file.path(dir.mod, paste0(spec,"_gam_in-situ_link_predictions.rds")))

# Cold Pool Extent SVC models
CPE_low <- readRDS(file.path(dir.work, paste0(spec, "_gam_CPE-low_EFH_50.rds"))) 
CPE_med <- readRDS(file.path(dir.work, paste0(spec, "_gam_CPE-med_EFH_50.rds")))
CPE_high <- readRDS(file.path(dir.work, paste0(spec, "_gam_CPE-high_EFH_50.rds")))
CPE_CV <- readRDS(file.path(dir.mod, paste0(spec, "_gam_CPE-median_link_predictions.rds")))

# Climate stanza predictions
Warm1 <- readRDS(file.path(dir.work, paste0(spec, "_gam_Warm1_EFH_50.rds")))
Cool <- readRDS(file.path(dir.work, paste0(spec, "_gam_Cool_EFH_50.rds")))
Warm2 <- readRDS(file.path(dir.work, paste0(spec, "_gam_Warm2_EFH_50.rds")))
CS_CV <- readRDS(file.path(dir.mod, paste0(spec, "_gam_Warm1_link_predictions.rds")))

# Even/odd year predictions
Even <- readRDS(file.path(dir.work, paste0(spec, "_gam_Even-year_EFH_50.rds")))
Odd <- readRDS(file.path(dir.work, paste0(spec, "_gam_Odd-year_EFH_50.rds")))
EO_CV <- readRDS(file.path(dir.mod, paste0(spec, "_gam_Even-year_link_predictions.rds")))

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
shelf <- get_survey_bathymetry(select.region = "ebs", set.crs = 4326)
ebs_layers <- akgfmaps::get_base_layers(select.region = "ebs", set.crs = 4326)
shelfb <- ebs_layers$bathymetry
cshelf <- st_read(file.path(dir.shelf, "AK_CSB.gdb"))

## Convert predictions to sf objects

# In-situ model (1A)
in_situ_df <- data.frame(Fit = in_situ$fit, Lon = in_situ$Lon, Lat = in_situ$Lat, 
                         CV = in_situ$CV)
in_situ_sf <- st_as_sf(in_situ_df, coords = c("Lon", "Lat"), crs = 4326)
is_CV_df <- data.frame(Fit = is_CV$fit, Lon = is_CV$Lon, Lat = is_CV$Lat, 
                         CV = is_CV$CV)
is_CV_sf <- st_as_sf(is_CV_df, coords = c("Lon", "Lat"), crs = 4326)
ispoly_sf <-st_as_sf(ispoly, coords = c("Lon", "Lat"), crs = 4326)

# Cold Pool Extent SVC models
CPE_low_df <- data.frame(Fit = CPE_low$fit, Lon = CPE_low$Lon, Lat = CPE_low$Lat, 
                       CV = CPE_low$CV)
CPE_low_sf <- st_as_sf(CPE_low_df, coords = c("Lon", "Lat"), crs = 4326)

CPE_med_df <- data.frame(Fit = CPE_med$fit, Lon = CPE_med$Lon, Lat = CPE_med$Lat, 
                         CV = CPE_med$CV)
CPE_med_sf <- st_as_sf(CPE_med_df, coords = c("Lon", "Lat"), crs = 4326)

CPE_high_df <- data.frame(Fit = CPE_high$fit, Lon = CPE_high$Lon, Lat = CPE_high$Lat, 
                         CV = CPE_high$CV)
CPE_high_sf <- st_as_sf(CPE_high_df, coords = c("Lon", "Lat"), crs = 4326)
CPE_CV_df <- data.frame(Fit = CPE_CV$fit, Lon = CPE_CV$Lon, Lat = CPE_CV$Lat, 
                        CV = CPE_CV$CV)
CPE_CV_sf <- st_as_sf(CPE_CV_df, coords = c("Lon", "Lat"), crs = 4326)

# Climate stanza SVC models
Warm1_df <- data.frame(Fit = Warm1$fit, Lon = Warm1$Lon, Lat = Warm1$Lat, 
                       CV = Warm1$CV)
Warm1_sf <- st_as_sf(Warm1_df, coords = c("Lon", "Lat"), crs = 4326)
Cool_df <- data.frame(Fit = Cool$fit, Lon = Cool$Lon, Lat = Cool$Lat, 
                       CV = Cool$CV)
Cool_sf <- st_as_sf(Cool_df, coords = c("Lon", "Lat"), crs = 4326)
Warm2_df <- data.frame(Fit = Warm2$fit, Lon = Warm2$Lon, Lat = Warm2$Lat, 
                       CV = Warm2$CV)
Warm2_sf <- st_as_sf(Warm2_df, coords = c("Lon", "Lat"), crs = 4326)
CS_CV_df <- data.frame(Fit = CS_CV$fit, Lon = CS_CV$Lon, Lat = CS_CV$Lat, 
                       CV = CS_CV$CV)
CS_CV_sf <- st_as_sf(CS_CV_df, coords = c("Lon", "Lat"), crs = 4326)

# Even/Odd year models
Even_df <- data.frame(Fit = Even$fit, Lon = Even$Lon, Lat = Even$Lat, 
                       CV = Even$CV)
Even_sf <- st_as_sf(Even_df, coords = c("Lon", "Lat"), crs = 4326)
Odd_df <- data.frame(Fit = Odd$fit, Lon = Odd$Lon, Lat = Odd$Lat, 
                      CV = Odd$CV)
Odd_sf <- st_as_sf(Odd_df, coords = c("Lon", "Lat"), crs = 4326)
EO_df <- data.frame(Fit = EO_CV$fit, Lon = EO_CV$Lon, Lat = EO_CV$Lat, 
                      CV = EO_CV$CV)
EO_sf <- st_as_sf(EO_df, coords = c("Lon", "Lat"), crs = 4326)

#### In-situ Maps ####
ggplot() +
  geom_sf(data = in_situ_sf, color = "#15A3A3") +
  geom_sf(data = russia, fill = "#CCCCCC") +
  geom_sf(data = ak, fill = "#CCCCCC") +
  geom_sf(data = shelfb, color = "white", fill = NA) +
  geom_sf(data = cshelf, color = "white") +
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
       title = paste("Juvenile",spec,"salmon \n In-Situ Covar. Model \nTop 50% of Predictions"))

ggsave(paste0(Spec,"_in-situ_EFH_50.jpeg"), width = 5, height = 5, units = "in")

#In-situ uncertainty
ggplot() +
  geom_sf(data = is_CV_sf, mapping = aes(color = CV)) +
  geom_sf(data = russia, fill = "#CCCCCC") +
  scale_color_viridis(name = "CV") +
  geom_sf(data = ak, fill = "#CCCCCC") +
  geom_sf(data = shelfb, color = "white", fill = NA) +
  geom_sf(data = cshelf, color = "white") +
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
       title = paste("Juvenile", spec, "salmon\n In-Situ Covar. Model\n Uncertainty"))

ggsave(paste0(Spec,"_in-situ_CV.jpeg"), width = 5, height = 5, units = "in")

#### Cold Pool Extent Maps ####
# CRS 3336 is NAD83 Alaska Albers Projection
#limits <- c(-166, 124)
#cvlims <- c()

#Low CPE
ggplot() +
  geom_sf(data = CPE_low_sf, color = "#15A3A3") +
  geom_sf(data = ispoly_sf, fill = NA, col = "#d8b365",
          linewidth = 0.8, alpha = 0.5) +
  geom_sf(data = russia, fill = "#CCCCCC") +
  geom_sf(data = ak, fill = "#CCCCCC") +
  geom_sf(data = shelfb, color = "white", fill = NA) +
  geom_sf(data = cshelf, color = "white") +
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
       title = paste("Juvenile",spec,"salmon\n Low CPE"))

ggsave(paste0(Spec,"_CPE-low_EFH_50.jpeg"), width = 5, height = 5, units = "in")

#Median CPE
ggplot() +
  geom_sf(data = CPE_med_sf, color = "#15A3A3") +
  geom_sf(data = ispoly_sf, fill = NA, col = "#d8b365",
          linewidth = 0.8, alpha = 0.5) +
  geom_sf(data = russia, fill = "#CCCCCC") +
  geom_sf(data = ak, fill = "#CCCCCC") +
  geom_sf(data = shelfb, color = "white", fill = NA) +
  geom_sf(data = cshelf, color = "white") +
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
       title = paste("Juvenile",spec,"salmon\n Median CPE"))

ggsave(paste0(Spec,"_CPE-med_EFH_50.jpeg"), width = 5, height = 5, units = "in")

# High CPE
ggplot() +
  geom_sf(data = CPE_high_sf, color = "#15A3A3") +
  geom_sf(data = ispoly_sf, fill = NA, col = "#d8b365",
          linewidth = 0.8, alpha = 0.5) +
  geom_sf(data = russia, fill = "#CCCCCC") +
  geom_sf(data = ak, fill = "#CCCCCC") +
  geom_sf(data = shelfb, color = "white", fill = NA) +
  geom_sf(data = cshelf, color = "white") +
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
       title = paste("Juvenile",spec,"salmon\n High CPE"))

ggsave(paste0(Spec,"_CPE-high_EFH_50.jpeg"), width = 5, height = 5, units = "in")

# Cold Pool Extent Uncertainty
ggplot() +
  geom_sf(data = CPE_CV_sf, mapping = aes(color = CV)) +
  geom_sf(data = russia, fill = "#CCCCCC") +
  scale_color_viridis(name = "CV") +
  geom_sf(data = ak, fill = "#CCCCCC") +
  geom_sf(data = shelfb, color = "white", fill = NA) +
  geom_sf(data = cshelf, color = "white") +
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
       title = paste("Juvenile", spec, "salmon \nCold Pool Extent \n Model Uncertainty"))
ggsave(paste0(Spec,"_CPE_CV.jpeg"), width = 5.5, height = 5, units = "in")


#### Climate Stanza Maps ####
# CRS 3336 is NAD83 Alaska Albers Projection

# Warm1 (2002-2005)
ggplot() +
  geom_sf(data = Warm1_sf, color = "#15A3A3") +
  geom_sf(data = ispoly_sf, fill = NA, col = "#d8b365",
          linewidth = 0.8, alpha = 0.5) +
  geom_sf(data = russia, fill = "#CCCCCC") +
  geom_sf(data = ak, fill = "#CCCCCC") +
  geom_sf(data = shelfb, color = "white", fill = NA) +
  geom_sf(data = cshelf, color = "white") +
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
       title = paste("Juvenile",spec,
                     "salmon \nWarm C.S. #1 (2002-2005)"))

ggsave(paste0(Spec,"_Warm1_EFH_50.jpeg"), width = 5.5, height = 5, units = "in")

# Cool (2006-2013)
ggplot() +
  geom_sf(data = Cool_sf, color = "#15A3A3") +
  geom_sf(data = ispoly_sf, fill = NA, col = "#d8b365",
          linewidth = 0.8, alpha = 0.5) +
  geom_sf(data = russia, fill = "#CCCCCC") +
  geom_sf(data = ak, fill = "#CCCCCC") +
  geom_sf(data = shelfb, color = "white", fill = NA) +
  geom_sf(data = cshelf, color = "white") +
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
       title = paste("Juvenile",spec,
                     "salmon \nCool C.S. (2006-2013)"))

ggsave(paste0(Spec,"_Cool_EFH_50.jpeg"), width = 5, height = 5, units = "in")

# Warm2 (2014-2019)
ggplot() +
  geom_sf(data = Warm2_sf, color = "#15A3A3") +
  geom_sf(data = ispoly_sf, fill = NA, col = "#d8b365",
          linewidth = 0.8, alpha = 0.5) +
  geom_sf(data = russia, fill = "#CCCCCC") +
  geom_sf(data = ak, fill = "#CCCCCC") +
  geom_sf(data = shelfb, color = "white", fill = NA) +
  geom_sf(data = cshelf, color = "white") +
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
       title = paste("Juvenile",spec,
                     "salmon \nWarm C.S. #2 (2014-2019)"))

ggsave(paste0(Spec,"_Warm2_EFH_50.jpeg"), width = 5.5, height = 5, units = "in")

# Climate Stanza Uncertainty
ggplot() +
  geom_sf(data = CS_CV_sf, mapping = aes(color = CV)) +
  geom_sf(data = russia, fill = "#CCCCCC") +
  scale_color_viridis(name = "CV") +
  geom_sf(data = ak, fill = "#CCCCCC") +
  geom_sf(data = shelfb, color = "white", fill = NA) +
  geom_sf(data = cshelf, color = "white") +
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
       title = paste("Juvenile", spec, "salmon\n Climate Stanza \n Model Uncertainty"))
ggsave(paste0(Spec,"_CS_CV.jpeg"), width = 5.5, height = 5, units = "in")

#### Even/Odd Year Maps ####
# Even years
ggplot() +
  geom_sf(data = Even_sf, color = "#15A3A3") +
  geom_sf(data = ispoly_sf, fill = NA, col = "#d8b365",
          linewidth = 0.8, alpha = 0.5) +
  geom_sf(data = russia, fill = "#CCCCCC") +
  geom_sf(data = ak, fill = "#CCCCCC") +
  geom_sf(data = shelfb, color = "white", fill = NA) +
  geom_sf(data = cshelf, color = "white") +
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
       title = paste("Juvenile",spec,
                     "salmon \n Even Years"))

ggsave(paste0(Spec,"_Even_EFH_50.jpeg"), width = 5, height = 5, units = "in")

# Odd Years
ggplot() +
  geom_sf(data = Odd_sf, color = "#15A3A3") +
  geom_sf(data = ispoly_sf, fill = NA, col = "#d8b365",
          linewidth = 0.8, alpha = 0.5) +
  geom_sf(data = russia, fill = "#CCCCCC") +
  geom_sf(data = ak, fill = "#CCCCCC") +
  geom_sf(data = shelfb, color = "white", fill = NA) +
  geom_sf(data = cshelf, color = "white") +
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
       title = paste("Juvenile",spec,
                     "salmon \n Odd Years"))

ggsave(paste0(Spec,"_Odd_EFH_50.jpeg"), width = 5, height = 5, units = "in")

# Climate Stanza Uncertainty
ggplot() +
  geom_sf(data = EO_sf, mapping = aes(color = CV)) +
  geom_sf(data = russia, fill = "#CCCCCC") +
  scale_color_viridis(name = "CV") +
  geom_sf(data = ak, fill = "#CCCCCC") +
  geom_sf(data = shelfb, color = "white", fill = NA) +
  geom_sf(data = cshelf, color = "white") +
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
       title = paste("Juvenile", spec, "salmon\n Even/Odd Year \n Model Uncertainty"))

ggsave(paste0(Spec,"_EOYear_CV.jpeg"), width = 5.5, height = 5, units = "in")