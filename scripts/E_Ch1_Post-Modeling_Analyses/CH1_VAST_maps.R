#### Maps of predicted densities for best spatiotemporal VAST models  ####
## Author: Lilian Hart 
## Last edited: 02/21/23

require(here)
require(viridis)
require(mgcv)
require(visreg)
require(gratia)
require(beepr)
library(remote)
library(rnaturalearth)
library(rnaturalearthhires)
library(gganimate)
library(sf)
library(rnaturalearthdata)
require(ggspatial)
require(VAST)
require(tidyverse)
require(dplyr)

### Workflow ###
Spec <- "Chum"
spec <- "chum"

dir.work <- here("data", "Chapter_1_RDSModels")
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
shelf <- st_read(file.path(dir.shelf, "arctic_coast_bathy/arctic_coast_bathy.shp"))
shelfb <- shelf %>% dplyr::select(elevation, geometry) %>% 
  filter(elevation == -105 | elevation == -55)
shelfb <- st_transform(shelfb, 4326)
cshelf <- st_read(file.path(dir.shelf, "AK_CSB.gdb"))
# Major AK rivers
rivers <- st_read(file.path(dir.shelf, "USA_Major_Rivers/v10/rivers.gdb"))
riv <- rivers %>% filter(NAME == "Kuskokwim" | NAME == "Yukon" |
                           NAME == "Koyukuk" | NAME == "Stikine")


#### Load in model predictions and reformat ####
mod1_V <- readRDS(file.path(dir.work,paste0(Spec,"_VAST_mod1.rds")))
mod1_V <- reload_model(mod1_V)
sites <- as.data.frame(mod1_V$extrapolation_list$Data_Extrap) %>% 
  drop_na(Include) %>% dplyr::select(Lon, Lat)

mod1 <- as.data.frame(mod1_V$Report$D_gct) %>% drop_units %>% 
  rename("2002" = "1.2002", "2003" = "1.2003", "2004" = "1.2004", 
         "2005" = "1.2005","2006" = "1.2006", "2007" = "1.2007", "2008" = "1.2008",
         "2009" = "1.2009", "2010" = "1.2010", "2011" = "1.2011", "2012" = "1.2012",
         "2013" = "1.2013", "2014" = "1.2014", "2015" = "1.2015", "2016" = "1.2016",
         "2017" = "1.2017", "2018" = "1.2018", "2019" = "1.2019")
sub <- mod1[,1]
mod1 <- data.frame(Fit = sub, Lat = sites$Lat, Lon = sites$Lon)

# model 3/4
mod3_V <- readRDS(file.path(dir.work,paste0(Spec,"_VAST_mod4.rds")))
mod3_V <- reload_model(mod3_V)
temp <- as.data.frame(mod3_V$Report$D_gct) %>% drop_units() %>%
  rename("2002" = "1.2002", "2003" = "1.2003", "2004" = "1.2004", 
  "2005" = "1.2005","2006" = "1.2006", "2007" = "1.2007", "2008" = "1.2008",
  "2009" = "1.2009", "2010" = "1.2010", "2011" = "1.2011", "2012" = "1.2012",
  "2013" = "1.2013", "2014" = "1.2014", "2015" = "1.2015", "2016" = "1.2016",
  "2017" = "1.2017", "2018" = "1.2018", "2019" = "1.2019")
index <- c(1:18)
years <- as.factor(2002:2019)
est3_V <- data.frame()
for (i in index){
  year <- years[i]
  a <- data.frame(Year = year, Fit = temp[,i], Lat = sites$Lat, 
                  Lon = sites$Lon)
  est3_V <- rbind(est3_V, a)
}

# Convert to shapefile
mod1_sf <- st_as_sf(mod1, coords = c("Lon", "Lat"), crs = 4326)
est3_V_sf <- st_as_sf(est3_V, coords = c("Lon", "Lat"), crs = 4326)

#### Map predictions ####

# model 1
ggplot() +
  geom_sf(data = mod1_sf, mapping = aes(color = log(Fit))) +
  geom_sf(data = russia, fill = "#CCCCCC") +
  scale_color_viridis(name = "Log(count)") +
  geom_sf(data = ak, fill = "#CCCCCC") +
  geom_sf(data = riv, color = "#666262") +
  coord_sf(crs = st_crs(3338), xlim = c(-1250000, -100000), 
           ylim = c(400000, 2000000), expand = FALSE, 
           datum = st_crs(4326))+
  scale_x_continuous(breaks = c(seq(-180 , -125, by = 7)))+
  labs(x = "Longitude", y = "Latitude", 
       title = paste("VAST juvenile",spec,"salmon model 1")) +
  theme(axis.title=element_text(size=14), 
        strip.text=element_text(size=14))

#Save to output folder
ggsave(device = "jpeg", height = 5, width = 8,
       path = dir.fig, filename = paste0(spec,"_VAST_mod1_Link.jpeg"))

# model 3
ggplot() +
  geom_sf(data = est3_V_sf, mapping = aes(color = log(Fit))) +
  geom_sf(data = russia, fill = "#CCCCCC") +
  scale_color_viridis(name = "Log(count)") +
  geom_sf(data = ak, fill = "#CCCCCC") +
  geom_sf(data = riv, color = "#666262") +
  coord_sf(crs = st_crs(3338), xlim = c(-1250000, -100000), 
           ylim = c(400000, 2000000), expand = FALSE, 
           datum = st_crs(4326))+
  scale_x_continuous(breaks = c(seq(-180 , -125, by = 7)))+
  facet_wrap(~Year, ncol = 5) +
  labs(x = "Longitude", y = "Latitude", 
       title = paste("VAST juvenile",spec,"salmon model 3")) +
  theme(axis.title=element_text(size=14), 
        strip.text=element_text(size=14))
#Save to output folder
ggsave(device = "jpeg", height = 11, width = 10,
       path = dir.fig, filename = paste0(spec,"_VAST_mod3_Link.jpeg"))
