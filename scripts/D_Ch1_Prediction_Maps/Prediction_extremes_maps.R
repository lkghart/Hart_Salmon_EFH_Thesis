### Juvenile salmon prediction extremes maps ###
# author: Lilian Hart
# date last edited: 01/10/23

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

spec <- "Sockeye"
spec2 <- "sockeye"

dir.data <- here("data", "BASIS")
dir.work <- here("data", "Chapter_1_RDSModels")
dir.fig <- file.path("~/Documents/Salmon_EFH_Deliverables/Chapter1_Figures/V3_Figs")

years <- as.character(2002:2019)

# GAM model 3
modG <- readRDS(file.path(dir.work,paste0(spec,"_GAM_Mod3_Predictions_Link.rds")))

### Load in mapping attributes ###
usa <- ne_states("United States of America", returnclass = "sf")
ak <- subset(usa, name == "Alaska")
ak <- st_set_crs(ak, 4326)
russia <-rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")%>%
  filter(name=="Russia")
russia <- st_set_crs(russia, 4326)

predG <- st_as_sf(modG, coords = c("Lon", "Lat"), crs = 4326)

# Divide years into sets of four
set1 <- predG %>% filter(Year == 2002 | Year == 2003 | Year == 2004 |
                           Year == 2005)
set2 <- predG %>% filter(Year == 2006 | Year == 2007 | Year == 2008 |
                           Year == 2009)
set3 <- predG %>% filter(Year == 2010 | Year == 2011 | Year == 2012 |
                           Year == 2013)
set4 <- predG %>% filter(Year == 2014 | Year == 2015 | Year == 2016 |
                           Year == 2017)
set5 <- predG %>% filter(Year == 2018 | Year == 2019)

test <- predG %>% filter(Year == 2017)
# GAM maps
ggplot() +
  geom_sf(data = set5, aes(color = Fit)) +
  geom_sf(data = russia) +
  scale_color_viridis(name = "Log(count)") +
  geom_sf(data = ak) +
  coord_sf(crs = st_crs(3338), xlim = c(-2280000, 180000), 
           ylim = c(50000, 2800000), expand = FALSE, 
           datum = st_crs(4326))+
  scale_x_continuous(breaks = c(seq(-180 , -125, by = 7))) +
  facet_wrap(~Year) +
  labs(x = "Longitude", y = "Latitude", 
       title = paste("Juvenile",spec2,"salmon Model 3, GAM")) +
  theme(axis.text.x = element_text(angle = 45, vjust=0.5))

