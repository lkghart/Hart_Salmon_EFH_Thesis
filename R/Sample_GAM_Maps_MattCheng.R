# Load in libraries -------------------------------------------------------

library(tidyverse)
library(here)
library(lubridate)
library(mgcv)
library(visreg)
library(LaCroixColoR)
library(coefplot)

# for mapping
library(remote)
library(rnaturalearth)
library(rnaturalearthhires)
library(gganimate)
library(sf)
library(rnaturalearthdata)


# Setting up spatial plotting stuff ---------------------------------------

# mapping attributes that we may need
usa <- ne_states("United States of America", returnclass = "sf")

usa_sf <- st_as_sf(usa)

ak <- subset(usa, name == "Alaska")

ak <- st_shift_longitude(ak)


# Fit GAM to Target Longline Sablefish sets
mod_l_2 <- bam(log(cpue + 1) ~ year + s(bottom_depth) + te(AdjLon, lat_dd_end) + s(skates_in_set) 
               + s(vessel_length) + s(duration_min) + s(haul_doy), data = target_longline)

# creating a spatial field for predictions
preds_long <- with(target_longline_preds,
                   expand.grid(year = factor(1995:2021),
                               bottom_depth = 598, # 613 is the mean depth
                               vessel_length = 85, # mean vessel length
                               duration_min = 918, # mean duration
                               haul_doy = 160,# mean doy
                               lat_dd_end = seq(min(lat_dd_end), max(lat_dd_end), length = 100),
                               AdjLon = seq(min(AdjLon), max(AdjLon), length = 100),
                               skates_in_set = 19))

preds <- predict(mod_l_2, preds_long) # make predictions 


ind <- exclude.too.far(preds_long$AdjLon, preds_long$lat_dd_end, target_longline_preds$AdjLon,
                       target_longline_preds$lat_dd_end, dist = 0.1) # preventing large extrapolations


preds[ind] <- NA # not showing any extrapolation!


ggplot(data=world)+
  geom_raster(preds, mapping = aes(x=AdjLon,y=lat_dd_end,fill=preds))  +
  geom_sf(data=world,fill="gray70")+
  coord_sf(xlim=c(130,237.5),ylim=c(30,70))+
  theme_bw()+
  scale_fill_viridis_c(na.value = "transparent")+
  facet_wrap(~Year)+
  labs(x = "Longitude", y = "Latitude", title = "Longline", fill = "log CPUE")+
  theme(legend.position = "bottom", axis.title=element_text(size=14),
        axis.text=element_text(size=13),panel.background = element_rect(fill="azure"),
        strip.text=element_text(size=14),
        plot.title = element_text(hjust = 0.5))