### Step B: Define Area of Interest (AOI) ###
# author: Lilian Hart
# last edited: 09/22/23
require(tidyverse)
require(dplyr)
require(here)
require(viridis)
require(mgcv)
require(visreg)
require(ggplot2)
require(ggmap)
require(gratia)
require(beepr)
require(sf)

## Read in the data ##
dir.vast <- here("data", "BASIS")
dir.work <- here("data","Chapter_1_RDS")
setwd(dir.work)

og <- readRDS(paste0(dir.vast,"/full_basis_combo_data"))
dat <- og %>% filter(CommonName != "Pollock") %>%
  filter(CommonName != "Coho Salmon") %>% drop_na(CommonName,
                                                  Effort_area_km2,
                                                  TotalCatchNum)

## Manually click on a plot of the data to define the AOI
plot(x=dat$EQ.Longitude, y = dat$EQ.Latitude, pch = 20, cex = 0.4)
LL <-locator(type = "l")
# Now click on points around the area of interest. When done, hit ESC key

# Save to an RDS file
saveRDS(LL, 'AOI_extent.rds')
LL <-readRDS('AOI_extent.rds')
# Create a dataframe to hold values from the RDS file
region_extent <- data.frame(long = LL$x, lat = LL$y)
# Examine the dataframe
str(region_extent)

## Turn into spatial polygon object
# Duplicate the first row point so that it is connected into a polygon
#The rbind() creates a single ring feature
region_extent <-rbind(region_extent, region_extent[1,])

# Polygon() creates a double ring feature, formal class Polygon
poly <- Polygon(region_extent)
# This is a list.
polys <- Polygons(list(poly), ID='all')

# Create a spatial polygon
sps <- SpatialPolygons(list(polys))
## I think the F_AREA could be dropped here
sps <- SpatialPolygonsDataFrame(sps, data.frame(Id=factor('all'), F_AREA=1, row.names='all'))

# CRS() refers to coordinate reference system
proj4string(sps)<- CRS("+proj=longlat +datum=WGS84")
saveRDS(sps, "AOI_polygon.rds")
