#==================================================================================================
#Project Name: BASIS SALMON - Create BASIS Survey Footprint Shape File for Extrapolation Grid
#Creator: Curry James Cunningham, College of Fisheries and Ocean Sciences, UAF
#Date: 11.18.21
#
#Purpose: To create BASIS extrapolation grid. 
#
#
#
#==================================================================================================
#NOTES:
# Parameters are estimated by identifying the value of fixed effects that maximizes the marginal likelihood when integrated across random effects.  We approximate this multidimensional integral using the Laplace approximation, as implemented using Template Model Builder (Kristensen et al., 2016).
#==================================================================================================
library(here)
wd <- here()

#=================== CREATE USER REGIONS ========================================================================
#https://github.com/James-Thorson-NOAA/VAST/wiki/Creating-an-extrapolation-grid

packageVersion("sf")
library(sp) #1.4.5
library(sf) #0.9.6
library(rgdal)
library(here)
library(splines)
library(sp)  # vector data
library(raster)  # raster data
library(rgdal)  # input/output, projections
library(rgeos)  # geometry ops
library(spdep)  # spatial dependence
library(dplyr)
library(ggplot2)

# Define workflows =============================================================
dir.data <- here("data", "BASIS")
dir.vast <- here("VAST")

# Read in Data =================================================================
dat <- readRDS(file.path(dir.data, "full_basis_combo_data"))
names(dat)

# Filter data for only observations with Lats/Lons =============================
dat.2 <- dat %>% filter(!is.na(EQ.Latitude), !is.na(EQ.Longitude))


par(mfrow=c(1,1))

## Manually click on a plot of the data to define the AOI
# Draw points around data
plot(dat.2$EQ.Longitude, dat.2$EQ.Latitude)
LL <-locator()
# Now click on points around the area of interest. When done, hit ESC key

# Save to an RDS file
saveRDS(LL, file.path(dir.vast, 'extent_LL.rds'))
# Read in the RDS file
LL <-readRDS(file.path(dir.vast,'extent_LL.rds'))
# Create a dataframe to hold values from the RDS file
region_extent <- data.frame(long = LL$x, lat = LL$y)

# Examine the dataframe
str(region_extent)

## Turn into spatial polygon object
# Duplicate the first row point so that it is connected into a polygon
#The rbind() creates a single ring feature
region_extent <-rbind(region_extent, region_extent[1,])
## https://www.maths.lancs.ac.uk/~rowlings/Teaching/Sheffield2013/cheatsheet.html
# Polygon() creates a double ring feature, formal class Polygon

#Change Polygon to polygon
poly <- Polygon(region_extent)

# This is a list...?
polys <- Polygons(list(poly), ID='all')

# Create a spatial polygon
sps <- SpatialPolygons(list(polys))
## I think the F_AREA could be dropped here
sps <- SpatialPolygonsDataFrame(sps, data.frame(Id=factor('all'), F_AREA=1, row.names='all'))

# CRS() refers to coordinate reference system
proj4string(sps)<- CRS("+proj=longlat +datum=WGS84")

#Reprojecting with function spTransform
sps <- spTransform(sps, CRS("+proj=longlat +lat_0=90 +lon_0=180 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 "))

### Get UTM zone for conversion to UTM projection
## retrieves spatial bounding box from spatial data [,1] is
## longitude
lon <- sum(bbox(sps)[1,])/2
## convert decimal degrees to utm zone for average longitude, use
## for new CRS
utmzone <- floor((lon + 180)/6)+1
crs_LL <- CRS('+proj=longlat +ellps=WGS84 +no_defs')
#ERROR: Warning message: In showSRID(uprojargs, format = "PROJ", multiline = "NO", prefer_proj = prefer_proj) :
#Discarded datum Unknown based on WGS84 ellipsoid in Proj4 definition
sps@proj4string <- crs_LL

### End method 1

### Create the VAST extrapolation grid for method 1 and 2
## Convert the final in polygon to UTM
crs_UTM <- CRS(paste0("+proj=utm +zone=",utmzone," +ellps=WGS84 +datum=WGS84 +units=m +no_defs "))
region_polygon <- spTransform(sps, crs_UTM)

### Construct the  extrapolation  grid for VAST using sf package
## Size of grid **in meters** (since working in UTM). Controls
## the resolution of the grid.
cell_size <- 2000
## This step is slow at high resolutions
region_grid <- st_make_grid(region_polygon, cellsize = cell_size, what = "centers")
#Warning messages: CRS object has comment, which is lost in output
# Warning 2: same as warning 1
## Convert region_grid to Spatial Points to SpatialPointsDataFrame
region_grid <- as(region_grid, "Spatial")


region_grid_sp <- as(region_grid, "SpatialPointsDataFrame")
## combine shapefile data (region_polygon) with Spatial Points
## (region_grid_spatial) & place in SpatialPointsDataFrame data
## (this provides you with your strata identifier (here called
## Id) in your data frame))
region_grid_sp@data <- over(region_grid, region_polygon)

## Convert back to lon/lat coordinates as that is what VAST uses
region_grid_LL <- as.data.frame(spTransform(region_grid_sp, crs_LL))
region_df <- with(region_grid_LL,
                  data.frame(Lon=coords.x1,
                             Lat=coords.x2, Id,
                             Area_km2=( (cell_size/1000^2)),
                             row=1:nrow(region_grid_LL)))
## Filter out the grid that does not overlap (outside extent)
region <- subset(region_df, !is.na(Id))
## This is the final file needed.
str(region)
## > 'data.frame': 106654 obs. of  5 variables:
##  $ Lon     : num  -166 -166 -166 -166 -166 ...
##  $ Lat     : num  53.9 53.9 54 53.9 53.9 ...
##  $ Id      : Factor w/ 1 level "all": 1 1 1 1 1 1 1 1 1 1 ...
##  $ Area_km2: num  4 4 4 4 4 4 4 4 4 4 ...
##  $ row     : int  401 402 975 976 977 978 1549 1550 1551 1552 ...

### Save it to be read in and passed to VAST later.
saveRDS(region, file = file.path(dir.vast, "user_region_ALL.rds"))
### End of creating user extrapolation region object

g1 <- png(filename = file.path(dir.vast,"user_region_ALL.png"), width=7, height=7, units='in', res=800)
# The Environment pane says that g1 is NULL, and the "New_extrapolation_area..." 
# won't save in the directory.
ggsave(file.path(dir.vast,"New_extrapolation_area_ALL.png"), plot=g1, height=8, width=9, units='in')

par(mfrow=c(2,2))
with(region_extent, plot(long, lat, main='Extent in points in LL'))

plot(region_polygon, main='Polygon in UTM', axes=TRUE)

plot(region_grid, col=ifelse(is.na(region_df$Id), 'red', 'black'),
     axes=TRUE, main='Extrapolation area UTM')
with(region, plot(Lon, Lat, main='Extrapolation region in LL', pch='.'))

ggsave(file.path(dir.vast,"user_region_ALL2.png"),  height=8, width=9, units='in')

dev.off()


