### Importing Shapefiles from Geodatabases ###
require(sf)
require(here)
require(ggplot2)
require(tidyverse)
require(dplyr)

dir.dat <- file.path("~/Documents/AK_Shapefiles")
shelf <- st_read(file.path(dir.dat, "AK_CSB.gdb"), 
                 layer = "AK_CONTINENTAL_SHELF_BDRY")
#Checking what layers exist
shelf_layers <- st_layers(dsn = file.path(dir.dat, "AK_CSB.gdb"))
