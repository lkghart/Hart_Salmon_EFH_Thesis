#### Chapter 1 Figure scripts ####
# author: Lilian Hart
# date last edited: 10/31/2022
# Note: Code for figures of survey stations over time, area of interest/user region,
# and plots of zero-inflation in the data by species

require(tidyverse)
require(dplyr)
require(here)
require(viridis)
require(mgcv)
require(visreg)
require(rnaturalearth)
require(rnaturalearthhires)
require(sf)
require(sp)
require(rnaturalearthdata)
require(ggspatial)
require(viridis)

#### Setup ####
# Set directories
dir.data <- here("data", "BASIS")
dir.clip <- here("data", "Chapter_1_RDS")
dir.shelf <- file.path("~/Documents/AK_Shapefiles")

# Import data
full_data <- readRDS(paste0(dir.data,"/full_basis_combo_data"))
studyarea <- readRDS(file.path(dir.clip, "user_region.rds"))
cshelf <- cshelf <- st_read(file.path(dir.shelf, "AK_CSB.gdb"))

# Clean data and convert data
dat <- full_data %>% drop_na(CommonName,Effort_area_km2,TotalCatchNum) %>%
  filter(CommonName != "Pollock")
# Save sample year as a factor
dat$fSampleYear <- as.factor(dat$SampleYear)
# Convert data to shapefile
datsf <- st_as_sf(dat, coords = c("EQ.Longitude", "EQ.Latitude"), crs = 4326)
studyareasf <- st_as_sf(studyarea, coords = c("Lon", "Lat"), crs = 4326)

## Load and set map attributes
# Shapefile mapping attributes
usa <- ne_states("United States of America", returnclass = "sf")
ak <- subset(usa, name == "Alaska")
ak <- st_set_crs(ak, 4326)
russia <-rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")%>%
  filter(name=="Russia")
russia <- st_set_crs(russia, 4326)

#Subset species
chinook <- dat %>% filter(CommonName == "Chinook Salmon")
chum <- dat %>% filter(CommonName == "Chum Salmon")
pink <- dat %>% filter(CommonName == "Pink Salmon")
sockeye <- dat %>% filter(CommonName == "Sockeye Salmon")
coho <- dat %>% filter(CommonName == "Coho Salmon")

# Appendix A: Survey stations across time
aa <- ggplot() +
  geom_sf(data = datsf, size = .25) +
  geom_sf(data = russia, fill = "#CCCCCC") +
  geom_sf(data = cshelf, color = "white") +
  geom_sf(data = ak, fill = "#CCCCCC") +
  facet_wrap(~fSampleYear, ncol = 4) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        strip.text=element_text(size=15)) +
  coord_sf(crs = st_crs(3338), xlim = c(-2280000, 180000), 
           ylim = c(50000, 2800000), expand = FALSE, 
           datum = st_crs(4326))

# Figure: Survey stations and study area
ggplot() +
  geom_sf(data = studyareasf, fill = alpha("blue", 0.2), color = "blue") +
  geom_sf(data = datsf, size = .25) +
  geom_sf(data = russia, fill = "#CCCCCC") +
  geom_sf(data = cshelf, color = "white") +
  geom_sf(data = ak, fill = "#CCCCCC") +
  coord_sf(crs = st_crs(3338), xlim = c(-2280000, 180000), 
           ylim = c(50000, 2800000), expand = FALSE, 
           datum = st_crs(4326))+
  scale_x_continuous(breaks = c(seq(-180 , -125, by = 7)))+
  annotate("text", label = "Russia", x=-1700000, y=2300000, color = "white",
           size = 4) +
  annotate("text", label = "Alaska", x=-110000, y=1500000, color = "white", size = 4)+
  annotate("text", x= -1500000, y = 1000000, label = "Bering\n Sea", size = 4, color = "white") +
  annotate("text", x= -780000, y = 2300000, label = "Chukchi\n Sea", size = 4, color = "white") +
  annotate("text", label = "Continental Shelf", x=-1900000, y=1250000, 
           color = "white", size = 3, angle = 23) +
  xlab("Longitude") +
  ylab("Latitude")

# Figure: Positive catch records
dat2 <- full_data %>% filter(CommonName != "Pollock") %>%
  filter(TotalCatchWt != 0) %>% drop_na(CommonName,Effort_area_km2,TotalCatchNum)
dat2$fCommonName <- as.factor(dat2$CommonName)
ggplot(dat2, aes(x=CommonName)) + geom_bar() + 
  ggtitle("Positive catch records, BASIS 2002-2019")

# Figure: Percentages of zero inflated catch data by year
# Calculate totals
total_events <- 2053
dat3 <- dat %>% group_by(CommonName, SampleYear, pres_abs) %>% summarize(count=n())
chinook <- dat3 %>% filter(CommonName =="Chinook Salmon")
chum <- dat3 %>% filter(CommonName == "Chum Salmon")
coho <- dat3 %>% filter(CommonName == "Coho Salmon")
pink <- dat3 %>% filter(CommonName == "Pink Salmon")
sockeye <- dat3 %>% filter(CommonName == "Sockeye Salmon")
# Plots
ggplot(chinook, aes(x=SampleYear, fill = pres_abs, y= count)) + 
  geom_bar(position = "fill", stat="identity") + 
  ggtitle("Chinook Salmon") +
  xlab("") + labs(fill = "Presence (1)/Absence (0)") 

ggplot(chum, aes(x=SampleYear, fill = pres_abs, y= count)) + 
  geom_bar(position = "fill", stat="identity") + 
  ggtitle("Chum Salmon") +
  xlab("") + labs(fill = "Presence (1)/Absence (0)") 

ggplot(coho, aes(x=SampleYear, fill = pres_abs, y= count)) + 
  geom_bar(position = "fill", stat="identity") + 
  ggtitle("Coho Salmon") +
  xlab("") + labs(fill = "Presence (1)/Absence (0)") 

ggplot(pink, aes(x=SampleYear, fill = pres_abs, y= count)) + 
  geom_bar(position = "fill", stat="identity") + 
  ggtitle("Pink Salmon") +
  xlab("") + labs(fill = "Presence (1)/Absence (0)") 

ggplot(sockeye, aes(x=SampleYear, fill = pres_abs, y= count)) + 
  geom_bar(position = "fill", stat="identity") + 
  ggtitle("Sockeye Salmon") +
  xlab("") + labs(fill = "Presence (1)/Absence (0)") 


