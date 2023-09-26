### Thesis proposal Figures ###
# Lilian Hart

require(tidyverse)
require(dplyr)
require(here)
require(viridis)
require(mgcv)
require(visreg)
require(beepr)
library(rnaturalearth)
library(rnaturalearthhires)
library(gganimate)
library(sf)
library(rnaturalearthdata)
require(ggspatial)

dat <- readRDS(paste0(here(),"/data/BASIS/full_basis_combo_data"))
dat2 <- dat %>% filter(TotalCatchWt != 0)

dat2 <- dat %>% filter(SampleYear == c(2008,2014))

dir.work <- here("data", "Chapter_1_RDSModels")
setwd(dir.work)

# Histogram of total catches
dat2$fCommonName <- as.factor(dat2$CommonName)
ggplot(dat2, aes(x=CommonName)) + geom_bar() + 
  ggtitle("Positive catch records, BASIS 2002-2019")

# Zero inflation
dat <- dat %>% filter(CommonName != "Coho Salmon" & CommonName != "Pollock")
dat3 <- dat %>% group_by(CommonName, SampleYear, pres_abs) %>% summarize(count=n())
ggplot(dat3, aes(x=CommonName, fill = pres_abs, y= count)) + 
  geom_bar(position = "fill", stat="identity") + 
  ggtitle("Percentages of zero-inflated catch data by species \n 2002-2019") +
  xlab("") + labs(fill = "Presence (1)/Absence (0)") 

# Now by species
chinook <- dat3 %>% filter(CommonName =="Chinook Salmon")
chum <- dat3 %>% filter(CommonName == "Chum Salmon")
pink <- dat3 %>% filter(CommonName == "Pink Salmon")
sock <- dat3 %>% filter(CommonName == "Sockeye Salmon")

ggplot(chinook, aes(x=SampleYear, fill = pres_abs, y= count)) + 
  geom_bar(position = "fill", stat="identity") + 
  ggtitle("Percentages of zero-inflated catch data by year \n Chinook Salmon") +
  xlab("") + labs(fill = "Presence (1)/Absence (0)") 

ggplot(chum, aes(x=SampleYear, fill = pres_abs, y= count)) + 
  geom_bar(position = "fill", stat="identity") + 
  ggtitle("Percentages of zero-inflated catch data by year \n Chum Salmon") +
  xlab("") + labs(fill = "Presence (1)/Absence (0)") 

ggplot(pink, aes(x=SampleYear, fill = pres_abs, y= count)) + 
  geom_bar(position = "fill", stat="identity") + 
  ggtitle("Percentages of zero-inflated catch data by year \n Pink Salmon") +
  xlab("") + labs(fill = "Presence (1)/Absence (0)") 

ggplot(sock, aes(x=SampleYear, fill = pres_abs, y= count)) + 
  geom_bar(position = "fill", stat="identity") + 
  ggtitle("Percentages of zero-inflated catch data by year \n Sockeye Salmon") +
  xlab("") + labs(fill = "Presence (1)/Absence (0)") 

map.dat <- get_map(location=c(left, bottom,
                              right, top),
                   maptype='toner-lite', source='stamen', crop=TRUE)
mapa <- ggmap(map.dat) +
  theme_linedraw() +  
  geom_point( data = dat2,
              aes(x=EQ.Longitude, y=EQ.Latitude, color=log(cpueWt)), alpha=0.5) +
  facet_wrap(~SampleYear, ncol = 6) +
  ggtitle("Chinook CPUE by Weight") +
  scale_color_viridis() +
  xlab("Longitude") +
  ylab("Latitutde")
mapa
