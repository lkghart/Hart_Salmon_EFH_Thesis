#### Chapter 1 Figure scripts ####
# author: Lilian Hart
# date last edited: 12/30/2022

require(tidyverse)
require(dplyr)
require(here)
require(viridis)
require(mgcv)
# require(equatiomatic)
require(visreg)
require(ggmap)
require(rnaturalearth)
require(rnaturalearthhires)
require(sf)
require(sp)
require(rnaturalearthdata)
require(ggspatial)
require(viridis)

# Set directories
dir.data <- here("data", "BASIS")
dir.clip <- here("data", "Chapter_1_RDSModels")
dir.shelf <- file.path("~/Documents/AK_Shapefiles")

# Import data
full_data <- readRDS(paste0(dir.data,"/full_basis_combo_data"))
studyarea <- readRDS(file.path(dir.clip, "user_region2.rds"))
cshelf <- cshelf <- st_read(file.path(dir.shelf, "AK_CSB.gdb"))

# Clean data and convert data
dat <- full_data %>% drop_na(CommonName,Effort_area_km2,TotalCatchNum) %>%
  filter(CommonName != "Pollock", CommonName != "Coho Salmon")
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
# 
# #ggmap mapping attributes
# left <- min(dat$EQ.Longitude, na.rm=TRUE)
# right <- max(dat$EQ.Longitude, na.rm=TRUE)
# bottom <- min(dat$EQ.Latitude, na.rm=TRUE)
# top <- max(dat$EQ.Latitude, na.rm=TRUE)
# map.dat <- get_map(location=c(left, bottom,
#                               right, top),
#                    maptype='toner-lite', source='stamen', crop=TRUE)

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
  facet_wrap(~fSampleYear, ncol = 6) +
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

# Figure 1: Survey stations and study area
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


# Figure 2: Sockeye salmon presence absence map
ggmap(map.dat) +
  theme_linedraw() +  
  ggtitle("Sockeye Salmon Presence/Absence")+
  xlab("Longitude") +
  ylab("Latitude") +
  labs(shape = "Occurrence\n") +
  facet_wrap(~SampleYear, nrow = 2) +
  theme(legend.position = "right", axis.title=element_text(size = 20), title = 
          element_text(size = 15), legend.title = element_text(size = 15),
        legend.text = element_text(size = 12)) +
  geom_text(x=-159, y = 62, label = "Alaska", color = "white") +
  geom_text(x=-170, y = 70, label = "Chuckchi\n Sea", size = 3, color = "white") +
  geom_text(x= -172, y = 58, label = "Bering\n Sea", size = 3, color = "white") +
  scale_shape_manual(values = c(1,16), labels = c("absence", "presence")) +
  geom_point( data = sockeye,
              aes(x=EQ.Longitude, y=EQ.Latitude, shape=pres_abs))

# Figure 3: Positive catch records
dat2 <- full_data %>% filter(CommonName != "Pollock") %>%
  filter(TotalCatchWt != 0) %>% drop_na(CommonName,Effort_area_km2,TotalCatchNum)
dat2$fCommonName <- as.factor(dat2$CommonName)
ggplot(dat2, aes(x=CommonName)) + geom_bar() + 
  ggtitle("Positive catch records, BASIS 2002-2019")

# Figure 4: Percentages of zero inflated catch data by year
#### Calculate Zero catch proportions ####
total_events <- 2053
dat3 <- dat %>% group_by(CommonName, SampleYear, pres_abs) %>% summarize(count=n())
chinook <- dat3 %>% filter(CommonName =="Chinook Salmon")
chum <- dat3 %>% filter(CommonName == "Chum Salmon")
pink <- dat3 %>% filter(CommonName == "Pink Salmon")
sockeye <- dat3 %>% filter(CommonName == "Sockeye Salmon")

ggplot(chinook, aes(x=SampleYear, fill = pres_abs, y= count)) + 
  geom_bar(position = "fill", stat="identity") + 
  ggtitle("Chinook Salmon") +
  xlab("") + labs(fill = "Presence (1)/Absence (0)") 

ggplot(chum, aes(x=SampleYear, fill = pres_abs, y= count)) + 
  geom_bar(position = "fill", stat="identity") + 
  ggtitle("Chum Salmon") +
  xlab("") + labs(fill = "Presence (1)/Absence (0)") 

ggplot(pink, aes(x=SampleYear, fill = pres_abs, y= count)) + 
  geom_bar(position = "fill", stat="identity") + 
  ggtitle("Pink Salmon") +
  xlab("") + labs(fill = "Presence (1)/Absence (0)") 

ggplot(sockeye, aes(x=SampleYear, fill = pres_abs, y= count)) + 
  geom_bar(position = "fill", stat="identity") + 
  ggtitle("Sockeye Salmon") +
  xlab("") + labs(fill = "Presence (1)/Absence (0)") 

## Figure 5: 95% habitat maps
#Loading static model

pred1 <- readRDS("Chum_GAM_Mod1_Predictions_link.rds")
pred1b <- readRDS("Chum_GAM_Mod1_Predictions.rds")
# Calculate 5th percentile/quantile
tile <- quantile(pred1$Fit,0.05) 
quantile(pred1$Fit, 0.95)
tileb <- quantile(pred1b$Fit,0.05)

# Create dataframe with a new column that says NA if below the 5th 
# percentile, or gives the estimated Fit if above.
dat <- pred1 %>% mutate(core = ifelse(Fit > tile, Fit, NA))
datb <- pred1b %>% mutate(core = ifelse(Fit > tileb, Fit, NA))

## Plot 95% Occurrence map
# Load in mapping attributes
usa <- ne_states("United States of America", returnclass = "sf")
ak <- subset(usa, name == "Alaska")
#get russia
russia<-rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")%>%
  filter(name=="Russia")
ak <- st_set_crs(ak, 4326)
russia <- st_set_crs(russia, 4326)
dat2 <- pred1 %>% mutate(core = ifelse(Fit > tile & Fit >= exp(0), Fit, NA))
dat2 <- st_as_sf(dat2, coords = c("Lon", "Lat"), crs = 4326)
dat2b <- pred1b %>% mutate(core = ifelse(Fit > tileb & Fit >= exp(0), Fit, NA))
dat2b <- st_as_sf(dat2b, coords = c("Lon", "Lat"), crs = 4326)

ggplot() +
  geom_sf(data=russia) + 
  geom_sf(data = dat2, mapping = aes(color = core)) +
  geom_sf(data = ak) +
  coord_sf(crs = st_crs(3338), xlim = c(-2280000, 180000), 
           ylim = c(50000, 2800000), expand = FALSE, 
           datum = st_crs(4326))+
  scale_x_continuous(breaks = c(seq(-180 , -125, by = 7)))+
  scale_color_viridis(name = "log(count)") +
  labs(x = "Longitude", y = "Latitude", 
       title = "Juvenile chum salmon\n 95th Quantile of Abundance",
                     fill = "Density")

ggplot() +
  geom_sf(data=russia) + 
  geom_sf(data = dat2b, mapping = aes(color = core)) +
  geom_sf(data = ak) +
  coord_sf(crs = st_crs(3338), xlim = c(-2280000, 180000), 
           ylim = c(50000, 2800000), expand = FALSE, 
           datum = st_crs(4326))+
  scale_x_continuous(breaks = c(seq(-180 , -125, by = 7)))+
  scale_color_viridis(name = "count") +
  labs(x = "Longitude", y = "Latitude", 
       title = "Juvenile chum salmon\n 95th Quantile of Abundance",
       fill = "Density")

## Figure 6: VAST chum estimates

## Figure 7: Comparisons of abundance estimates
species <- "Chinook Salmon"
Spec <- "Chinook"
spec <- "chinook"

dir.data <- here("data", "Chapter_1_RDSModels")
dir.vast <- here("VAST", species)
setwd(dir.data)

#mod2 <- readRDS(paste0(spec,"_gam_mod2.rds"))
mod4 <- readRDS(paste0(spec,"_gam_mod4.rds"))

#pred_2 <- readRDS(paste0(Spec,"_GAM_Mod2_Predictions.rds"))
pred_4 <- readRDS(paste0(Spec,"_GAM_Mod4_Predictions.rds"))

vast4 <- read.csv(file.path(dir.vast,"Mod_4","Index.csv"))
head(vast4)

# Total all of the responses by year to get predicted total relative abundance
dat4 <- pred_4 %>% group_by(Year) %>% summarize(Count = sum(Fit))
as.data.frame(dat4)
# Summary statistics
summary(dat4$Count)

# Combine data for better plotting
a <- data.frame(stat_class = "GAM", Count = dat4$Count, Year = as.factor(dat4$Year))
b <- data.frame(stat_class = "VAST", Count = vast4$Estimate, Year = as.factor(vast4$Time))
combo4 <- rbind(a,b)

## Plot index
plot1 <- ggplot(data = combo4, aes(x=Year, y=log(Count), col = stat_class)) + 
  geom_point() + 
  xlab("Sample Year") + ylab("Index\n log(count)") +
  scale_x_discrete(breaks = c(2002,2005, 2008, 2011, 2014, 2017)) + 
  geom_line(aes(x=as.numeric(Year))) + ggtitle("Chinook")

species <- "Chum Salmon"
Spec <- "Chum"
spec <- "chum"

dir.data <- here("data", "Chapter_1_RDSModels")
dir.vast <- here("VAST", species)
setwd(dir.data)

mod3 <- readRDS(paste0(spec,"_gam_mod3.rds"))
mod4 <- readRDS(paste0(spec,"_gam_mod4.rds"))

pred_3 <- readRDS(paste0(Spec,"_GAM_Mod3_Predictions.rds"))
pred_4 <- readRDS(paste0(Spec,"_GAM_Mod4_Predictions.rds"))

vast3 <- read.csv(file.path(dir.vast,"Mod_3","Index.csv"))
vast4 <- read.csv(file.path(dir.vast,"Mod_4","Index.csv"))
head(vast4)

# Total all of the responses by year to get predicted total relative abundance
dat3 <- pred_3 %>% group_by(Year) %>% summarize(Count = sum(Fit))
as.data.frame(dat3)
dat4 <- pred_4 %>% group_by(Year) %>% summarize(Count = sum(Fit))
as.data.frame(dat4)
# Summary statistics
summary(dat3$Count)
summary(dat4$Count)

# Combine data for better plotting
a <- data.frame(stat_class = "GAM", Count = dat3$Count, Year = as.factor(dat3$Year))
b <- data.frame(stat_class = "VAST", Count = vast3$Estimate, Year = as.factor(vast3$Time))
combo3 <- rbind(a,b)

a <- data.frame(stat_class = "GAM", Count = dat4$Count, Year = as.factor(dat4$Year))
b <- data.frame(stat_class = "VAST", Count = vast4$Estimate, Year = as.factor(vast4$Time))
combo4 <- rbind(a,b)

## Plot index
plot1 <- ggplot(data = combo3, aes(x=Year, y=log(Count), col = stat_class)) + 
  geom_point() + 
  xlab("Sample Year") + ylab("Index\n log(count)") +
  scale_x_discrete(breaks = c(2002,2005, 2008, 2011, 2014, 2017)) + 
  geom_line(aes(x=as.numeric(Year))) + ggtitle("Chum")
