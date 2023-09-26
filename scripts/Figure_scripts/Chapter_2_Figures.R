#### Chapter 2 Figures in one script ####
# author: Lilian Hart
# date last edited: 09/19/23

require(tidyverse)
require(dplyr)
require(here)
require(viridis)
require(ggmap)
require(ggplot2)
require(rnaturalearth)
require(rnaturalearthhires)
require(sf)
require(rnaturalearthdata)
require(ggspatial)
require(concaveman)
require(FishStatsUtils)
require(VAST)
require(mgcv)
require(gratia)

spec <- "Sockeye"
spec2 <- "sockeye"

# Set directories
dir.data <- here("data", "BASIS")
dir.efh <- here("data", "Chapter_2_EFH")
dir.rds <- here("data", "Chapter_2_RDS")
dir.shelf <- file.path("~/Documents/AK_Shapefiles")
dir.fig <- file.path("~/Documents/Salmon_EFH_Deliverables/Chapter1_Figures/Manuscript")

#### Setup ####
# Import and prepare data
full_data <- readRDS(paste0(dir.data,"/full_basis_combo_data"))
studyarea <- readRDS(file.path(dir.work, "user_region2.rds"))
cshelf <- cshelf <- st_read(file.path(dir.shelf, "AK_CSB.gdb"))
dat <- full_data %>% drop_na(CommonName,Effort_area_km2,TotalCatchNum) %>%
  filter(CommonName != "Pollock", CommonName != "Coho Salmon")
# Save sample year as a factor
dat$fSampleYear <- as.factor(dat$SampleYear)
# Convert data to shapefile
datsf <- st_as_sf(dat, coords = c("EQ.Longitude", "EQ.Latitude"), crs = 4326)
studyareasf <- st_as_sf(studyarea, coords = c("Lon", "Lat"), crs = 4326)

# Mapping attributes #
usa <- ne_states("United States of America", returnclass = "sf")
ak <- subset(usa, name == "Alaska")
ak <- st_set_crs(ak, 4326)
russia <-rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")%>%
  filter(name=="Russia")
russia <- st_set_crs(russia, 4326)
shelf <- st_read(file.path(dir.shelf, "arctic_coast_bathy/arctic_coast_bathy.shp"))
shelfb <- select(shelf, elevation, geometry) %>% 
  filter(elevation == -105 | elevation == -55)
shelfb <- st_transform(shelfb, 4326)
cshelf <- st_read(file.path(dir.shelf, "AK_CSB.gdb"))
# Major AK rivers
rivers <- st_read(file.path(dir.shelf, "USA_Major_Rivers/v10/rivers.gdb"))
riv <- rivers %>% filter(NAME == "Kuskokwim" | NAME == "Yukon" |
                           NAME == "Koyukuk" | NAME == "Stikine")

#### Figure 1: Survey stations and study area ####
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
  theme(axis.title=element_text(size=18),
        axis.text=element_text(size=15),
        strip.text=element_text(size=18),
        title = element_text(size = 18)) +
  xlab("Longitude") +
  ylab("Latitude")

ggsave(device = "jpeg", height = 5, width = 5.5,
              path = dir.fig, filename = "Ch1_Fig1_BASIS_Stations.jpeg")

#### Figure 2 and Appendix B: Smooth effects plots ####
## Load Models 
setwd(dir.rds)
# Cold Pool Extent Model
mod2B <- readRDS(paste0(spec2, "_gam_mod2B.rds"))
# Gather model names
mlc <- "mod2B"
# Set working directory to figures folder
setwd(dir.fig)
# Collect smooth terms through gratia
mod <- mod2B
mod_char <- "mod2B"
# Define smooths to plot
terms <- list("te(surfacetemp)", "te(bottomsal)",
              "te(MixedLayerDepth)")
for(i in 1:3){
  # Pull smooth estimates
  target <- terms[i]
  sm <- smooth_estimates(mod, smooth = as.character(target))
  sm_df <- data.frame(sm)
  # Create new names without parentheses for saving in filename
  newname <- gsub("[()]", "", x = target)
  # Pull column name for x-axis
  cnames <- colnames(sm_df)
  enviro <- cnames[6]
  # Get min and max x-axis values
  xmin <- min(sm_df[,6])
  xmax <- max(sm_df[,6])
  # Plot
  ggplot(sm_df, aes(sm_df[,6]) ) +
    geom_ribbon(aes(ymin = est - (1.96*se), ymax = est + (1.96*se)),
                fill = "grey70") +
    geom_line(aes(y = est)) +
    ggtitle(paste("Effect of", titles[i], "on juvenile\n", spec2,
                  "salmon abundance")) +
    labs(x = titles[i], y = "Effect") +
    theme(axis.title=element_text(size=15),
          axis.text=element_text(size=15),
          strip.text=element_text(size=15),
          title = element_text(size = 15)) 
  ggsave(paste0(spec, "_", mod_char, "_effects", "_", newname, ".jpg"), 
         width = 5, height = 4, units = "in")
} 

## CPE figures
for(i in 1:4){
  # Pull smooth estimates
  target <- "te(CPE)"
  sm <- smooth_estimates(mod, smooth = as.character(target))
  sm_df <- data.frame(sm)
  # Get min and max x-axis values
  xmin <- min(sm_df[,6])
  xmax <- max(sm_df[,6])
  # Plot
  ggplot(sm_df, aes(CPE)) +
    geom_ribbon(aes(ymin = est - (1.96*se), ymax = est + (1.96*se)),
                fill = "grey70") +
    geom_line(aes(y = est)) +
    ggtitle(paste("Effect of CPE on juvenile\n", spec2,
                  "salmon abundance")) +
    labs(x = "CPE", y = "Effect") +
    theme(axis.title=element_text(size=15),
          axis.text=element_text(size=15),
          strip.text=element_text(size=15),
          title = element_text(size = 15)) 
  ggsave(paste0(spec, "_CPE_effects.jpg"), 
         width = 5, height = 4, units = "in")
} 
