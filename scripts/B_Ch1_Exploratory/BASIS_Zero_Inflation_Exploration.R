### BASIS Zero Inflation Exploration ###
## author: Lilian Hart
# Last edited: 01/28/23
# Note: Goal is to classify/characterize the extent of coverage for
# each studied salmon species in each year of the survey

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

#### Setup ####
dir.work <- here("data", "Chapter_1_RDSModels")
# Read in the clipped data used for fitting the models.
og <- readRDS(file.path(dir.work,"v3_basis_subset.rds"))
setwd(dir.work)

# For loop: for each species of interest, query the number of positive 
# catch records in each year, calculate the percentage of positive catches
# to zero catches, and assign a coverage classification according to
# this percentage.

### Analyze positive catches by species  ###
spec_list <- c("Chinook Salmon", "Chum Salmon", "Pink Salmon", "Sockeye Salmon")
# Initialize empty dataframe
coverage_results <- data.frame()
for(i in spec_list){
  dat <- og %>% filter(CommonName == i)
  yearstats <- dat %>% count(SampleYear)
  specstats <- dat %>% filter(TotalCatchNum >0) %>% count(SampleYear) %>%
    mutate(prop_positive = n/yearstats$n)
  specstats <- specstats %>% 
    mutate(coverage = cut(prop_positive, breaks=c(-Inf, 0.4, 0.6, Inf), 
                          labels=c("poor","moderate","good")))
  specstats$CommonName <- i
  coverage_results <- rbind(coverage_results, specstats)
}

### Analyze yearly coverage vs max coverage #
# Note: Max coverage is here defined as the max number of stations ever 
# sampled in one year
year_max <- 191
subset <- og %>% filter(CommonName == "Chinook Salmon") #1884 total records
sample_extents <- subset %>% count(SampleYear) %>% 
  mutate(prop_cover = n/191) %>% 
  mutate(sample_coverage = cut(prop_cover, breaks=c(-Inf, 0.4, 0.6, Inf), 
                               labels=c("poor","moderate","good")))

