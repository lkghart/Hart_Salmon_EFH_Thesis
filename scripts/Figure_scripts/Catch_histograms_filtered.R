#### BASIS Catch data, filtered ####
## author: Lilian Hart
# date last modified: 10/20/22
#Note: Records filtered to include just those used to train the models

require(here)
require(tidyverse)
require(dplyr)
require(ggplot2)

## Import the data
dir.data <- here("data", "BASIS")
dat <- readRDS(file.path(dir.data,"full_basis_combo_data"))
dat$CommonName <- as.factor(dat$CommonName)
dat <- dat %>% drop_na(CommonName,Effort_area_km2,TotalCatchNum)
dat$fSampleYear <- as.factor(dat$SampleYear)
# Subset
chinook <- dat %>% filter(CommonName == "Chinook Salmon") %>% 
  group_by(SampleYear) %>% summarize(TotalCatch = sum(TotalCatchNum))
chum <- dat %>% filter(CommonName == "Chum Salmon") %>%
  group_by(SampleYear) %>% summarize(TotalCatch = sum(TotalCatchNum))
pink <- dat %>% filter(CommonName == "Pink Salmon") %>%
  group_by(SampleYear) %>% summarize(TotalCatch = sum(TotalCatchNum))
sockeye <- dat %>% filter(CommonName == "Sockeye Salmon") %>%
  group_by(SampleYear) %>% summarize(TotalCatch = sum(TotalCatchNum))

# Histograms of total catch
ggplot(aes(x=SampleYear, y=TotalCatch), data = chinook) +
  geom_bar(stat="identity") + ggtitle("Chinook salmon Catch by Year")

ggplot(aes(x=SampleYear, y=TotalCatch), data = chum) +
  geom_bar(stat="identity") + ggtitle("Chum salmon Catch by Year")  

ggplot(aes(x=SampleYear, y=TotalCatch), data = pink) +
  geom_bar(stat="identity") + ggtitle("Pink salmon Catch by Year")

ggplot(aes(x=SampleYear, y=TotalCatch), data = sockeye) +
  geom_bar(stat="identity") + ggtitle("Sockeye salmon Catch by Year")

# Histograms of max catch by species
maxchi <- dat %>% filter(CommonName != "Coho Salmon") %>% 
  filter(CommonName != "Pollock") %>% group_by(CommonName) %>%
  summarize(MaxCatch = max(TotalCatchNum))
ggplot(data = maxchi, aes(x=CommonName, y=MaxCatch)) +
  geom_bar(stat = "identity")


