### BASIS data exploration script ###
## author: Lilian Hart
## date: 05/13/21

#### Set up workspace ####
require(here)
require(knitr)
require(tidyverse)
library(ggplot2)
require(ggmap)
require(viridis)
require(dplyr)
require(ggsn)

## Load in the data 
dir.data <- here("data", "BASIS")
catch <- read.csv( file = file.path( dir.data, "Catch.txt"), header=TRUE)
event <- read.csv( file = file.path( dir.data, "Event_trawl.txt"), header = TRUE)
mld <- read.csv( file = file.path( dir.data, "MLD.txt"), header = TRUE)

#### Isolate the NAs in the Catch CSV ####
catchwt_nulls <- catch %>% filter(is.na(TotalCatchWt)) %>% 
                                    filter(CommonName != "Pollock")
( catch%>% filter(is.na(TotalCatchNum)) %>% filter(CommonName != "Pollock") %>%
    nrow())
# There are 27 NAs for weight, and zero for number in the Catch csv                                 
# Save as RDS file
saveRDS( object = catchwt_nulls, file = file.path( dir.data, "catch_object_nulls") )

#### Are there more events than catch records? Filtered by what condition? ####
str(event)
event_stationID <- unique(event$StationID)
# There are 2051 unique Station IDs
str(catch)
catch_stationID <- unique(catch$StationID)
# There are 1968 unique station IDs in the catch records. So some are filtered.
(a <- setdiff(event_stationID, catch_stationID))
# There are 345 unique events (StationIDs) that were filtered out
# Subset records using StationID
filtered_events <- data.frame(matrix(ncol = 18))
names(filtered_events) <- colnames(event)
head(filtered_events)
for(i in a){
  row_n <- event[event$StationID == i,]
  filtered_events <- rbind(filtered_events, row_n)
}
write.csv(filtered_events, "data/filtered_events.csv")
View(filtered_events)
# Check that the Station IDs listed do not match any records in the catch file
test <- catch[catch$StationID == 20020101065,]
# There are no records. When I do this with the event file, one record shows up.

## Make lists of the unique attribute levels and see if anything stands out
factor_cols <- filtered_events[,2:7]
attribute_names <- colnames(factor_cols)
for(i in 1:6){
  temp <- unique(factor_cols[i])
  assign(paste0('levels_',attribute_names[i]),temp)
  print(attribute_names[i])
  print(temp)
}
# Tow Type has only one level in the filtered events
(unique(event$Tow.Type))
# But so does the original data.
# Find out how many records contain a NA somewhere
# test_2 <- filtered_events[filtered_events %>% is.na(),] 
# Find out how many StationIds there are--duplicates? 
test_3 <- unique(filtered_events$StationID)
# There are 345 unique values, which means that 
# none of the Station Ids are repeated in the filtered events
# Are there any vessels that were filtered out?
(vessels <- unique(event$VesselName))
# Nope, these are all the same as those in the filtered events

#### Encounter probabilities ####
naughts <- chumsalmon_subdat[chumsalmon_subdat$TotalCatchWt <= 0,]
naughts

#### Does each StationID have every species of salmon represented? ####
stations <- data.frame("StatID" = unique(catch$StationID), "Spec_Count" = NA)
spec_counts <- data.frame("StatID" = NA, "Spec_Count" = NA)
iterations <- 0
for (i in stations$StatID){
  print(i)
  iterations <- iterations +1
  print(iterations)
  records <- catch[which(catch$StationID == i),]
  specs <- nrow(records)
  print(specs)
  spec_counts <- rbind(spec_counts, c(i,specs))
}
single_stations <- spec_counts[which(spec_counts$Spec_Count == 1),1]
# There are 613 StationIDs with just one species
# What is the max number of species? Apparently, the max is 10 records.
unique(spec_counts$Spec_Count)
#> unique(spec_counts$Spec_Count)
#[1] NA  2  3  1  4  5  6  8  7 10
tens <- spec_counts[which(spec_counts$Spec_Count == 10),1]
(tens_record <- catch[which(catch$StationID == tens),])
# It looks like one StationID can have multiple event records
# This is why species can be represented more than once.
# How many unique event codes are there?
(event_ints <- unique(catch$EventCode))
# [1] 3 4 1 2 5 6 7
# There are only seven unique event codes. What do these represent?