#### Step A: Join BASIS excel sheets into one dataframe ####
## author: Andrew Dimond, with edits by Lilian Hart
# date: 10/31/23
# This script joins the BASIS event, catch, mixed layer depth, and zero catch 
#data into one dataframe object, then saves it to an RDS.

#### Set up workspace and load in data ####
library(tidyverse)
library(here)
library(dplyr)

dir.data <- here("data","BASIS")

event <- read.csv(paste0(dir.data, "/Event_trawl.txt"))
catch <- read.csv(paste0(dir.data, "/Catch.txt"))
mld <- read.csv(paste0(dir.data, "/MLD.txt"))

#### Create zero catch records ####
#Unique list of species + LHS Codes
catch_2 <- catch[,]
catch_2$LHSCode[catch_2$LHSCode=="j"] <- "J" #Changes the one sockeye record 
# to match rest of records.

#Generate full list of stationID+EventCodes
stations <- subset(event,select=c(StationID,EventCode))

catch_species <- subset(catch_2,select=c(CommonName,LHSCode))
catch_unique <- unique(catch_species[c("CommonName","LHSCode")])

#Generate paired down catch file to make for easier joining later
catch_subset <- subset(catch_2,select=c(StationID,EventCode,CommonName,LHSCode,
                                      TotalCatchNum,TotalCatchWt))

#Generate all permutations of stations+catch
data_frame <-expand_grid(catch_unique,stations)

#Join data frame with paired down catch frame
catch_0s <- left_join(data_frame,catch_subset)

#Replace NAs with 0s
catch_0s[is.na(catch_0s)] <- 0

#### Joins ####
#Join with event data to add rows on stationid+eventcode 
penultimate_data <- inner_join(event, catch_0s, by=c("StationID","EventCode"))
#Join with MLD (environmental conditions)
full_data <- right_join(mld, penultimate_data, by="StationID")
# Add presence/absence column
full_data_new <- full_data %>% mutate(pres_abs = if_else(TotalCatchWt>0,1,0))
full_data_new$pres_abs <- as.factor(full_data_new$pres_abs)
#### Save to RDS file ####
saveRDS( object = full_data_new, file = file.path( dir.data, "full_basis_combo_data") )




