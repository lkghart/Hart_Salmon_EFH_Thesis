library(tidyverse)
library(here)

dir.data <- here("data","BASIS")
script.data <- here("scripts")
#import catch and event data
event <- read.csv(file.choose())
catch <- read.csv(file.choose())
mld <- read.csv(file.choose())

#Generate full list of stationID+EventCodes
stations <- subset(event,select=c(StationID,EventCode))

#unique list of species+lhs codes
catch_species <- subset(catch,select=c(CommonName,LHSCode))
catch_unique <- unique(catch_species[c("CommonName","LHSCode")])

#generate paired down catch file to make for easier joining later
catch_subset <- subset(catch,select=c(StationID,EventCode,CommonName,LHSCode,
                                      TotalCatchNum,TotalCatchWt))
#generate all permutations of stations+catch
data_frame <-expand_grid(catch_unique,stations)

#join data frame with paired down catch frame
catch_0s <- left_join(data_frame,catch_subset)

#replace NAs with 0s
catch_0s[is.na(catch_0s)] <- 0

#from here you can join with event data to add rows on stationid+eventcode or 
#with the catch file by joining on stationid+eventcode+common_name+lhscode
full_data <- inner_join(event, catch_0s, by=c("StationID","EventCode"))
saveRDS( object = full_data, file = file.path( dir.data, "full_basis_catch_data") )
write.csv(full_data, file = file.path(dir.data, "full_basis_catch_data.csv"))

                       