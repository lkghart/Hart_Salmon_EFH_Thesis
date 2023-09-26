### Offset test ###
# author: Lilian Hart
# date last modified: 10/25/2022
# Note: testing whether need to specify log() when GAM offset is specified as
# offset = 

require(tidyverse)
require(dplyr)
require(here)
require(viridis)
require(mgcv)
require(visreg)
require(ggplot2)

# Load in data
dir.data <- here("data", "BASIS")
a <- readRDS(file.path(dir.data, "full_basis_combo_data"))
dat <- a %>% filter(CommonName == "Chinook Salmon") %>% 
  drop_na(CommonName, Effort_area_km2, TotalCatchNum)
p_grid <- readRDS(file.path(dir.data,"prediction_grid.rds"))
p_grid <- data.frame(p_grid)

# Fit the model
mod1 <- gam(TotalCatchNum ~ te(EQ.Longitude,EQ.Latitude, bs = "tp", k=10), 
            offset= log(Effort_area_km2), family = tw(link = "log"), data = dat) 

## Generate predictions
# Pull a Lon and Lat to predict
Lon <- min(p_grid$Lon)
Lat <- min(p_grid$Lat)

effort <- c(1,2,3,4,5,5,6,7,8)

for (i in effort){
  print(i)
  temp_pred <-  predict(mod1, 
                        newdata = data.frame(EQ.Longitude=Lon, 
                                             EQ.Latitude=Lat, Effort = i), 
                        type = "response", se=TRUE)
  temp_df <- data.frame(temp_pred)
  
  colnames(temp_df) <- c("Fit", "SE.Fit")
  temp_df$Effort <- i
  temp_df$Lon <- p_grid$Lon
  temp_df$Lat <- p_grid$Lat
  temp_df$CV <- temp_df$SE.Fit / (mean(temp_df$Fit))
  
  pred_1 <- temp_df
}



