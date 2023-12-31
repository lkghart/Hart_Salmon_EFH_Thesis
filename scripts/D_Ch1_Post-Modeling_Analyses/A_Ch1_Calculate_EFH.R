### Ch1 Calculate EFH ###
# author: Lilian Hart
# date last edited: 02/29/23

require(tidyverse)
require(dplyr)
require(here)
require(viridis)
require(mgcv)
require(visreg)
require(ggplot2)
require(FishStatsUtils)
require(VAST)

# Set species
spec <- "chum"

dir.dat <- here("data", "Chapter_1_RDS")
dir.work <- here("data", "Chapter_1_EFH")

years <- c(2002:2019)

#### GAM EFH Calculations ####
## Model 1 - Static EFH ##
mod <- "Mod1"

# Load in predictions
preds <- readRDS(file.path(dir.dat, paste0(spec, "_GAM_",
                                            mod, "_Predictions_Response.rds")))
preds <- as.data.frame(preds)
# Order predictions by Fit size
preds2 <- preds %>% arrange(desc(Fit))
# Calculate top 95%. First get total.
(fit_total <- sum(preds$Fit)) 
# Calculate individual proportion of total
preds2$Proportion <- preds2$Fit/fit_total
# Calculate cumulative summary
preds2$CumSum <- cumsum(preds2$Proportion)
# Find the 95% cutoff.
preds3 <- preds2 %>% filter(CumSum <= 0.95)
# Find the 50% core habitat cutoff
preds4 <- preds2 %>% filter(CumSum <= .50)
nrow(preds2) # From 10,000 prediction points to
nrow(preds3) #4547 prediction points.

# Save EFH and core EFH as rds file
saveRDS(preds3, file.path(dir.work, paste0(spec, "_GAM_", mod, "_EFH.rds")))
saveRDS(preds4, file.path(dir.work, paste0(spec, "_GAM_", mod, "_EFH_50.rds")))

## Dynamic EFH: Model 4 - Dynamic model with independent annual spatial fields ##
mod <- "Mod4"

# Load in GAM predictions
preds <- readRDS(file.path(dir.dat, paste0(spec, "_GAM_",
                                           mod, "_Predictions_Response.rds")))
preds <- as.data.frame(preds)
# Order predictions by Fit size
preds2 <- preds %>% group_by(Year) %>% arrange(desc(Fit)) 
# Calculate total predicted numerical catch for each year
preds2 <- preds2 %>% group_by(Year) %>% mutate(tot = sum(Fit))

for (i in 1:18){
  year <- years[i]
  x <- preds2 %>% filter(Year == year)
  assign(paste0("df_",year), as.data.frame(x))
}

# Create list of new dataframes for for loop
df_list <- list(df_2002, df_2003, df_2004, df_2005, df_2006, df_2007, df_2008,
             df_2009, df_2010, df_2011, df_2012, df_2013, df_2014, df_2015,
             df_2016, df_2017, df_2018, df_2019)

gam4_efh <- data.frame()
gam4b_efh <- data.frame()
for (i in 1:18){
  year <- years[i]
  df <- as.data.frame(df_list[i])
  df$Proportion <- df$Fit/df$tot
  df <- df %>% arrange(desc(Proportion))
  df$CumSum <- cumsum(df$Proportion)
  # Find the 95% cutoff
  df2 <- df %>% filter(CumSum <= 0.95)
  gam4_efh <- rbind(gam4_efh, df2)
  # Find the 50% core habitat
  df3 <- df %>% filter(CumSum <= .50)
  gam4b_efh <- rbind(gam4b_efh, df3)
}

# Save out
saveRDS(gam4_efh, file.path(dir.work, paste0(spec,"_GAM_",mod,"_EFH.rds")))
saveRDS(gam4b_efh, file.path(dir.work, paste0(spec,"_GAM_",mod,"_EFH_50.rds")))

#### VAST EFH Calculations ####
## Model 1 - Static EFH ##
mod <- "Mod1"
# Read in model
vast1 <- readRDS(file.path(dir.dat, paste0(spec,"_VAST_mod1.rds")))
vast1 <- reload_model(vast1)
# Reformat dataframe
sites <- as.data.frame(vast1$extrapolation_list$Data_Extrap) %>% 
  drop_na(Include) %>% dplyr::select(Lon, Lat)
vast1 <- as.data.frame(vast1$Report$D_gct) %>% drop_units()
colnames(vast1) <- years
vast1 <- vast1[,1]
vast1_CV <- readRDS(file.path(dir.dat, paste0(spec, "_VAST_mod1_CV.rds")))
colnames(vast1_CV) <- years
test <- rowMeans(vast1_CV)
a <- data.frame(Fit = vast1, CV = test, Lat = sites$Lat, Lon = sites$Lon)
efh1_V <- a %>% arrange(desc(Fit))
# Calculate top 95%. First get total.
(fit_total <- sum(efh1_V$Fit)) # 30224.89
# Calculate individual proportion of total
efh1_V$Proportion <- efh1_V$Fit/fit_total
# Calculate cumulative summary
efh1_V$CumSum <- cumsum(efh1_V$Proportion)
# Find the 95% cutoff.
efh95 <- efh1_V %>% filter(CumSum <= 0.95)
efh50 <- efh1_V %>% filter(CumSum <= 0.50)

nrow(a) # From 3,000 prediction points to
nrow(efh1_V) #1,169 prediction points.
#Save to RDS file
saveRDS(efh95, file.path(dir.work, paste0(spec, "_VAST_", mod, "_EFH.rds")))
saveRDS(efh50, file.path(dir.work, paste0(spec, "_VAST_", mod, "_EFH_50.rds")))


## Model 4 - Dynamic model with independent annual spatial fields ##
mod <- "Mod4"
vast4 <- readRDS(file.path(dir.dat ,paste0(spec,"_VAST_mod4.rds")))
vast4 <- reload_model(vast4)
sites <- as.data.frame(vast4$extrapolation_list$Data_Extrap) %>% 
  drop_na(Include) %>% dplyr::select(Lon, Lat)
vast4 <- as.data.frame(vast4$Report$D_gct) %>% drop_units()
colnames(vast4) <- years
vast4_CV <- readRDS(file.path(dir.dat, paste0(spec, "_VAST_dynamic_model_CV.rds")))
# Reformat for better plotting, and add CV
est4_V <- data.frame()
for (i in 1:18){
  x <- years[i]
  a <- vast4[,i]
  b <- vast4_CV[,i]
  c <- data.frame(Fit = a, CV = b, Year = x, Lat = sites$Lat, Lon = sites$Lon)
  colnames(c) <- c("Fit", "CV", "Year", "Lat", "Lon")
  est4_V <- rbind(est4_V, c)
}

# Calculate total predicted numerical catch for each year
preds2 <- est4_V %>% group_by(Year) %>% mutate(tot = sum(Fit))

for (i in 1:18){
  year <- years[i]
  x <- preds2 %>% filter(Year == year)
  assign(paste0("df_",year), as.data.frame(x))
}

# Create list of new dataframes for for loop
df_list <- list(df_2002, df_2003, df_2004, df_2005, df_2006, df_2007, df_2008,
                df_2009, df_2010, df_2011, df_2012, df_2013, df_2014, df_2015,
                df_2016, df_2017, df_2018, df_2019)

vast4_efh <- data.frame()
vast4b_efh <- data.frame()
for (i in 1:18){
  year <- years[i]
  df <- as.data.frame(df_list[i])
  df$Proportion <- df$Fit/df$tot
  df <- df %>% arrange(desc(Proportion))
  df$CumSum <- cumsum(df$Proportion)
  # Find the 95% cutoff
  df2 <- df %>% filter(CumSum <= 0.95)
  df2b <- df %>% filter(CumSum <= .50)
  vast4_efh <- rbind(vast4_efh, df2)
  vast4b_efh <- rbind(vast4b_efh, df2b)
  
}
# Save to RDS
saveRDS(vast4_efh, file.path(dir.work, paste0(spec,"_VAST_",mod,"_EFH.rds"))) 
saveRDS(vast4b_efh, file.path(dir.work, paste0(spec,"_VAST_",mod,"_EFH_50.rds"))) 


nrow(est4_V) # From 54,000 prediction points to
nrow(vast4_efh) #24,334 prediction points.
