### VAST Model 3 ###
## author: Lilian Hart
# Last edited 09/29/2023

require(tidyverse)
require(dplyr)
require(ggthemes)
require(VAST)
require(here)
require(FishStatsUtils)
require(units)
require(beepr)

#### Setup ####
species <- "Chum"
spec <- tolower(Spec)

fit <- TRUE

if(fit == TRUE){
  species <- paste0(Spec, "Salmon")
  dir.data <- here("data", "BASIS")
  dir.work <- here("data", "Chapter_1_RDS")
  dir.vast <- here("data", "Chapter_1_RDS", "VAST", spec, "Mod_3")
  dir.create(dir.vast, recursive=TRUE)
  
  # Number of knots (i.e. spatial complexity)
  n_x <- 300
  Region <- "User"
  fine_scale <- TRUE
  
  ObsModel=c(2,0) #Standard delta model with gamma distribution
  treat_nonencounter_as_zero <- TRUE
  
  # Read in Compiled Basis Data ==================================================
  full_data <- readRDS(file.path(dir.work,"basis_subset.rds"))
  species.dat <- subset(full_data, full_data$CommonName == species)
  
  # Filter out observations with missing values
  temp.dat <- species.dat %>% filter(!is.na(EQ.Longitude),
                                     !is.na(EQ.Latitude),
                                     !is.na(Effort_area_km2),
                                     !is.na(TotalCatchNum))
  
  # Configure model ====================================================================
  setwd(dir.vast)
  FieldConfig <- array("IID", dim=c(3,2), 
                       dimnames=list(c("Omega","Epsilon","Beta"),
                                     c("Component_1", "Component_2")))
  
  settings <- make_settings(n_x = n_x, 
                            Region=Region,
                            purpose = "index2", 
                            strata.limits = data.frame(STRATA="All_areas"),
                            fine_scale = fine_scale, 
                            bias.correct = TRUE,
                            ObsModel=ObsModel,
                            treat_nonencounter_as_zero=treat_nonencounter_as_zero,
                            FieldConfig = FieldConfig,
                            RhoConfig = c(0,0,2,2),
                            Version = "VAST_v14_0_1")
  
  # Read input grid
  user_region <- readRDS(file.path(dir.work, "user_region.rds"))
  
  # Run model
  mod_fit <- fit_model(settings = settings, 
                       Lat_i = temp.dat$EQ.Latitude, 
                       Lon_i = temp.dat$EQ.Longitude, 
                       t_i = temp.dat$SampleYear, 
                       c_i = rep(0,nrow(temp.dat)),
                       b_i = as_units(temp.dat$TotalCatchNum, 'count'), 
                       a_i = as_units(temp.dat$Effort_area_km2, 'km^2'),
                       input_grid = user_region); beep(sound=2)
  
  saveRDS(mod_fit, file.path(dir.work,paste0(spec,"_VAST_mod3.rds")))
  
  ## Create null models ##
  settings0 <- settings
  settings0$FieldConfig = matrix( c(0,0,0,0,"IID","IID"), byrow=TRUE, ncol=2 )
  settings0$RhoConfig[c("Beta1","Beta2")] = 3
  settings0$RhoConfig[c("Epsilon1","Epsilon2")]=0
  fit0 <- fit_model(settings = settings0, 
                    Lat_i = temp.dat$EQ.Latitude, 
                    Lon_i = temp.dat$EQ.Longitude, 
                    t_i = temp.dat$SampleYear, 
                    c_i = rep(0,nrow(temp.dat)),
                    b_i = as_units(temp.dat$TotalCatchNum, 'count'), 
                    a_i = as_units(temp.dat$Effort_area_km2, 'km^2'),
                    input_grid = user_region,
                    getsd = FALSE,
                    newtonsteps = 1)
  
  # The summary said: Error in (function...) Spatiotemporal variation is turned
  # off for a component with temporal structure, and this combination doesn't
  # make sense
  saveRDS(fit0, file.path(dir.work, paste0(spec, "_VAST_mod3_null.rds")))
  
  # Plot Output ==================================================================
  plot(mod_fit); beep(sound=2)
  mod_fit$parameter_estimates$AIC
  
} else {
  print("Model is fitted. Loading RDS files.")
  dir.work <- here("data", "Chapter_1_RDS")
  mod_fit <- readRDS(file.path(dir.work, paste0(spec, "_VAST_mod3.rds")))
  mod_fit_0 <- readRDS(file.path(dir.work, paste0(spec, "_VAST_mod3_null.rds")))
}

# Reset working directory ======================================================
setwd(here())
  
  
  