#### VAST CV Calculations ###
## author: Lilian Hart
## last edited: 09/29/23

require(tidyverse)
require(dplyr)
require(here)
require(beepr)
require(sf)
require(VAST)
require(FishStatsUtils)
require(TMBhelper)
require(TMB)

# Set directories
dir.data <- here("data", "BASIS")
dir.work <- here("data", "Chapter_1_RDS")

## Workflow ##
# Set species
spec <- "chinook"
years <- as.character(2002:2019)

# Load model 1(static model)
mod1 <- readRDS(file.path(dir.work, paste0(spec,"_VAST_mod1.rds")))
mod1 <- VAST::reload_model(mod1)

# Method 1 use the sample_variable() function from FishStats Utils
sample <- sample_variable(Sdreport = mod1$parameter_estimates$SD, 
                          Obj = mod1$tmb_list$Obj,
                          variable_name = "D_gct")

SE_mod1 <- apply(sample, MARGIN=1:3, FUN=sd )
SE_mod1 <- as.data.frame(SE_mod1)
means <- colMeans(as.data.frame(sample))
CV_mod1 <- SE_mod1/means

#Save to RDS
setwd(dir.work)
saveRDS(CV_mod1, paste0(spec,"_VAST_mod1_CV.rds"))

# Load est-performing dynamic model (model 4)
dyn_mod <- readRDS(file.path(dir.work,paste0(spec,"_VAST_mod4.rds")))
dyn_mod <- VAST::reload_model(dyn_mod)

# Sample predicted densities
sampledm <- sample_variable(Sdreport = dyn_mod$parameter_estimates$SD, 
                          Obj = dyn_mod$tmb_list$Obj,
                          variable_name = "D_gct")

SE_dm <- apply(sampledm, MARGIN=1:3, FUN=sd )
SE_dm <- as.data.frame(SE_dm)
means <- colMeans(as.data.frame(sampledm))
CV_dm <- SE_dm/means
colnames(CV_dm) <- years

#Save to RDS
saveRDS(CV_dm, paste0(spec,"_VAST_dynamic_model_CV.rds"))


