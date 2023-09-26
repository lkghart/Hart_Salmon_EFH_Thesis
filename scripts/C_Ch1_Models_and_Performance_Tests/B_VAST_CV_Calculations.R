#### VAST CV Calculations for best model (Model 3) ###
## author: Lilian Hart
## last edited: 02/06/23

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
dir.work <- here("data", "Chapter_1_RDSModels")

## Workflow ##
# Set species
spec <- "chinook"
years <- as.character(2002:2019)

# Load model 1(static model)
mod1 <- readRDS(file.path(dir.work,paste0(spec,"_VAST_mod1.rds")))
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

# Load model 3/4 (best model)
mod3 <- readRDS(file.path(dir.work,paste0(spec,"_VAST_mod3.rds")))
mod3 <- VAST::reload_model(mod3)

# Sample predicted densities
sample3 <- sample_variable(Sdreport = mod3$parameter_estimates$SD, 
                          Obj = mod3$tmb_list$Obj,
                          variable_name = "D_gct")

SE_mod3 <- apply(sample, MARGIN=1:3, FUN=sd )
SE_mod3 <- as.data.frame(SE_mod3)
means <- colMeans(as.data.frame(sample))
CV_mod3 <- SE_mod3/means
colnames(CV_mod3) <- years

#Save to RDS
saveRDS(CV_mod3, paste0(spec,"_VAST_CV.rds"))


