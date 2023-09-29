### Chapter 1 GAM Output Pulling Script ###
# author: Lilian Hart
# last modified: 09/29/23
# Note: Can load RDS file outputs in second section of code

require(tidyverse)
require(dplyr)
require(here)
require(mgcv)

#### Pull data ####
spec_list <- c("chinook", "chum", "pink", "sockeye")
dir.work <- here("data", "Chapter_1_RDS")
setwd(dir.work)

for (i in spec_list){
  species <- i # Set species for filepath names
  # Load fitted models
  mod1 <- readRDS(file.path(dir.work, paste0(species,"_gam_mod1.rds")))
  mod2 <- readRDS(file.path(dir.work, paste0(species,"_gam_mod2.rds")))
  mod3 <- readRDS(file.path(dir.work, paste0(species,"_gam_mod3.rds")))
  mod4 <- readRDS(file.path(dir.work, paste0(species,"_gam_mod4.rds")))

  
  # Set up array for outputs
  mod_list <- list(mod1, mod2, mod3, mod4)
  mod_names <- c("mod1", "mod2", "mod3", "mod4")
  cnames <- c("Species", "Model", "Deviance_Explained", "AIC")
  spec_out <- array(dim=c(4, 4)) %>% as.data.frame()
  colnames(spec_out) <- cnames

  # Loop through models

  for (i in 1:4){
    nums <- c(1,2,3,4)
    a <- as.character(species)
    b <- paste0("Model_", nums[i])
    c <- summary(mod_list[[i]])$dev.expl
    d <- AIC(mod_list[[i]])
    spec_out[i,] <- c(a,b,c,d)
    saveRDS(spec_out, file = paste0(species, "_GAM_metrics.rds"))
  }
}

#### Load data outputs ####
chinook_out <- readRDS("chinook_GAM_metrics.rds")
chum_out <- readRDS("chum_GAM_metrics.rds")
pink_out <- readRDS("pink_GAM_metrics.rds")
sockeye_out <- readRDS("sockeye_GAM_metrics.rds")
