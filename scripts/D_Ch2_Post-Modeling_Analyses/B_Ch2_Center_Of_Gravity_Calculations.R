### Ch2 Center of Gravity Calculations
## Author: Lilian Hart 
## Last edited: 08/22/23

require(tidyverse)
require(dplyr)
require(here)
require(mgcv)
require(ggplot2)
require(gratia)
require(sf)
require(SDMTools)

Spec <- "Chum"
spec <- tolower(Spec)

#### Setup ####
dir.mod <- here("data", "Chapter_2_EFH")
dir.out <- here("data", "Chapter_2_RDS")
setwd(dir.mod)

# Load core model (top 50%) predictions for in-situ and SVC models
insitu <- readRDS(paste0(spec,"_gam_in-situ_EFH_50.rds"))
cpe_low <- readRDS(paste0(spec, "_gam_CPE-low_EFH_50.rds"))
cpe_med <- readRDS(paste0(spec, "_gam_CPE-med_EFH_50.rds"))
cpe_high <- readRDS(paste0(spec, "_gam_CPE-high_EFH_50.rds"))
warm1 <- readRDS(paste0(spec, "_gam_Warm1_EFH_50.rds"))
cool <- readRDS(paste0(spec, "_gam_Cool_EFH_50.rds"))
warm2 <- readRDS(paste0(spec, "_gam_Warm2_EFH_50.rds"))
even <- readRDS(paste0(spec, "_gam_Even-year_EFH_50.rds"))
odd <- readRDS(paste0(spec, "_gam_Odd-year_EFH_50.rds"))

# Condense into a list
preds <- list(insitu, cpe_low, cpe_med, cpe_high, warm1, cool, warm2,
           even, odd)

#### Conduct Center of Gravity calculations ####
out_df <- data.frame("COG_Longitude" = NA,
                     "Long_SD" = NA, "COG_Latitude" = NA,
                     "Lat_SD" = NA)
for (i in 1:9) {
  a <- as.data.frame(preds[i])
  cg_i <- COGravity(x = a$Lon, y = a$Lat)
  out_df[i,] <- cg_i
}

# Add model labels
out_df$Model_variant <- c("Model1A", "Model2B_LowCPE", "Model2B_MedianCPE",
                          "Model2B_HighCPE", "Model3B_Warm1", "Model3B_Cool",
                          "Model3B_Warm2", "Model4B_Even", "Model4B_Odd")
# Save to file
setwd(dir.out)
write.csv(out_df, file = paste0("COG_df_", spec, ".csv"))





