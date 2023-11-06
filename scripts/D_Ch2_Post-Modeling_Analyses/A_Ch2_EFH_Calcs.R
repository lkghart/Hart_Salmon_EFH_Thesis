### Ch2 EFH Calculation Script ###
# author: Lilian Hart
# date last edited: 06/07/23

require(tidyverse)
require(dplyr)
require(here)
require(viridis)
require(mgcv)
require(ggplot2)

#### Workflow ####
dir.dat <- here("data", "Chapter_2_RDS")
dir.work <- here("data", "Chapter_2_EFH")

Spec <- "Chum"
spec <- tolower(Spec)

## GAM EFH Calculations ##
#### Model 1A - In-Situ Environmental Covariates EFH ####
# Load in predictions
preds <- readRDS(file.path(dir.dat, paste0(spec, "_gam_in-situ_predictions.rds")))
preds <- as.data.frame(preds)
# Order predictions by Fit size
preds2 <- preds %>% arrange(desc(fit))
# Calculate top 95%. First get total.
(fit_total <- sum(preds$fit)) #26029
# Calculate individual proportion of total
preds2$Proportion <- preds2$fit/fit_total
# Calculate cumulative summary
preds2$CumSum <- cumsum(preds2$Proportion)
# Find the 95% cutoff.
preds3 <- preds2 %>% filter(CumSum <= 0.95)
# Find the 50% core habitat cutoff
preds4 <- preds2 %>% filter(CumSum <= .50)
nrow(preds2) # From 10,000 prediction points to
nrow(preds3) #7721 prediction points.

# Save EFH and core EFH as rds file
saveRDS(preds3, file.path(dir.work, paste0(spec, "_gam_in-situ_EFH.rds")))
saveRDS(preds4, file.path(dir.work, paste0(spec, "_gam_in-situ_EFH_50.rds")))

#### Cold Pool Extent Model EFH ####

# Low CPE 
preds <- readRDS(file.path(dir.dat, paste0(spec, "_gam_CPE-low_predictions.rds")))
preds <- as.data.frame(preds)
preds2 <- preds %>% arrange(desc(fit))
(fit_total <- sum(preds$fit)) #31313.64
preds2$Proportion <- preds2$fit/fit_total
preds2$CumSum <- cumsum(preds2$Proportion)
preds3 <- preds2 %>% filter(CumSum <= 0.95)
preds4 <- preds2 %>% filter(CumSum <= .50)
nrow(preds2) # From 10,000 prediction points to
nrow(preds3) #8317 prediction points.

saveRDS(preds3, file.path(dir.work, paste0(spec, "_gam_CPE-low_EFH.rds")))
saveRDS(preds4, file.path(dir.work, paste0(spec, "_gam_CPE-low_EFH_50.rds")))

# Median CPE 
preds <- readRDS(file.path(dir.dat, paste0(spec, "_gam_CPE-median_predictions.rds")))
preds <- as.data.frame(preds)
preds2 <- preds %>% arrange(desc(fit))
(fit_total <- sum(preds$fit)) #24010.85
preds2$Proportion <- preds2$fit/fit_total
preds2$CumSum <- cumsum(preds2$Proportion)
preds3 <- preds2 %>% filter(CumSum <= 0.95)
preds4 <- preds2 %>% filter(CumSum <= .50)
nrow(preds2) # From 10,000 prediction points to
nrow(preds3) #7361 prediction points.

saveRDS(preds3, file.path(dir.work, paste0(spec, "_gam_CPE-med_EFH.rds")))
saveRDS(preds4, file.path(dir.work, paste0(spec, "_gam_CPE-med_EFH_50.rds")))

# High CPE 
preds <- readRDS(file.path(dir.dat, paste0(spec, "_gam_CPE-high_predictions.rds")))
preds <- as.data.frame(preds)
preds2 <- preds %>% arrange(desc(fit))
(fit_total <- sum(preds$fit)) #15103.35
preds2$Proportion <- preds2$fit/fit_total
preds2$CumSum <- cumsum(preds2$Proportion)
preds3 <- preds2 %>% filter(CumSum <= 0.95)
preds4 <- preds2 %>% filter(CumSum <= .50)
nrow(preds2) # From 10,000 prediction points to
nrow(preds3) #4374 prediction points.

saveRDS(preds3, file.path(dir.work, paste0(spec, "_gam_CPE-high_EFH.rds")))
saveRDS(preds4, file.path(dir.work, paste0(spec, "_gam_CPE-high_EFH_50.rds")))

#### Climate Stanzas ####
# Warm1 Climate Stanza (2002-2005) 
preds <- readRDS(file.path(dir.dat, paste0(spec, "_gam_Warm1_predictions.rds")))
preds <- as.data.frame(preds)
preds2 <- preds %>% arrange(desc(fit))
(fit_total <- sum(preds$fit)) #15103.35
preds2$Proportion <- preds2$fit/fit_total
preds2$CumSum <- cumsum(preds2$Proportion)
preds3 <- preds2 %>% filter(CumSum <= 0.95)
preds4 <- preds2 %>% filter(CumSum <= .50)

saveRDS(preds3, file.path(dir.work, paste0(spec, "_gam_Warm1_EFH.rds")))
saveRDS(preds4, file.path(dir.work, paste0(spec, "_gam_Warm1_EFH_50.rds")))

# Cool Climate Stanza (2006-2013) 
preds <- readRDS(file.path(dir.dat, paste0(spec, "_gam_Cool_predictions.rds")))
preds <- as.data.frame(preds)
preds2 <- preds %>% arrange(desc(fit))
(fit_total <- sum(preds$fit)) #15103.35
preds2$Proportion <- preds2$fit/fit_total
preds2$CumSum <- cumsum(preds2$Proportion)
preds3 <- preds2 %>% filter(CumSum <= 0.95)
preds4 <- preds2 %>% filter(CumSum <= .50)

saveRDS(preds3, file.path(dir.work, paste0(spec, "_gam_Cool_EFH.rds")))
saveRDS(preds4, file.path(dir.work, paste0(spec, "_gam_Cool_EFH_50.rds")))

# Warm2 Climate Stanza (2014-2019) 
preds <- readRDS(file.path(dir.dat, paste0(spec, "_gam_Warm2_predictions.rds")))
preds <- as.data.frame(preds)
preds2 <- preds %>% arrange(desc(fit))
(fit_total <- sum(preds$fit)) #15103.35
preds2$Proportion <- preds2$fit/fit_total
preds2$CumSum <- cumsum(preds2$Proportion)
preds3 <- preds2 %>% filter(CumSum <= 0.95)
preds4 <- preds2 %>% filter(CumSum <= .50)

saveRDS(preds3, file.path(dir.work, paste0(spec, "_gam_Warm2_EFH.rds")))
saveRDS(preds4, file.path(dir.work, paste0(spec, "_gam_Warm2_EFH_50.rds")))

#### Even/Odd Years ####
# Even Years 
preds <- readRDS(file.path(dir.dat, paste0(spec, "_gam_Even-year_predictions.rds")))
preds <- as.data.frame(preds)
preds2 <- preds %>% arrange(desc(fit))
(fit_total <- sum(preds$fit))
preds2$Proportion <- preds2$fit/fit_total
preds2$CumSum <- cumsum(preds2$Proportion)
preds3 <- preds2 %>% filter(CumSum <= 0.95)
preds4 <- preds2 %>% filter(CumSum <= .50)

saveRDS(preds3, file.path(dir.work, paste0(spec, "_gam_Even-year_EFH.rds")))
saveRDS(preds4, file.path(dir.work, paste0(spec, "_gam_Even-year_EFH_50.rds")))

# Odd Years 
preds <- readRDS(file.path(dir.dat, paste0(spec, "_gam_Odd-year_predictions.rds")))
preds <- as.data.frame(preds)
preds2 <- preds %>% arrange(desc(fit))
(fit_total <- sum(preds$fit))
preds2$Proportion <- preds2$fit/fit_total
preds2$CumSum <- cumsum(preds2$Proportion)
preds3 <- preds2 %>% filter(CumSum <= 0.95)
preds4 <- preds2 %>% filter(CumSum <= .50)

saveRDS(preds3, file.path(dir.work, paste0(spec, "_gam_Odd-year_EFH.rds")))
saveRDS(preds4, file.path(dir.work, paste0(spec, "_gam_Odd-year_EFH_50.rds")))
