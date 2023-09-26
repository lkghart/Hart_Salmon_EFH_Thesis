### Deviance Explained - Chinook VAST Models
## author: Lilian Hart
# Last edited 09/08/2022

require(tidyverse)
require(dplyr)
require(ggthemes)
require(VAST)
require(here)
require(FishStatsUtils)
require(units)
require(beepr)

dir.data <- here("data", "BASIS")
dir.work <- here("data", "Chapter_1_RDSModels")
dir.R <- here("R")

## Load models
mod1 <- readRDS(file.path(dir.work,"chinook_VAST_mod1.rds"))
mod2 <- readRDS(file.path(dir.work, "chinook_VAST_mod2.rds"))
mod3 <- readRDS(file.path(dir.work, "chinook_VAST_mod3.rds"))
mod4 <- readRDS(file.path(dir.work, "chinook_VAST_mod4.rds"))
mod1_0 <- readRDS(file.path(dir.work, "chinook_VAST_mod1_null.rds"))
mod2_0 <- readRDS(file.path(dir.work, "chinook_VAST_mod2_null.rds"))
mod3_0 <- readRDS(file.path(dir.work, "chinook_VAST_mod3_null.rds"))
mod4_0 <- readRDS(file.path(dir.work, "chinook_VAST_mod4_null.rds"))

## Calculate percent deviance explained
(p1 <- 1-mod1$Report$deviance/mod1_0$Report$deviance) #0.617139 is almost 62%
(p2 <- 1-mod2$Report$deviance/mod2_0$Report$deviance) #0.6558708 is almost 66%
(p3 <- 1-mod3$Report$deviance/mod3_0$Report$deviance) #0.7564717 is almost 76%
(p4 <- 1-mod4$Report$deviance/mod4_0$Report$deviance) #0.7018406 is almost 70%




