### Chapter 2 Correlation Tests ###
# author: Lilian Hart
# date last modified: 10/31/23

require(dplyr)
require(tidyverse)
require(here)
require(corrplot)
require(ggcorrplot)

# Load data
dir.dat <- here::here("data", "BASIS")
dat <- readRDS(file=paste0(dir.dat, "/Ch2_dataframe.rds"))
enviro <- readRDS(file=paste0(dir.dat, "/Ch2_enviro_covariates.rds"))

# Subset by species to test MLD correlations
chinook <- dat %>% filter(CommonName == "Chinook Salmon")
c_e <- chinook %>% dplyr::select(surfacetemp, bottomsal, MixedLayerDepth, CPE,
                          Climate_stanza, Even_odd_year)
pink <- dat %>% filter(CommonName == "Pink Salmon")
p_e <- pink %>% dplyr::select(surfacetemp, bottomsal, MixedLayerDepth, CPE,
                          Climate_stanza, Even_odd_year)

## Correlation plot of environmental covariates ##
#e_cor <- cor(enviro) # This doesn't work because of the categorical variables.
model.matrix(~0+., data=enviro) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(show.diag=FALSE, type="lower", lab=TRUE, lab_size=2)

# For Chinook
model.matrix(~0+., data=c_e) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(show.diag=FALSE, type="lower", lab=TRUE, lab_size=2)

# For Pink
model.matrix(~0+., data=p_e) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(show.diag=FALSE, type="lower", lab=TRUE, lab_size=2)

