### Chapter 1 K-fold Cross Validation, Model 4 ###
# authors: Lilian Hart and Curry Cunningham
# With a great deal of contribution from James Thorson's tutorial in the VAST wiki.
# Tutorial: https://github.com/James-Thorson-NOAA/VAST/wiki/Crossvalidation

# Date last edited: 09/29/23

# Set species
# Note: if running spatiotemporal models for sockeye, change k value to k = 8
Spec <- "Chinook"
spec <- "chinook"

## Load packages
library(TMB)
library(VAST)
library(here)
library(tidyverse)
library(dplyr)
library(mgcv)
library(tweedie)

## Set local working directory (change for your machine)
dir.res <- here("data", "Chapter_1_RDS", "Chapter_1_CrossVal", spec, "Mod_4")
dir.create(dir.res, recursive=TRUE)
dir.dat <- here("data", "Chapter_1_RDS")
setwd(dir.res)

## Load data
dat <- readRDS(file.path(dir.dat, "basis_subset.rds"))
dat$fSampleYear <- as.factor(dat$SampleYear)
species.dat <- subset(dat, dat$CommonName == paste(Spec, "Salmon"))
# Missing values already filtered out of dataset

## Configure Model ##
# Specify Number of knots (i.e. spatial complexity) and ObsModel
n_x <- 300
Region <- "User"
fine_scale <- TRUE

ObsModel=c(2,0) #Standard delta model with gamma dist.

treat_nonencounter_as_zero <- TRUE

FieldConfig <- array("IID", dim=c(3,2), 
                     dimnames=list(c("Omega","Epsilon","Beta"),
                                   c("Component_1", "Component_2")))

# Make settings (turning off bias.correct to save time for example)
settings <- make_settings(n_x = n_x, 
                          Region=Region,
                          purpose = "index2", 
                          strata.limits = data.frame(STRATA="All_areas"),
                          fine_scale = fine_scale, 
                          bias.correct = FALSE,
                          ObsModel=ObsModel,
                          treat_nonencounter_as_zero=treat_nonencounter_as_zero,
                          FieldConfig = FieldConfig,
                          RhoConfig = c(0,0,0,0),
                          Version = "VAST_v14_0_1")

# Fit the model and a first time and record MLE
# Read input grid
user_region <- readRDS(file.path(dir.dat, "user_region.rds"))

# Run model
mod_fit <- fit_model(settings = settings, 
                     Lat_i = species.dat$EQ.Latitude, 
                     Lon_i = species.dat$EQ.Longitude, 
                     t_i = species.dat$SampleYear, 
                     c_i = rep(0,nrow(species.dat)),
                     b_i = as_units(species.dat$TotalCatchNum, 'count'), 
                     a_i = as_units(species.dat$Effort_area_km2, 'km^2'),
                     input_grid = user_region)

ParHat <- mod_fit$ParHat

# Generate partitions in data
n_fold = 10
Partition_i = sample( 1:n_fold,
                      size=nrow(species.dat),
                      replace=TRUE )

prednll_vast <- rep(NA, n_fold)
prednll_gam <- rep(NA, n_fold)

# Loop through partitions, refitting each time with a different PredTF_i
fI <- 1
for( fI in 1:n_fold ) {
  print(paste("##### Running fold", fI, "of", n_fold))
  
  PredTF_i = ifelse( Partition_i==fI, TRUE, FALSE )
  
  # Refit, starting at MLE, without calculating standard errors (to save time)
  fit_new = fit_model( settings = settings,
                       Lat_i = species.dat$EQ.Latitude, 
                       Lon_i = species.dat$EQ.Longitude, 
                       t_i = species.dat$SampleYear, 
                       c_i = rep(0,nrow(species.dat)),
                       b_i = as_units(species.dat$TotalCatchNum, 'count'), 
                       a_i = as_units(species.dat$Effort_area_km2, 'km^2'),
                       input_grid = user_region,
                       "PredTF_i"=PredTF_i,
                       "Parameters"=ParHat,
                       "getsd"=FALSE )
  
  # Save fit to out-of-bag data
  prednll_vast[fI] = fit_new$Report$pred_jnll
  
  # # Fit GAM Model =====================================
  # 
  # # Partition Data
  # temp.train <- species.dat[PredTF_i==FALSE,]
  # temp.test <- species.dat[PredTF_i==TRUE,]
  # 
  # # Fit Model to Training Data Set
  # # GAM Model #1 Structure: Average spatial field
  # temp.fit <- gam(TotalCatchNum ~ te(EQ.Longitude,EQ.Latitude, bs = "tp", k = 8,
  #                                    m = 1) + fSampleYear +
  #                   te(EQ.Longitude,EQ.Latitude, by = fSampleYear, bs = "tp", k = 8,
  #                      m = 1) +
  #                   offset(log(Effort_area_km2)),
  #                 family=tw(link = "log"), data = temp.train)
  # 
  # # Predict Testing Data Set
  # pred.test <- predict(temp.fit, newdata=temp.test, type="response")
  # 
  # # Calculate Negative LogLikelihood of Testing Observations, given Predictions
  # #   under Tweedie
  # 
  # # Extract Tweedie Dispersion
  # temp.phi <- summary(temp.fit)$dispersion
  # 
  # # Extract power term
  # p_temp<- data.frame(p=c(temp.fit$family$family)) %>%
  #   separate(p, 10, into=c("delete", "p")) %>%
  #   separate(p, -1, into=c("p", "delete")) %>% 
  #   mutate( p = as.numeric(p))
  # temp.p <- p_temp$p
  # 
  # Tweedie Deviance Function Check
  #   This test is to ensure that we can replicate the logLikelihood of the original 
  #     model fit to the training dataset, using the extracted power and dispersion
  #      parameter values, and the observed and predicted catch numbers in the
  #        training dataset
  # obs.train <- temp.train$TotalCatchNum
  # pred.train <- predict(temp.fit, type="response")
  # trial.probs <- sum(log(tweedie::dtweedie(y=obs.train, power=temp.p, mu=pred.train, phi=temp.phi)))
  # logLik(temp.fit);trial.probs
  # Check! Yes, we can replicate log Likelihood of the original fit.
  
  # Calculate negative logLikelihood of observations in the testing dataset,
  #   given the predicted values for the testing dataset, and fitted model
  #     power term (temp.p) and disperstion estimate (temp.phi)
  # prednll_gam[fI] <- -1*sum(log(tweedie::dtweedie(y=temp.test$TotalCatchNum, power=temp.p, 
                                       # mu=pred.test, phi=temp.phi)))

  
}

# Check fit to all out-of-bag data and use as metric of out-of-bag performance
x <- sum( prednll_vast )
# y <- sum( prednll_gam)
y <- 1
res <- c("vast:",x,"gam:",y)
saveRDS(res, paste0("nll_", spec, "_results.rds"))


