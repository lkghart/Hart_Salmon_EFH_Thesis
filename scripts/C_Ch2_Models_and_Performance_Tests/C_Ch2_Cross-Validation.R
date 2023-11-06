### Chapter 2 10-fold Cross Validation ###
# authors: Lilian Hart and Curry Cunningham
# Date last edited: 06/22/23

# Set species
Spec <- "Chum"
spec <- tolower(Spec)

## Load packages
library(here)
library(tidyverse)
library(dplyr)
library(mgcv)
library(tweedie)

#### Settings ####
## Set local working directory (change for your machine)
setwd(here("data", "Chapter_2_RDS", "Chapter_2_CrossVal"))
dir.mod <- here("data", "Chapter_2_RDS")
dir.dat <- here::here("data", "BASIS")
## Load data
og <- readRDS(file=paste0(dir.dat, "/Ch2_dataframe.rds")) %>% drop_na()
species.dat <- subset(og, og$CommonName == paste(Spec, "Salmon"))
species.dat$Climate_stanza <- as.factor(species.dat$Climate_stanza)

# Save factor versions 
species.dat$fSampleYear <- as.factor(species.dat$SampleYear)
species.dat$Climate_stanza <- factor(species.dat$Climate_stanza)
species.dat$Even_odd_year <- factor(species.dat$Even_odd_year)

# Generate partitions in data
n_fold = 10
Partition_i = sample( 1:n_fold,
                      size=nrow(species.dat),
                      replace=TRUE )

#### Model 0 #### 
prednll_gam <- rep(NA, n_fold)

fI <- 1
for( fI in 1:n_fold ) {
  print(paste("##### Running fold", fI, "of", n_fold))
  
  PredTF_i = ifelse( Partition_i==fI, TRUE, FALSE )
  # Fit GAM Model =====================================
  # Partition Data
  temp.train <- species.dat[PredTF_i==FALSE,]
  temp.test <- species.dat[PredTF_i==TRUE,]
  
  ## Fit Models to Training Data Set ##
  temp.fit <- gam(TotalCatchNum ~ te(EQ.Longitude,EQ.Latitude, bs = "tp", 
                                     k = 9, m = 1) + s(fSampleYear, bs = "re") +
                    offset(log(Effort_area_km2)),
                  family = tw(link = "log"), method = "REML", data = temp.train)
  
  # Predict Testing Data Set
  pred.test <- predict(temp.fit, newdata=temp.test, type="response")
  
  # Calculate Negative LogLikelihood of Testing Observations, given Predictions
  #   under Tweedie
  
  # Extract Tweedie Dispersion
  temp.phi <- summary(temp.fit)$dispersion
  
  # Extract power term
  p_temp<- data.frame(p=c(temp.fit$family$family)) %>%
    separate(p, 10, into=c("delete", "p")) %>%
    separate(p, -1, into=c("p", "delete")) %>% 
    mutate( p = as.numeric(p))
  temp.p <- p_temp$p
  
  # Calculate negative logLikelihood of observations in the testing dataset,
  #   given the predicted values for the testing dataset, and fitted model
  #     power term (temp.p) and disperstion estimate (temp.phi)
  prednll_gam[fI] <- -1*sum(log(tweedie::dtweedie(y=temp.test$TotalCatchNum, power=temp.p, 
                                                  mu=pred.test, phi=temp.phi)))
  
}

# Check fit to all out-of-bag data and use as metric of out-of-bag performance
y <- sum( prednll_gam)
res1 <- c("Mod0:",y)
saveRDS(res1, paste0("nll_", spec, "_mod0_results"))

#### Model 1 Workflow ####
## Model 1A ##
prednll_gam <- rep(NA, n_fold)

fI <- 1
for( fI in 1:n_fold ) {
  print(paste("##### Running fold", fI, "of", n_fold))
  
  PredTF_i = ifelse( Partition_i==fI, TRUE, FALSE )
  # Fit GAM Model =====================================
  # Partition Data
  temp.train <- species.dat[PredTF_i==FALSE,]
  temp.test <- species.dat[PredTF_i==TRUE,]
  
  ## Fit Models to Training Data Set ##
  temp.fit <- gam(TotalCatchNum ~ te(EQ.Longitude,EQ.Latitude, bs = "tp", 
                                     k = 9, m = 1) + s(fSampleYear, bs = "re") +
                    offset(log(Effort_area_km2)) + 
                    te(surfacetemp, bs = "tp", k = 9, m = 1) + 
                    te(bottomsal, bs = "tp", k = 9, m = 1) +
                    te(MixedLayerDepth, bs = "tp", k = 9, m = 1),
                  family = tw(link = "log"), method = "REML", data = temp.train)
  
  # Predict Testing Data Set
  pred.test <- predict(temp.fit, newdata=temp.test, type="response")
  
  # Calculate Negative LogLikelihood of Testing Observations, given Predictions
  #   under Tweedie
  
  # Extract Tweedie Dispersion
  temp.phi <- summary(temp.fit)$dispersion
  
  # Extract power term
  p_temp<- data.frame(p=c(temp.fit$family$family)) %>%
    separate(p, 10, into=c("delete", "p")) %>%
    separate(p, -1, into=c("p", "delete")) %>% 
    mutate( p = as.numeric(p))
  temp.p <- p_temp$p
  
  # Calculate negative logLikelihood of observations in the testing dataset,
  #   given the predicted values for the testing dataset, and fitted model
  #     power term (temp.p) and disperstion estimate (temp.phi)
  prednll_gam[fI] <- -1*sum(log(tweedie::dtweedie(y=temp.test$TotalCatchNum, power=temp.p, 
                                                  mu=pred.test, phi=temp.phi)))

}

# Check fit to all out-of-bag data and use as metric of out-of-bag performance
y <- sum( prednll_gam)
res1 <- c("Mod1A:",y)
saveRDS(res1, paste0("nll_", spec, "_mod1A_results"))

#### Model 2 ####
## Model 2A ##
prednll_gam <- rep(NA, n_fold)

fI <- 1
for( fI in 1:n_fold ) {
  print(paste("##### Running fold", fI, "of", n_fold))
  
  PredTF_i = ifelse( Partition_i==fI, TRUE, FALSE )
  # Fit GAM Model =====================================
  # Partition Data
  temp.train <- species.dat[PredTF_i==FALSE,]
  temp.test <- species.dat[PredTF_i==TRUE,]
  
  ## Fit Models to Training Data Set ##
  temp.fit <- gam(TotalCatchNum ~ te(EQ.Longitude,EQ.Latitude, bs = "tp", 
                                     k = 9, m = 1) + s(fSampleYear, bs = "re") +
                    offset(log(Effort_area_km2)) + 
                    te(surfacetemp, bs = "tp", k = 9, m = 1) + 
                    te(bottomsal, bs = "tp", k = 9, m = 1) +
                    te(MixedLayerDepth, bs = "tp", k = 9, m = 1) +
                    te(CPE, bs = "tp", k = 9, m = 1),
                  family = tw(link = "log"), method = "REML", data = temp.train)
  
  
  # Predict Testing Data Set
  pred.test <- predict(temp.fit, newdata=temp.test, type="response")
  
  # Calculate Negative LogLikelihood of Testing Observations, given Predictions
  #   under Tweedie
  
  # Extract Tweedie Dispersion
  temp.phi <- summary(temp.fit)$dispersion
  
  # Extract power term
  p_temp<- data.frame(p=c(temp.fit$family$family)) %>%
    separate(p, 10, into=c("delete", "p")) %>%
    separate(p, -1, into=c("p", "delete")) %>% 
    mutate( p = as.numeric(p))
  temp.p <- p_temp$p
  
  # Calculate negative logLikelihood of observations in the testing dataset,
  #   given the predicted values for the testing dataset, and fitted model
  #     power term (temp.p) and disperstion estimate (temp.phi)
  prednll_gam[fI] <- -1*sum(log(tweedie::dtweedie(y=temp.test$TotalCatchNum, power=temp.p, 
                                                  mu=pred.test, phi=temp.phi)))
  
}

# Check fit to all out-of-bag data and use as metric of out-of-bag performance
y <- sum( prednll_gam)
res1 <- c("Mod2A:",y)
saveRDS(res1, paste0("nll_", spec, "_mod2A_results"))

## Model 2B ##
prednll_gam <- rep(NA, n_fold)

fI <- 1
for( fI in 1:n_fold ) {
  print(paste("##### Running fold", fI, "of", n_fold))
  
  PredTF_i = ifelse( Partition_i==fI, TRUE, FALSE )
  # Fit GAM Model =====================================
  # Partition Data
  temp.train <- species.dat[PredTF_i==FALSE,]
  temp.test <- species.dat[PredTF_i==TRUE,]
  
  ## Fit Models to Training Data Set ##
  temp.fit <- gam(TotalCatchNum ~ te(EQ.Longitude,EQ.Latitude, bs = "tp", 
                                              k = 9, m = 1) + s(fSampleYear, bs = "re") +
                             offset(log(Effort_area_km2)) + 
                             te(surfacetemp, bs = "tp", k = 9, m = 1) + 
                             te(bottomsal, bs = "tp", k = 9, m = 1) +
                             te(MixedLayerDepth, bs = "tp", k = 9, m = 1) +
                             te(CPE, bs = "tp", k = 9, m = 1) +
                             te(EQ.Longitude,EQ.Latitude, bs = "tp", k = 9, m = 1, by = CPE),
                           family = tw(link = "log"), method = "REML", data = temp.train)
  
  # Predict Testing Data Set
  pred.test <- predict(temp.fit, newdata=temp.test, type="response")
  
  # Calculate Negative LogLikelihood of Testing Observations, given Predictions
  #   under Tweedie
  
  # Extract Tweedie Dispersion
  temp.phi <- summary(temp.fit)$dispersion
  
  # Extract power term
  p_temp<- data.frame(p=c(temp.fit$family$family)) %>%
    separate(p, 10, into=c("delete", "p")) %>%
    separate(p, -1, into=c("p", "delete")) %>% 
    mutate( p = as.numeric(p))
  temp.p <- p_temp$p
  
  # Calculate negative logLikelihood of observations in the testing dataset,
  #   given the predicted values for the testing dataset, and fitted model
  #     power term (temp.p) and disperstion estimate (temp.phi)
  prednll_gam[fI] <- -1*sum(log(tweedie::dtweedie(y=temp.test$TotalCatchNum, power=temp.p, 
                                                  mu=pred.test, phi=temp.phi)))
  
}

# Check fit to all out-of-bag data and use as metric of out-of-bag performance
y <- sum( prednll_gam)
res2 <- c("Mod2B:",y)
saveRDS(res2, paste0("nll_", spec, "_mod2B_results"))

## Model 2C ##
prednll_gam <- rep(NA, n_fold)

fI <- 1
for( fI in 1:n_fold ) {
  print(paste("##### Running fold", fI, "of", n_fold))
  
  PredTF_i = ifelse( Partition_i==fI, TRUE, FALSE )
  # Fit GAM Model =====================================
  # Partition Data
  temp.train <- species.dat[PredTF_i==FALSE,]
  temp.test <- species.dat[PredTF_i==TRUE,]
  
  ## Fit Models to Training Data Set ##
  temp.fit <- gam(TotalCatchNum ~ te(EQ.Longitude,EQ.Latitude, bs = "tp", 
                                     k = 9, m = 1) + s(fSampleYear, bs = "re") +
                    offset(log(Effort_area_km2)) + 
                    te(CPE, bs = "tp", k = 9, m = 1) +
                    te(EQ.Longitude,EQ.Latitude, bs = "tp", k = 9, m = 1, by = CPE),
                  family = tw(link = "log"), method = "REML", data = temp.train)
  
  # Predict Testing Data Set
  pred.test <- predict(temp.fit, newdata=temp.test, type="response")
  
  # Calculate Negative LogLikelihood of Testing Observations, given Predictions
  #   under Tweedie
  
  # Extract Tweedie Dispersion
  temp.phi <- summary(temp.fit)$dispersion
  
  # Extract power term
  p_temp<- data.frame(p=c(temp.fit$family$family)) %>%
    separate(p, 10, into=c("delete", "p")) %>%
    separate(p, -1, into=c("p", "delete")) %>% 
    mutate( p = as.numeric(p))
  temp.p <- p_temp$p
  
  # Calculate negative logLikelihood of observations in the testing dataset,
  #   given the predicted values for the testing dataset, and fitted model
  #     power term (temp.p) and disperstion estimate (temp.phi)
  prednll_gam[fI] <- -1*sum(log(tweedie::dtweedie(y=temp.test$TotalCatchNum, power=temp.p, 
                                                  mu=pred.test, phi=temp.phi)))
  
}

# Check fit to all out-of-bag data and use as metric of out-of-bag performance
y <- sum( prednll_gam)
res2 <- c("Mod2C:",y)
saveRDS(res2, paste0("nll_", spec, "_mod2C_results"))

#### Model 3 ####
## Model 3A ##
prednll_gam <- rep(NA, n_fold)

fI <- 1
for( fI in 1:n_fold ) {
  print(paste("##### Running fold", fI, "of", n_fold))
  
  PredTF_i = ifelse( Partition_i==fI, TRUE, FALSE )
  # Fit GAM Model =====================================
  # Partition Data
  temp.train <- species.dat[PredTF_i==FALSE,]
  temp.test <- species.dat[PredTF_i==TRUE,]
  
  ## Fit Models to Training Data Set ##
  temp.fit <- gam(TotalCatchNum ~ te(EQ.Longitude,EQ.Latitude, bs = "tp", 
                                     k = 9, m = 1) + s(fSampleYear, bs = "re") +
                    offset(log(Effort_area_km2)) + 
                    te(surfacetemp, bs = "tp", k = 9, m = 1) + 
                    te(bottomsal, bs = "tp", k = 9, m = 1) +
                    te(MixedLayerDepth, bs = "tp", k = 9, m = 1) +
                    Climate_stanza,
                  family = tw(link = "log"), method = "REML", data = temp.train)
  
  # Predict Testing Data Set
  pred.test <- predict(temp.fit, newdata=temp.test, type="response")
  
  # Calculate Negative LogLikelihood of Testing Observations, given Predictions
  #   under Tweedie
  
  # Extract Tweedie Dispersion
  temp.phi <- summary(temp.fit)$dispersion
  
  # Extract power term
  p_temp<- data.frame(p=c(temp.fit$family$family)) %>%
    separate(p, 10, into=c("delete", "p")) %>%
    separate(p, -1, into=c("p", "delete")) %>% 
    mutate( p = as.numeric(p))
  temp.p <- p_temp$p
  
  # Calculate negative logLikelihood of observations in the testing dataset,
  #   given the predicted values for the testing dataset, and fitted model
  #     power term (temp.p) and disperstion estimate (temp.phi)
  prednll_gam[fI] <- -1*sum(log(tweedie::dtweedie(y=temp.test$TotalCatchNum, power=temp.p, 
                                                  mu=pred.test, phi=temp.phi)))
  
}

# Check fit to all out-of-bag data and use as metric of out-of-bag performance
y <- sum( prednll_gam)
res3 <- c("Mod3A:",y)
saveRDS(res3, paste0("nll_", spec, "_mod3A_results"))

## Model 3B ##
prednll_gam <- rep(NA, n_fold)

fI <- 1
for( fI in 1:n_fold ) {
  print(paste("##### Running fold", fI, "of", n_fold))
  
  PredTF_i = ifelse( Partition_i==fI, TRUE, FALSE )
  # Fit GAM Model =====================================
  # Partition Data
  temp.train <- species.dat[PredTF_i==FALSE,]
  temp.test <- species.dat[PredTF_i==TRUE,]
  
  ## Fit Models to Training Data Set ##
  temp.fit <- gam(TotalCatchNum ~ te(EQ.Longitude,EQ.Latitude, bs = "tp", 
                                     k = 9, m = 1) + s(fSampleYear, bs = "re") +
                    offset(log(Effort_area_km2)) + 
                    te(surfacetemp, bs = "tp", k = 9, m = 1) + 
                    te(bottomsal, bs = "tp", k = 9, m = 1) +
                    te(MixedLayerDepth, bs = "tp", k = 9, m = 1) +
                    Climate_stanza +
                    te(EQ.Longitude,EQ.Latitude, bs = "tp", k = 9, m = 1, 
                       by = Climate_stanza),
                  family = tw(link = "log"), method = "REML", data = temp.train)
  
  # Predict Testing Data Set
  pred.test <- predict(temp.fit, newdata=temp.test, type="response")
  
  # Calculate Negative LogLikelihood of Testing Observations, given Predictions
  #   under Tweedie
  
  # Extract Tweedie Dispersion
  temp.phi <- summary(temp.fit)$dispersion
  
  # Extract power term
  p_temp<- data.frame(p=c(temp.fit$family$family)) %>%
    separate(p, 10, into=c("delete", "p")) %>%
    separate(p, -1, into=c("p", "delete")) %>% 
    mutate( p = as.numeric(p))
  temp.p <- p_temp$p
  
  # Calculate negative logLikelihood of observations in the testing dataset,
  #   given the predicted values for the testing dataset, and fitted model
  #     power term (temp.p) and disperstion estimate (temp.phi)
  prednll_gam[fI] <- -1*sum(log(tweedie::dtweedie(y=temp.test$TotalCatchNum, power=temp.p, 
                                                  mu=pred.test, phi=temp.phi)))
  
}

# Check fit to all out-of-bag data and use as metric of out-of-bag performance
y <- sum( prednll_gam)
res3 <- c("Mod3B:",y)
saveRDS(res3, paste0("nll_", spec, "_mod3B_results"))

## Model 3C ##
prednll_gam <- rep(NA, n_fold)

fI <- 1
for( fI in 1:n_fold ) {
  print(paste("##### Running fold", fI, "of", n_fold))
  
  PredTF_i = ifelse( Partition_i==fI, TRUE, FALSE )
  # Fit GAM Model =====================================
  # Partition Data
  temp.train <- species.dat[PredTF_i==FALSE,]
  temp.test <- species.dat[PredTF_i==TRUE,]
  
  ## Fit Models to Training Data Set ##
  temp.fit <- gam(TotalCatchNum ~ te(EQ.Longitude,EQ.Latitude, bs = "tp", 
                                     k = 9, m = 1) + s(fSampleYear, bs = "re") +
                    offset(log(Effort_area_km2)) + 
                    Climate_stanza +
                    te(EQ.Longitude,EQ.Latitude, bs = "tp", k = 9, m = 1, 
                       by = Climate_stanza),
                  family = tw(link = "log"), method = "REML", data = temp.train)
  
  # Predict Testing Data Set
  pred.test <- predict(temp.fit, newdata=temp.test, type="response")
  
  # Calculate Negative LogLikelihood of Testing Observations, given Predictions
  #   under Tweedie
  
  # Extract Tweedie Dispersion
  temp.phi <- summary(temp.fit)$dispersion
  
  # Extract power term
  p_temp<- data.frame(p=c(temp.fit$family$family)) %>%
    separate(p, 10, into=c("delete", "p")) %>%
    separate(p, -1, into=c("p", "delete")) %>% 
    mutate( p = as.numeric(p))
  temp.p <- p_temp$p
  
  # Calculate negative logLikelihood of observations in the testing dataset,
  #   given the predicted values for the testing dataset, and fitted model
  #     power term (temp.p) and disperstion estimate (temp.phi)
  prednll_gam[fI] <- -1*sum(log(tweedie::dtweedie(y=temp.test$TotalCatchNum, power=temp.p, 
                                                  mu=pred.test, phi=temp.phi)))
  
}

# Check fit to all out-of-bag data and use as metric of out-of-bag performance
y <- sum( prednll_gam)
res3 <- c("Mod3C:",y)
saveRDS(res3, paste0("nll_", spec, "_mod3C_results"))

#### Model 4 ####
## Model 4A ##
prednll_gam <- rep(NA, n_fold)

fI <- 1
for( fI in 1:n_fold ) {
  print(paste("##### Running fold", fI, "of", n_fold))
  
  PredTF_i = ifelse( Partition_i==fI, TRUE, FALSE )
  # Fit GAM Model =====================================
  # Partition Data
  temp.train <- species.dat[PredTF_i==FALSE,]
  temp.test <- species.dat[PredTF_i==TRUE,]
  
  ## Fit Models to Training Data Set ##
  temp.fit <- gam(TotalCatchNum ~ te(EQ.Longitude,EQ.Latitude, bs = "tp", 
                                     k = 9, m = 1) + s(fSampleYear, bs = "re") +
                    offset(log(Effort_area_km2)) + 
                    te(surfacetemp, bs = "tp", k = 9, m = 1) + 
                    te(bottomsal, bs = "tp", k = 9, m = 1) +
                    te(MixedLayerDepth, bs = "tp", k = 9, m = 1) +
                    Even_odd_year,
                  family = tw(link = "log"), method = "REML", data = temp.train)
  
  # Predict Testing Data Set
  pred.test <- predict(temp.fit, newdata=temp.test, type="response")
  
  # Calculate Negative LogLikelihood of Testing Observations, given Predictions
  #   under Tweedie
  
  # Extract Tweedie Dispersion
  temp.phi <- summary(temp.fit)$dispersion
  
  # Extract power term
  p_temp<- data.frame(p=c(temp.fit$family$family)) %>%
    separate(p, 10, into=c("delete", "p")) %>%
    separate(p, -1, into=c("p", "delete")) %>% 
    mutate( p = as.numeric(p))
  temp.p <- p_temp$p
  
  # Calculate negative logLikelihood of observations in the testing dataset,
  #   given the predicted values for the testing dataset, and fitted model
  #     power term (temp.p) and disperstion estimate (temp.phi)
  prednll_gam[fI] <- -1*sum(log(tweedie::dtweedie(y=temp.test$TotalCatchNum, power=temp.p, 
                                                  mu=pred.test, phi=temp.phi)))
  
}

# Check fit to all out-of-bag data and use as metric of out-of-bag performance
y <- sum( prednll_gam)
res4 <- c("Mod4A:",y)
saveRDS(res4, paste0("nll_", spec, "_mod4A_results"))

## Model 4B ##
prednll_gam <- rep(NA, n_fold)

fI <- 1
for( fI in 1:n_fold ) {
  print(paste("##### Running fold", fI, "of", n_fold))
  
  PredTF_i = ifelse( Partition_i==fI, TRUE, FALSE )
  # Fit GAM Model =====================================
  # Partition Data
  temp.train <- species.dat[PredTF_i==FALSE,]
  temp.test <- species.dat[PredTF_i==TRUE,]
  
  ## Fit Models to Training Data Set ##
  temp.fit <- gam(TotalCatchNum ~ te(EQ.Longitude,EQ.Latitude, bs = "tp", 
                                     k = 9, m = 1) + s(fSampleYear, bs = "re") +
                    offset(log(Effort_area_km2)) + 
                    te(surfacetemp, bs = "tp", k = 9, m = 1) + 
                    te(bottomsal, bs = "tp", k = 9, m = 1) +
                    te(MixedLayerDepth, bs = "tp", k = 9, m = 1) +
                    Even_odd_year +
                    te(EQ.Longitude,EQ.Latitude, bs = "tp", k = 9, m = 1, 
                       by = Even_odd_year),
                  family = tw(link = "log"), method = "REML", data = temp.train)
  
  # Predict Testing Data Set
  pred.test <- predict(temp.fit, newdata=temp.test, type="response")
  
  # Calculate Negative LogLikelihood of Testing Observations, given Predictions
  #   under Tweedie
  
  # Extract Tweedie Dispersion
  temp.phi <- summary(temp.fit)$dispersion
  
  # Extract power term
  p_temp<- data.frame(p=c(temp.fit$family$family)) %>%
    separate(p, 10, into=c("delete", "p")) %>%
    separate(p, -1, into=c("p", "delete")) %>% 
    mutate( p = as.numeric(p))
  temp.p <- p_temp$p
  
  # Calculate negative logLikelihood of observations in the testing dataset,
  #   given the predicted values for the testing dataset, and fitted model
  #     power term (temp.p) and disperstion estimate (temp.phi)
  prednll_gam[fI] <- -1*sum(log(tweedie::dtweedie(y=temp.test$TotalCatchNum, power=temp.p, 
                                                  mu=pred.test, phi=temp.phi)))
  
}

# Check fit to all out-of-bag data and use as metric of out-of-bag performance
y <- sum( prednll_gam)
res4 <- c("Mod4B:",y)
saveRDS(res4, paste0("nll_", spec, "_mod4B_results"))

## Model 4C ##
prednll_gam <- rep(NA, n_fold)

fI <- 1
for( fI in 1:n_fold ) {
  print(paste("##### Running fold", fI, "of", n_fold))
  
  PredTF_i = ifelse( Partition_i==fI, TRUE, FALSE )
  # Fit GAM Model =====================================
  # Partition Data
  temp.train <- species.dat[PredTF_i==FALSE,]
  temp.test <- species.dat[PredTF_i==TRUE,]
  
  ## Fit Models to Training Data Set ##
  temp.fit <- gam(TotalCatchNum ~ te(EQ.Longitude,EQ.Latitude, bs = "tp", 
                                     k = 9, m = 1) + s(fSampleYear, bs = "re") +
                    offset(log(Effort_area_km2)) + 
                    Even_odd_year +
                    te(EQ.Longitude,EQ.Latitude, bs = "tp", k = 9, m = 1, 
                       by = Even_odd_year),
                  family = tw(link = "log"), method = "REML", data = temp.train)
  
  # Predict Testing Data Set
  pred.test <- predict(temp.fit, newdata=temp.test, type="response")
  
  # Calculate Negative LogLikelihood of Testing Observations, given Predictions
  #   under Tweedie
  
  # Extract Tweedie Dispersion
  temp.phi <- summary(temp.fit)$dispersion
  
  # Extract power term
  p_temp<- data.frame(p=c(temp.fit$family$family)) %>%
    separate(p, 10, into=c("delete", "p")) %>%
    separate(p, -1, into=c("p", "delete")) %>% 
    mutate( p = as.numeric(p))
  temp.p <- p_temp$p
  
  # Calculate negative logLikelihood of observations in the testing dataset,
  #   given the predicted values for the testing dataset, and fitted model
  #     power term (temp.p) and disperstion estimate (temp.phi)
  prednll_gam[fI] <- -1*sum(log(tweedie::dtweedie(y=temp.test$TotalCatchNum, power=temp.p, 
                                                  mu=pred.test, phi=temp.phi)))
  
}

# Check fit to all out-of-bag data and use as metric of out-of-bag performance
y <- sum( prednll_gam)
res4 <- c("Mod4C:",y)
saveRDS(res4, paste0("nll_", spec, "_mod4C_results"))


#### Load results ####
dir.res <- here("data", "Chapter_2_RDS", "Chapter_2_CrossVal")
mod0 <- readRDS(paste0(dir.res,"/nll_", spec, "_mod0_results"))
mod1A <- readRDS(paste0(dir.res, "/nll_", spec, "_mod1A_results"))
mod2A <- readRDS(paste0(dir.res, "/nll_", spec, "_mod2A_results"))
mod2B <- readRDS(paste0(dir.res, "/nll_", spec, "_mod2B_results"))
mod2C <- readRDS(paste0(dir.res, "/nll_", spec, "_mod2C_results"))
mod3A <- readRDS(paste0(dir.res, "/nll_", spec, "_mod3A_results"))
mod3B <- readRDS(paste0(dir.res, "/nll_", spec, "_mod3B_results"))
mod3C <- readRDS(paste0(dir.res, "/nll_", spec, "_mod3C_results"))
mod4A <- readRDS(paste0(dir.res, "/nll_", spec, "_mod4A_results"))
mod4B <- readRDS(paste0(dir.res, "/nll_", spec, "_mod4B_results"))
mod4C <- readRDS(paste0(dir.res, "/nll_", spec, "_mod4C_results"))
