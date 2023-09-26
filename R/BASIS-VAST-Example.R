#==================================================================================================
#Project Name: BASIS SALMON - Example Implementation of VAST for Lilian Hart
#Creator: Curry James Cunningham, College of Fisheries and Ocean Sciences, UAF
#Date: 11.15.21
#
#Purpose: To create a general script for fitting a VAST model to BASIS surface trawl data. 
#
#
#
#==================================================================================================
#NOTES:
# Parameters are estimated by identifying the value of fixed effects that 
# maximizes the marginal likelihood when integrated across random effects.  
# We approximate this multidimensional integral using the Laplace approximation, 
# as implemented using Template Model Builder (Kristensen et al., 2016).
#==================================================================================================
#devtools::install_github("james-thorson/FishStatsUtils", INSTALL_opts="--no-staged-install")
#devtools::install_github("james-thorson/VAST", INSTALL_opts="--no-staged-install")

require(tidyverse)
require(dplyr)
require(ggthemes)
require(VAST)
require(here)
require(FishStatsUtils)
require(units)

# detach("package:rnaturalearth", unload=TRUE)

# CONTROL ======================================================================

# Select Species
species <- "Coho Salmon"

# Version <- "VAST_v12_0_0"

# Number of knots (i.e. spatial complexity)
n_x <- 250
Region <- "User"
fine_scale <- TRUE

# ObsModel=c(1,1) #Lognormal and Poisson-Linked Delta
ObsModel=c(2,1) #Gamma and Poisson-Linked Delta

treat_nonencounter_as_zero <- TRUE

# Workflow =====================================================================
dir.data <- here("data", "BASIS")

dir.vast <- file.path(here(), "VAST", species)
dir.create(dir.vast, recursive=TRUE)

dir.R <- here("R")

# Source FishStatsUtils function that cannot be loaded =========================
# source(file.path(dir.R, "strip_units.R")) #No longer needed


# Read in Compiled Basis Data ==================================================
full_data <- readRDS(file.path(dir.data,"full_basis_combo_data"))
species.dat <- subset(full_data, full_data$CommonName == species)

#str(species.dat)

#species.dat[is.na(species.dat$EQ.Longitude),]
#species.dat[is.na(species.dat$EQ.Latitude),]
#species.dat[is.na(species.dat$Effort_area_km2),]
#species.dat[is.na(species.dat$TotalCatchWt),]

# Filter out observations with missing values
temp.dat <- species.dat %>% filter(!is.na(EQ.Longitude),
                                  !is.na(EQ.Latitude),
                                  !is.na(Effort_area_km2),
                                  !is.na(TotalCatchWt))

# Plot Data ====================================================================
# Fit Original Model

setwd(dir.vast)

settings <- make_settings(n_x = n_x, 
                          # Region = file.path(dir.data, "Ecopath_shapefiles_v01", "EGOA.shp"),
                          Region=Region,
                          purpose = "index2", 
                          strata.limits = data.frame(STRATA="All_areas"),
                          fine_scale = fine_scale, 
                          bias.correct = TRUE,
                          ObsModel=ObsModel,
                          treat_nonencounter_as_zero=treat_nonencounter_as_zero,
                          FieldConfig = c(Omega1 = "IID", Epsilon1 = "IID",Omega2 = "IID", Epsilon2 = "IID"))#,
                          # Version=Version)

# coords <- cbind(temp.dat$EQ.Latitude, temp.dat$EQ.Longitude)
# dimnames(coords)[[2]] <- c("Lat","Lon")
# is.matrix(coords)

# Read input grid
user_region <- readRDS(here("VAST", "user_region_ALL.rds"))

# Run model
fit <- fit_model(settings = settings, 
                 Lat_i = temp.dat$EQ.Latitude, 
                 Lon_i = temp.dat$EQ.Longitude, 
                 t_i = temp.dat$SampleYear, 
                 c_i = rep(0,nrow(temp.dat)),
                 b_i = as_units(temp.dat$TotalCatchWt, 'kg'), 
                 a_i = as_units(temp.dat$Effort_area_km2, 'km^2'),
                 input_grid = user_region)

# fit <- fit_model( "settings" = settings, 
#                   "Lat_i" = temp.dat[,'EQ.Latitude'], 
#                   "Lon_i" = temp.dat[,'EQ.Longitude'], 
#                   "t_i" = temp.dat[,'SampleYear'], 
#                   "c_i" = rep(0,nrow(temp.dat)),
#                   "b_i" = as_units(temp.dat[,'TotalCatchWt'],'kg'), 
#                   "a_i" = temp.dat[,'Effort_area_km2'],
#                   "observations_LL" = coords)

typeof(temp.dat$TotalCatchWt)

# Plot Output ==================================================================
plot(fit)
# plot_results(fit)
plot_results(fit, plot_set=3)

# Reset working directory ======================================================
setwd(here())




