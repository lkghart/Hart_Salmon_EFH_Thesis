### Coho Chapter 2 GAM Effects plots
## Author: Lilian Hart 
## Last edited: 08/21/23

require(tidyverse)
require(dplyr)
require(here)
require(viridis)
require(mgcv)
require(visreg)
require(ggplot2)
require(gratia)
require(sf)
require(rnaturalearthdata)
require(rnaturalearth)
require(rnaturalearthhires)
require(mmtable2)

Spec <- "Chum"
spec <- "chum"

#### Setup ####
dir.mod <- here("data", "Chapter_2_RDS")
setwd(dir.mod)
dir.fig <- file.path("~/Documents/Salmon_EFH_Deliverables/Chapter2_Figures")

## Load in mapping attributes
usa <- ne_states("United States of America", returnclass = "sf")
ak <- subset(usa, name == "Alaska")
ak <- st_set_crs(ak, 4326)
russia <-rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")%>%
  filter(name=="Russia")
russia <- st_set_crs(russia, 4326)

#### Load Models ####
# In Situ Model
mod1A <- readRDS(paste0(spec, "_gam_mod1A.rds"))

# Cold Pool Extent Models
mod2A <- readRDS(paste0(spec, "_gam_mod2A.rds"))
mod2B <- readRDS(paste0(spec, "_gam_mod2B.rds"))
mod2C <- readRDS(paste0(spec, "_gam_mod2C.rds"))

# Climate Stanza Models
mod3A <- readRDS(paste0(spec, "_gam_mod3A.rds"))
mod3B <- readRDS(paste0(spec, "_gam_mod3B.rds"))
mod3C <- readRDS(paste0(spec, "_gam_mod3C.rds"))

# Even/Odd Year Models
mod4A <- readRDS(paste0(spec, "_gam_mod4A.rds"))
mod4B <- readRDS(paste0(spec, "_gam_mod4B.rds"))
mod4C <- readRDS(paste0(spec, "_gam_mod4C.rds"))

# Gather model names
mlc <- c("mod1A", "mod2A", "mod2B", "mod2C", "mod3A", "mod3B", "mod3C",
         "mod4A", "mod4B", "mod4C")

#### Make Plots ####
# Set working directory to figures folder
setwd(dir.fig)

## Collect smooth terms through gratia ##
## Model 1A ##
mod <- mod1A
mod_char <- mlc[1]
terms <- list("te(surfacetemp)", "te(bottomsal)",
                 "te(MixedLayerDepth)")
titles <- list("SST", "bottom salinity", "MLD")
for(i in 1:3){
  # Pull smooth estimates
  target <- terms[i]
  sm <- smooth_estimates(mod, smooth = as.character(target))
  sm_df <- data.frame(sm)
  # Create new names without parentheses for saving in filename
  newname <- gsub("[()]", "", x = target)
  # Pull column name for x-axis
  cnames <- colnames(sm_df)
  enviro <- cnames[6]
  # Plot
  ggplot(sm_df, aes(sm_df[,6]) ) +
    geom_ribbon(aes(ymin = est - (1.96*se), ymax = est + (1.96*se)),
                fill = "grey70") +
    geom_line(aes(y = est)) +
    ggtitle(paste("Effect of", titles[i], "on \n",
            "juvenile", spec, "salmon abundance")) +
    labs(x = titles[i], y = "Effect")
  ggsave(paste0(spec, "_", mod_char, "_effects", "_", newname, ".jpg"), 
         width = 5, height = 4, units = "in")
} 

## Model 2A ##
mod <- mod2A
mod_char <- "mod2A"
# Define smooths to plot
terms <- list("te(surfacetemp)", "te(bottomsal)",
                 "te(MixedLayerDepth)", "te(CPE)")
titles <- list("SST", "bottom salinity", "MLD", "CPE")
for(i in 1:4){
  # Pull smooth estimates
  target <- terms[i]
  sm <- smooth_estimates(mod, smooth = as.character(target))
  sm_df <- data.frame(sm)
  # Create new names without parentheses for saving in filename
  newname <- gsub("[()]", "", x = target)
  # Pull column name for x-axis
  cnames <- colnames(sm_df)
  enviro <- cnames[6]
  # Get min and max x-axis values
  xmin <- min(sm_df[,6])
  xmax <- max(sm_df[,6])
  # Plot
  ggplot(sm_df, aes(sm_df[,6]) ) +
    geom_ribbon(aes(ymin = est - (1.96*se), ymax = est + (1.96*se)),
                fill = "grey70") +
    geom_line(aes(y = est)) +
    ggtitle(paste("Effect of", titles[i], "on \n",
                  "juvenile", spec, "salmon abundance")) +
    labs(x = titles[i], y = "Effect")
  ggsave(paste0(spec, "_", mod_char, "_effects", "_", newname, ".jpg"), 
         width = 5, height = 4, units = "in")
} 

## Model 2B ##
mod <- mod2B
mod_char <- "mod2B"
# Define smooths to plot
terms <- list("te(surfacetemp)", "te(bottomsal)",
                 "te(MixedLayerDepth)", "te(CPE)")
for(i in 1:4){
  # Pull smooth estimates
  target <- terms[i]
  sm <- smooth_estimates(mod, smooth = as.character(target))
  sm_df <- data.frame(sm)
  # Create new names without parentheses for saving in filename
  newname <- gsub("[()]", "", x = target)
  # Pull column name for x-axis
  cnames <- colnames(sm_df)
  enviro <- cnames[6]
  # Get min and max x-axis values
  xmin <- min(sm_df[,6])
  xmax <- max(sm_df[,6])
  # Plot
  ggplot(sm_df, aes(sm_df[,6]) ) +
    geom_ribbon(aes(ymin = est - (1.96*se), ymax = est + (1.96*se)),
                fill = "grey70") +
    geom_line(aes(y = est)) +
    ggtitle(paste("Effect of", titles[i], "on \n",
                  "juvenile", spec, "salmon abundance")) +
    labs(x = titles[i], y = "Effect")
  ggsave(paste0(spec, "_", mod_char, "_effects", "_", newname, ".jpg"), 
         width = 5, height = 4, units = "in")
} 

# gratia::draw() effects plots
draw(mod2B)

## Model 2C ##
mod <- mod2C
mod_char <- "mod2C"
# Define smooths to plot
terms <- "te(CPE)"
for(i in 1:4){
  # Pull smooth estimates
  sm <- smooth_estimates(mod, smooth = terms)
  sm_df <- data.frame(sm)
  # Create new names without parentheses for saving in filename
  newname <- gsub("[()]", "", x = terms)
  # Get min and max x-axis values
  xmin <- min(sm_df[,6])
  xmax <- max(sm_df[,6])
  # Plot
  ggplot(sm_df, aes(sm_df[,6]) ) +
    geom_ribbon(aes(ymin = est - (1.96*se), ymax = est + (1.96*se)),
                fill = "grey70") +
    geom_line(aes(y = est)) +
    ggtitle(paste("Effect of", terms[i], "on \n",
                  "juvenile", spec, "salmon abundance")) +
    labs(x = titles[i], y = "Effect")
  ggsave(paste0(spec, "_", mod_char, "_effects", "_", newname, ".jpg"), 
         width = 5, height = 4, units = "in")
} 
# gratia::draw() effects plots
draw(mod2C)
ggsave(paste0(spec, "_", mod_char, "_draw_effects.jpg"),
       width = 8, height = 6, units = "in")

## Model 3A ##
mod <- mod3A
mod_char <- "mod3A"
# Define smooths to plot
terms <- list("te(surfacetemp)", "te(bottomsal)",
              "te(MixedLayerDepth)")
for(i in 1:3){
  # Pull smooth estimates
  target <- terms[i]
  sm <- smooth_estimates(mod, smooth = as.character(target))
  sm_df <- data.frame(sm)
  # Create new names without parentheses for saving in filename
  newname <- gsub("[()]", "", x = target)
  # Pull column name for x-axis
  cnames <- colnames(sm_df)
  enviro <- cnames[6]
  # Get min and max x-axis values
  xmin <- min(sm_df[,6])
  xmax <- max(sm_df[,6])
  # Plot
  ggplot(sm_df, aes(sm_df[,6]) ) +
    geom_ribbon(aes(ymin = est - (1.96*se), ymax = est + (1.96*se)),
                fill = "grey70") +
    geom_line(aes(y = est)) +
    ggtitle(paste("Effect of", terms[i], "on \n",
                  "juvenile", spec, "salmon abundance")) +
    labs(x = titles[i], y = "Effect")
  ggsave(paste0(spec, "_", mod_char, "_effects", "_", newname, ".jpg"), 
         width = 5, height = 4, units = "in")
} 

# Get parametric effects
param_effects <- parametric_effects(mod3A) %>% as.data.frame()
#a <- mmtable(data = param_effects, table_name = "Climate Stanza Model, 3A: Parametric Effects")
# Error in mutate() caused by error in vctrs_vec_compat()

## Model 3B ##
mod <- mod3B
mod_char <- "mod3B"
# Define smooths to plot
terms <- list("te(surfacetemp)", "te(bottomsal)",
              "te(MixedLayerDepth)")
for(i in 1:3){
  # Pull smooth estimates
  target <- terms[i]
  sm <- smooth_estimates(mod, smooth = as.character(target))
  sm_df <- data.frame(sm)
  # Create new names without parentheses for saving in filename
  newname <- gsub("[()]", "", x = target)
  # Pull column name for x-axis
  cnames <- colnames(sm_df)
  enviro <- cnames[6]
  # Get min and max x-axis values
  xmin <- min(sm_df[,6])
  xmax <- max(sm_df[,6])
  # Plot
  ggplot(sm_df, aes(sm_df[,6]) ) +
    geom_ribbon(aes(ymin = est - (1.96*se), ymax = est + (1.96*se)),
                fill = "grey70") +
    geom_line(aes(y = est)) +
    ggtitle(paste("Effect of", terms[i], "on \n",
                  "juvenile", spec, "salmon abundance")) +
    labs(x = titles[i], y = "Effect")
  ggsave(paste0(spec, "_", mod_char, "_effects", "_", newname, ".jpg"), 
         width = 5, height = 4, units = "in")
} 

# gratia::draw() effects plots
draw(mod3B)
ggsave(paste0(spec, "_", mod_char, "_draw_effects.jpg"),
       width = 8, height = 6, units = "in")

# Get parametric effects
param_effects <- parametric_effects(mod3B) %>% as.data.frame()

## Model 3C ##
mod <- mod3C
mod_char <- "mod3C"

# gratia::draw() effects plots
draw(mod3C)
ggsave(paste0(spec, "_", mod_char, "_draw_effects.jpg"),
       width = 8, height = 6, units = "in")

## Model 4A ##
mod <- mod3A
mod_char <- "mod4A"
# Define smooths to plot
terms <- list("te(surfacetemp)", "te(bottomsal)",
              "te(MixedLayerDepth)")
for(i in 1:3){
  # Pull smooth estimates
  target <- terms[i]
  sm <- smooth_estimates(mod, smooth = as.character(target))
  sm_df <- data.frame(sm)
  # Create new names without parentheses for saving in filename
  newname <- gsub("[()]", "", x = target)
  # Pull column name for x-axis
  cnames <- colnames(sm_df)
  enviro <- cnames[6]
  # Get min and max x-axis values
  xmin <- min(sm_df[,6])
  xmax <- max(sm_df[,6])
  # Plot
  ggplot(sm_df, aes(sm_df[,6]) ) +
    geom_ribbon(aes(ymin = est - (1.96*se), ymax = est + (1.96*se)),
                fill = "grey70") +
    geom_line(aes(y = est)) +
    ggtitle(paste("Effect of", terms[i], "on \n",
                  "juvenile", spec, "salmon abundance")) +
    labs(x = titles[i], y = "Effect")
  ggsave(paste0(spec, "_", mod_char, "_effects", "_", newname, ".jpg"), 
         width = 5, height = 4, units = "in")
} 

# Get parametric effects
param_effects <- parametric_effects(mod4A) %>% as.data.frame()
#a <- mmtable(data = param_effects, table_name = "Climate Stanza Model, 3A: Parametric Effects")
# Error in mutate() caused by error in vctrs_vec_compat()

## Model 4B ##
mod <- mod4B
mod_char <- "mod4B"
# Define smooths to plot
terms <- list("te(surfacetemp)", "te(bottomsal)",
              "te(MixedLayerDepth)")
for(i in 1:3){
  # Pull smooth estimates
  target <- terms[i]
  sm <- smooth_estimates(mod, smooth = as.character(target))
  sm_df <- data.frame(sm)
  # Create new names without parentheses for saving in filename
  newname <- gsub("[()]", "", x = target)
  # Pull column name for x-axis
  cnames <- colnames(sm_df)
  enviro <- cnames[6]
  # Get min and max x-axis values
  xmin <- min(sm_df[,6])
  xmax <- max(sm_df[,6])
  # Plot
  ggplot(sm_df, aes(sm_df[,6]) ) +
    geom_ribbon(aes(ymin = est - (1.96*se), ymax = est + (1.96*se)),
                fill = "grey70") +
    geom_line(aes(y = est)) +
    ggtitle(paste("Effect of", terms[i], "on \n",
                  "juvenile", spec, "salmon abundance")) +
    labs(x = enviro, y = "Effect")
  ggsave(paste0(spec, "_", mod_char, "_effects", "_", newname, ".jpg"), 
         width = 5, height = 4, units = "in")
} 

# gratia::draw() effects plots
draw(mod4B)
ggsave(paste0(spec, "_", mod_char, "_draw_effects.jpg"),
       width = 8, height = 6, units = "in")

# Get parametric effects
param_effects <- parametric_effects(mod4B) %>% as.data.frame()

## Model 4C ##
mod <- mod4C
mod_char <- "mod4C"

# gratia::draw() effects plots
draw(mod4C)
ggsave(paste0(spec, "_", mod_char, "_draw_effects.jpg"),
       width = 8, height = 6, units = "in")

#### Get parametric effects values ####
(parametric_effects(mod3A))
(parametric_effects(mod3B))
(parametric_effects(mod3C))
(parametric_effects(mod4A))
(parametric_effects(mod4B))
(parametric_effects(mod4C))

#### Closer look at bottom salinity ####
# Set working directory to figures folder
setwd(dir.fig)

## Collect smooth terms through gratia ##
## Model 1A ##
mod <- mod1A
target <- "te(bottomsal)"
sm <- smooth_estimates(mod, smooth = as.character(target))
sm_df <- data.frame(sm)
# Create new names without parentheses for saving in filename
newname <- gsub("[()]", "", x = target)
# Assign x axis title
enviro <- "Bottom salinity (PSU)"
# Set min and max x-axis values based on histogram
xmin <- 30
xmax <- 33
# Plot
ggplot(sm_df, aes(sm_df[,6]) ) +
  geom_ribbon(aes(ymin = est - (1.96*se), ymax = est + (1.96*se)),
              fill = "grey70") +
  geom_line(aes(y = est)) +
  ggtitle(paste("Effect of bottom salinity on", "\n",
                "juvenile", spec, "salmon abundance")) +
  labs(x = enviro, y = "Effect") +
  xlim(xmin, xmax)
ggsave(paste0(spec, "_", "zoomed_effects", "_", newname, ".jpg"), 
       width = 5, height = 4, units = "in")
