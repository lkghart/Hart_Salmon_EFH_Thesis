#### Predicting! ####

# Create model
mod1 <- glm(log(Weight.CPUE..kg.km2. +1) ~ fYear, data = catch)

# Create dataframe to hold predictions
n_years <- length(years)
cpue_pop <- array(dim=c(n_years, 6), 
                  dimnames=list(years, 
                                c("Year", "lnCPUE", "SE", "CV", "low95","up95")))

# Loop through years and predict 
y <- 1
for(y in 1:n_years) {
  # Predict lnCPUE
  temp.pred <-  predict(mod1, 
                        newdata = data.frame(fYear=factor(years[y])), se=TRUE)
  
  # Populate our output array
  cpue_pop1[y,1] <- years[y] # Year
  cpue_pop1[y,2] <- temp.pred$fit # lnCPUE
  cpue_pop1[y,3] <- temp.pred$se # Standard Error
  cpue_pop1[y,4] <- temp.pred$se/temp.pred$fit # CV
  cpue_pop1[y,5] <- temp.pred$fit - 1.96*temp.pred$se # Approximate lower bound of 95% CI
  cpue_pop1[y,6] <- temp.pred$fit + 1.96*temp.pred$se # Approximate upper bound of 95% CI
}
# Finally, convert to dataframe
cpue_pop1 <- data.frame(cpue_pop)