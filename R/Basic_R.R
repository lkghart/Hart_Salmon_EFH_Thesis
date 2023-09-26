#### Basic R functions  ####

## Create dataframe to hold a given dimension of values ##
n_years <- length(years)
cpue <- array(dim=c(n_years, 6) dimnames=list(years, c("Year", "lnCPUE")))

## Rowbinding with rbind()
# Create data frames, then rbind to combine for better plotting
meat_noregion <- data.frame(Season = unique(scallop$Season), 
                            Predictions = model1_response, 
                            Model_Type = "Without Region",
                            SE = pred_mod1$se.fit)
meat_yesregion <- data.frame(Season = unique(scallop$Season), 
                             Predictions = model2_response, 
                             Model_Type = "With Region",
                             SE = pred_mod2$se.fit)
ultimate_meat <- rbind(meat_noregion, meat_yesregion)

