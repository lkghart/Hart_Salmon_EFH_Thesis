#### Tidyverse functions! ####

# group_by() and summarise()
# Count stations per strata in each year
catch %>% group_by(Year, Stratum) %>% summarise(n = n(), na.rm=T) 

# sort() 
sort(unique(catch$Year), decreasing = FALSE)

# filter()
# filter out distances less than 1.25 m
duck <- duck %>% filter(distance <= 1.25)

# drop_na()
# drop all rows with NA in all columns, or just some columns when specified
dat <- dat %>% drop_na(CommonName, Effort_area_km2, TotalCatchNum)