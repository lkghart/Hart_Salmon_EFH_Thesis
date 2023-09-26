#### GGPLOT magic ####

## Histogram with geom_bar() 
ggplot(data = wren, aes(x = distance)) +
  geom_bar() + ggtitle("Histogram of detection distances")
# Example: Show total number of wren detections by transect number
ggplot(data = wren, aes(x = Transect, y = object)) + geom_bar(stat = "identity") 

## Boxplots with geom_boxplot()
ggplot(data = wren, aes(y = distance)) + geom_boxplot(group) +
  ggtitle("Boxplots of detection distances by transect")

## 95% CIs with geom_ribbon() 
ggplot(cpue_pop1, aes(x=Year, y=lnCPUE)) + 
  geom_line() +
  geom_point(pch=21, fill="red") +
  geom_ribbon(aes(ymin=low95, ymax=up95), fill="red", alpha=0.2) +
  ggtitle("Model 1 and uncertainty")+
  ylim(c(-2,5))

## Show uncertainty around estimates with geom_linerange()
dens <- data.frame(means=c(118,96,107), upper=c(147,113,157),
                   lower=c(94,81,72), 
                   x=c("Half-normal","Hazard rate", "Uniform"))
ggplot() + geom_linerange(data=dens, mapping=aes(x=x, ymax=upper,
                                                 ymin=lower), width = 0.2) +
  geom_point(data = dens, mapping = aes(x=x, y=means), size=4, shape=21) +
  xlab("Distance model") + ylab("Estimate in individuals/km^2") +
  ggtitle("Density estimates by model")

## Plot by a factor level, and edit X-Axis tick marks and breaks.
ggplot(ultimate_meat, mapping = aes(x=Season, y=Predictions, color=Model_Type, 
                                    fill=Model_Type, group = Model_Type,
                                    ymin=(Predictions-(1.96*SE)), ymax=(Predictions+(1.96*SE)))) + 
  geom_line() +
  geom_ribbon(alpha = 0.3, color = NA)+
  scale_x_discrete(breaks = seq(2000, 2020, 5)) +
  xlab("Season") + ylab("CPUE") + ggtitle("Meat Model Comparison")

