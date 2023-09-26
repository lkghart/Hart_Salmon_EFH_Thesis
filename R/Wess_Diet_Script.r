###################
#  FIGURE 8: DIET PLOT
###################
library(tidyverse)
library(viridis)
library(here)

dirData <- here("scripts")

dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dirData)

#I converted to annual proportion before reading in
diet <- read.csv("SockDietProp.csv")

#Changed prey item names to appear the way I wanted them to, instead of _ or . separators. 
diet.name <- diet %>% rename("Age-0 Pollock" = Age0pollock, "Arrow Worms" = Arrowworm, "Calanus spp." = Calanus, 
                             "Large Copepods" = Largecopepods, "Other Crustaceans" = Othercrustaceans, "Small Copepods" = Smallcopepods)

#Long pivot to make ggplot happy. 
diet.long <- pivot_longer(diet.name, cols = 2:12, names_to = "PreyItem", values_to = "SCI")

#Quick easy way to remove the gaps on the plot for years with no data.
diet.long$Year <- as.factor(diet.long$Year)

#Reorder prey items to the order that I want them to appear in. I grouped fish with fish, crustaceans with crustaceans, 
#descending order within each group. 

p1 <- diet.long %>%
  mutate(PreyItem = fct_relevel(PreyItem,
                               "Age-0 Pollock", "Fish", "Euphausiids", 
                               "Amphipods", "Calanus spp.", "Large Copepods", "Small Copepods", 
                               "Other Crustaceans", "Arrow Worms", "Pteropods", "Other")) %>%
  ggplot(aes(Year,PreyItem,fill=SCI))+
  geom_tile(color= "white",size=0.1) + 
  scale_fill_viridis(name="%SCI",option ="C")+
  theme_minimal(base_size = 8)+
  ylab("Prey Item")+
  xlab("")
 
#Clean up plot and export using res that Ellen had previously set, though I changed the aspect ratio to better fit this style of 
#plot. 
tiff(file="FIGURE 8.tiff", height=8, width=11, units='in', pointsize=16,compression="lzw", res=1200) 
  p1+theme(axis.text.y=element_text(size=12),axis.text.x=element_text(size=12),axis.title.y = element_text(size=14),legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),strip.background = element_rect(colour="white"),axis.ticks=element_blank()) 
  dev.off()