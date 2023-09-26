#### Maps of predicted densities and CV, separated  ####
dir.fig <- file.path("~/Documents/Salmon_EFH_Deliverables/Chapter1_Figures/V4_Figs")
setwd(dir.fig)
# Load in mapping attributes
usa <- ne_states("United States of America", returnclass = "sf")
ak <- subset(usa, name == "Alaska")
ak <- st_set_crs(ak, 4326)
russia <-rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")%>%
  filter(name=="Russia")
russia <- st_set_crs(russia, 4326)

# Save predictions to sf objects
pred1_df <- data.frame(Fit = pred_1$Fit, Lon = pred_1$Lon, Lat = pred_1$Lat, 
                       CV = pred_1$CV)
pred1_sf <- st_as_sf(pred1_df, coords = c("Lon", "Lat"), crs = 4326)

pred2_df <- data.frame(Fit = pred_2$Fit, Lon = pred_2$Lon, Lat = pred_2$Lat, 
                       CV = pred_2$CV, Year = pred_2$Year)
pred2_sf <- st_as_sf(pred2_df, coords = c("Lon", "Lat"), crs = 4326)

# pred3_df <- data.frame(Fit = pred_3$Fit, Lon = pred_3$Lon, Lat = pred_3$Lat, 
#                        CV = pred_3$CV, Year = pred_3$Year)
# pred3_sf <- st_as_sf(pred3_df, coords = c("Lon", "Lat"), crs = 4326)

pred4_df <- data.frame(Fit = pred_4$Fit, Lon = pred_4$Lon, Lat = pred_4$Lat, 
                       CV = pred_4$CV, Year = pred_4$Year)
pred4_sf <- st_as_sf(pred4_df, coords = c("Lon", "Lat"), crs = 4326)


## Model 1 Maps
# CRS 3336 is NAD83 Alaska Albers Projection
#limits <- c(-166, 124)
#cvlims <- c()

ggplot() +
  geom_sf(data = pred1_sf, mapping = aes(color = Fit)) +
  geom_sf(data = ak) +
  geom_sf(data = russia) +
  coord_sf(crs = st_crs(3338), xlim = c(-2280000, 180000),
           ylim = c(50000, 2800000), expand = FALSE,
           datum = st_crs(4326))+
  scale_x_continuous(breaks = c(seq(-180 , -125, by = 7)))+
  scale_color_viridis(name = "Count") +
  labs(x = "Longitude", y = "Latitude",
       title = paste("Juvenile", Spec, "- Avg. Spatial Pattern"))
#ggsave(paste0(Spec,"_GAM_Mod1_Response.jpeg"), width = 5, height = 5, units = "in")


#Model 1 uncertainty
ggplot() +
  geom_sf(data = pred1_sf, mapping = aes(color = CV)) +
  geom_sf(data = ak) +
  geom_sf(data = russia) +
  coord_sf(crs = st_crs(3338), xlim = c(-2280000, 180000), 
           ylim = c(50000, 2800000), expand = FALSE, 
           datum = st_crs(4326))+
  scale_x_continuous(breaks = c(seq(-180 , -125, by = 7)))+
  scale_color_viridis() +
  labs(x = "Longitude", y = "Latitude", 
       title = paste("Juvenile", Spec, "- Avg. Spatial Pattern\n Uncertainty"), 
       fill = "CV")
#ggsave(paste0(Spec,"_GAM_Mod1_Response_CV.jpeg"), width = 5, height = 5, units = "in")

## Model 2 (factor year effect)
ggplot() +
  geom_sf(data = pred2_sf, mapping = aes(color = Fit)) +
  geom_sf(data = ak) +
  geom_sf(data = russia) +
  coord_sf(crs = st_crs(3338), xlim = c(-2280000, 180000), 
           ylim = c(50000, 2800000), expand = FALSE, 
           datum = st_crs(4326))+
  facet_wrap(~Year, ncol = 4) +
  scale_x_continuous(breaks = c(seq(-180 , -125, by = 7)))+
  scale_color_viridis(name = "Count") +
  labs(x = "Longitude", y = "Latitude") +
  theme(axis.text.x = element_text(angle = 45, vjust=0.5))
ggsave(paste0(Spec,"_GAM_Mod2_Response.jpeg"), width = 8.5, height = 10,
       units = "in")


#Model 2 Uncertainty
ggplot() +
  geom_sf(data = pred2_sf, mapping = aes(color = CV)) +
  geom_sf(data = ak) +
  geom_sf(data = russia) +
  coord_sf(crs = st_crs(3338), xlim = c(-2280000, 180000), 
           ylim = c(50000, 2800000), expand = FALSE, 
           datum = st_crs(4326))+
  facet_wrap(~Year) +
  scale_x_continuous(breaks = c(seq(-180 , -125, by = 7)))+
  scale_color_viridis() +
  labs(x = "Longitude", y = "Latitude") +
  theme(axis.text.x = element_text(angle = 45, vjust=0.5))
ggsave(paste0(Spec,"_GAM_Mod2_Response_CV.jpeg"), width = 10, height = 11,
       units = "in")


# ## Model 3 (spatiotemporal, independent year effect)
# ggplot() +
#   geom_sf(data = pred3_sf, mapping = aes(color = Fit)) +
#   geom_sf(data = ak) +
#   geom_sf(data = russia) +
#   coord_sf(crs = st_crs(3338), xlim = c(-2280000, 180000), 
#            ylim = c(50000, 2800000), expand = FALSE, 
#            datum = st_crs(4326))+
#   facet_wrap(~Year, ncol = 4) +
#   scale_x_continuous(breaks = c(seq(-180 , -125, by = 7)))+
#   scale_color_viridis(name = "Count") +
#   labs(x = "Longitude", y = "Latitude") +
#   theme(axis.text.x = element_text(angle = 45, vjust=0.5))
# 
# ggsave(paste0(Spec,"_GAM_Mod3_Response.jpeg"), width = 10, height = 11,
#        units = "in")
# 
# 
# ggplot() +
#   geom_sf(data = pred3_sf, mapping = aes(color = CV)) +
#   geom_sf(data = ak) +
#   geom_sf(data = russia) +
#   coord_sf(crs = st_crs(3338), xlim = c(-2280000, 180000), 
#            ylim = c(50000, 2800000), expand = FALSE, 
#            datum = st_crs(4326))+
#   facet_wrap(~Year, ncol = 4) +
#   scale_x_continuous(breaks = c(seq(-180 , -125, by = 7)))+
#   scale_color_viridis() +
#   labs(x = "Longitude", y = "Latitude") +
#   theme(axis.text.x = element_text(angle = 45, vjust=0.5))
# 
# ggsave(paste0(Spec,"_GAM_Mod3_Response_CV.jpeg"), width = 10, height = 11,
#        units = "in")


## Model 4 (spatiotemporal, continuous year effect)
ggplot() +
  geom_sf(data = pred4_sf, mapping = aes(color = Fit)) +
  geom_sf(data = ak) +
  geom_sf(data = russia) +
  coord_sf(crs = st_crs(3338), xlim = c(-2280000, 180000), 
           ylim = c(50000, 2800000), expand = FALSE, 
           datum = st_crs(4326))+
  facet_wrap(~Year, ncol = 4) +
  scale_x_continuous(breaks = c(seq(-180 , -125, by = 7)))+
  scale_color_viridis(name = "Count") +
  labs(x = "Longitude", y = "Latitude") +
  theme(axis.text.x = element_text(angle = 45, vjust=0.5))

ggsave(paste0(Spec,"_GAM_Mod4_Response.jpeg"), width = 10, height = 11,
       units = "in")


#Model 4 Uncertainty
ggplot() +
  geom_sf(data = pred4_sf, mapping = aes(color = CV)) +
  geom_sf(data = ak) +
  geom_sf(data = russia) +
  coord_sf(crs = st_crs(3338), xlim = c(-2280000, 180000), 
           ylim = c(50000, 2800000), expand = FALSE, 
           datum = st_crs(4326))+
  facet_wrap(~Year) +
  scale_x_continuous(breaks = c(seq(-180 , -125, by = 7)))+
  scale_color_viridis() +
  labs(x = "Longitude", y = "Latitude") +
  theme(axis.text.x = element_text(angle = 45, vjust=0.5))

ggsave(paste0(Spec,"_GAM_Mod4_Response_CV.jpeg"), width = 10, height = 11,
       units = "in")


#### Maps of predicted densities and CV, combined ####
# Step 1: convert coordinate system of sf objects
a <- st_transform(pred1_sf, crs=3338)
b <- data.frame(st_coordinates(a))
# Step 2: convert sf object back to dataframe for alpha plotting
p1 <- data.frame(Longitude = b$X, 
                 Latitude = b$Y,
                 Fit = a$Fit,
                 CV = a$CV)
# Model 1
ggplot() +
  geom_point(data = p1, mapping = aes(x=Longitude, y=Latitude,
                                      color = Fit), alpha = 1-a$CV) +
  geom_sf(data = ak) +
  geom_sf(data = russia) +
  coord_sf(crs = st_crs(3338), xlim = c(-2280000, 180000), 
           ylim = c(50000, 2800000), expand = FALSE, 
           datum = st_crs(4326))+
  scale_x_continuous(breaks = c(seq(-180 , -125, by = 7)))+
  scale_color_viridis(name = "Count") +
  labs(x = "Longitude", y = "Latitude", 
       title = paste("Juvenile", Spec, "- Avg. Spatial Pattern\n Alpha set to 1-CV"))

ggsave(paste0(Spec,"_GAM_Mod1_Response_Combo.jpeg"), width = 5, height = 5, units = "in")

# Model 2
a <- st_transform(pred2_sf, crs=3338)
b <- data.frame(st_coordinates(a))
p2 <- data.frame(Longitude = b$X, 
                 Latitude = b$Y,
                 Fit = a$Fit,
                 CV = a$CV,
                 Year = a$Year)
ggplot() +
  geom_point(data = p2, mapping = aes(x=Longitude, y=Latitude,
                                      color = Fit), alpha = 1-a$CV) +
  geom_sf(data = ak) +
  geom_sf(data = russia) +
  coord_sf(crs = st_crs(3338), xlim = c(-2280000, 180000), 
           ylim = c(50000, 2800000), expand = FALSE, 
           datum = st_crs(4326))+
  facet_wrap(~Year, ncol = 4) +
  scale_x_continuous(breaks = c(seq(-180 , -125, by = 7)))+
  scale_color_viridis(name = "Count") +
  labs(x = "Longitude", y = "Latitude",
       title = paste("Juvenile", Spec, "- Model 2\n Alpha set to 1-CV"))+
  theme(axis.text.x = element_text(angle = 45, vjust=0.5))

ggsave(paste0(Spec,"_GAM_Mod2_Response_Combo.jpeg"), width = 8.5, height = 11,
       units = "in")


# # Model 3
# a <- st_transform(pred3_sf, crs=3338)
# b <- data.frame(st_coordinates(a))
# p3 <- data.frame(Longitude = b$X, 
#                  Latitude = b$Y,
#                  Fit = a$Fit,
#                  CV = a$CV,
#                  Year = a$Year)
# ggplot() +
#   geom_point(data = p3, mapping = aes(x=Longitude, y=Latitude,
#                                       color = Fit), alpha = 1-a$CV) +
#   geom_sf(data = ak) +
#   geom_sf(data = russia) +
#   coord_sf(crs = st_crs(3338), xlim = c(-2280000, 180000), 
#            ylim = c(50000, 2800000), expand = FALSE, 
#            datum = st_crs(4326))+
#   facet_wrap(~Year, ncol = 4) +
#   scale_x_continuous(breaks = c(seq(-180 , -125, by = 7)))+
#   scale_color_viridis(name = "Count") +
#   labs(x = "Longitude", y = "Latitude",
#        title = paste("Juvenile", spec, "- Model 3\n Alpha set to 1-CV")) +
#   theme(axis.text.x = element_text(angle = 45, vjust=0.5))
# 
# ggsave(paste0(Spec,"_GAM_Mod3_Response_Combo.jpeg"), width = 10, height = 11,
#        units = "in")
# 

# Model 4
a <- st_transform(pred4_sf, crs=3338)
b <- data.frame(st_coordinates(a))
p4 <- data.frame(Longitude = b$X, 
                 Latitude = b$Y,
                 Fit = a$Fit,
                 CV = a$CV,
                 Year = a$Year)
ggplot() +
  geom_point(data = p4, mapping = aes(x=Longitude, y=Latitude,
                                      color = Fit), alpha = 1-a$CV) +
  geom_sf(data = ak) +
  geom_sf(data = russia) +
  coord_sf(crs = st_crs(3338), xlim = c(-2280000, 180000), 
           ylim = c(50000, 2800000), expand = FALSE, 
           datum = st_crs(4326))+
  facet_wrap(~Year, ncol = 4) +
  scale_x_continuous(breaks = c(seq(-180 , -125, by = 7)))+
  scale_color_viridis(name = "Count") +
  labs(x = "Longitude", y = "Latitude",
       title = paste("Juvenile", Spec, "- Model 4\n Alpha set to 1-CV")) +
  theme(axis.text.x = element_text(angle = 45, vjust=0.5))

ggsave(paste0(Spec,"_GAM_Mod4_Response_Combo.jpeg"), width = 10, height = 11,
       units = "in")
