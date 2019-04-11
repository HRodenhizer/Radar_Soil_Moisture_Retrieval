####################################################################################################################
###        This code compiles VWC ground measurements for CiPEHR/DryPEHR and ABoVE transects                     ###
###                              Code by HGR 4/2019                                                              ###
####################################################################################################################

### load libraries #################################################################################################
library(sf)
library(mapview)
library(lubridate)
library(raster)
library(tidyverse)
####################################################################################################################

### load data ######################################################################################################
# soil.moisture.cipehr <- read.csv("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Soil Moisture/Soil_Sensors/CiPEHR soil sensor_2016-10-01_to_2017-09-30_daily.csv")
# soil.moisture.drypehr <- read.csv("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Soil Moisture/Soil_Sensors/DryPEHR soil sensor_2016-10-01_to_2017-09-30_daily.csv")
# plots <- st_read("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/All_Points/All_Points_2017_SPCSAK4.shp")
abovemay17 <- read.csv('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Soil Moisture/ABoVE_2017_Final/061617_corrected.csv')
abovesept17 <- read.csv('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Soil Moisture/ABoVE_2017_Final/09192017_corrected.csv')
maymodel <- raster('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Remote Sensing/Soil_Moisture_Model/Richard_run_using_Bircher/results_eps_170617.tif')
septmodel <- raster('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Remote Sensing/Soil_Moisture_Model/Richard_run_using_Bircher/results_eps_170919.tif')
####################################################################################################################

### clean above data for joining ###################################################################################
may.subset <- abovemay17 %>%
  dplyr::select(Site, Point, Depth, 8, Latitude, Longitude) %>%
  rename(eps = 4) %>% # dielectric constant
  mutate(Year = 2017,
         Month = 6,
         Day = 17)

sept.subset <- abovesept17 %>%
  dplyr::select(Site, Point, Depth, 8, Latitude, Longitude) %>%
  rename(eps = 4) %>% # dielectric constant
  mutate(Year = 2017,
         Month = 9,
         Day = 19)

# Joining and convert to sf
moisture.sf <- may.subset %>%
  rbind.data.frame(sept.subset) %>%
  filter(Site != is.na(Site)) %>%
  group_by(Site, Point, Year, Month, Day, Depth, Longitude, Latitude) %>%
  summarise(mean.eps = mean(eps, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Depth = ifelse(Depth == 6,
                        'eps.6cm',
                        ifelse(Depth == 12,
                               'eps.12cm',
                               'eps.20cm'))) %>%
  spread(key = Depth, value = mean.eps) %>%
  filter(Longitude != is.na(Longitude)) %>%
  group_by(Site, Point) %>%
  mutate(Latitude = first(Latitude),
         Longitude = first(Longitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  arrange(Year, Month, Day, Site, Point)

mapview(moisture.sf, zcol = "Month", legend = TRUE)
##################################################################################################################

# ### Filter and clean cipehr and drypehr data ###################################################################
# experiment.moisture <- soil.moisture.cipehr %>%
#   select(-WW, -SW) %>%
#   rbind.data.frame(soil.moisture.drypehr %>% select(-warm, -dry)) %>%
#   mutate(Site = ifelse(plot == 'B' | plot == 'C' | plot == 'D',
#                       'DryPEHR',
#                       'CiPEHR'),
#          Point = paste(fence, plot, sep = '.'),
#          Depth = 15,
#          Year = year,
#          Month = month(date),
#          Day = day(date),
#          Mean) %>%
#   filter
# 

### Extract dielectric constant from model output at ground locations ############################################
may.moisture.comparison <- maymodel %>%
  raster::extract(moisture.sf %>% filter(Month == 6), sp = TRUE) %>%
  st_as_sf() %>%
  rename(model.eps = 9)

sept.moisture.comparison <- septmodel %>%
  raster::extract(moisture.sf %>% filter(Month == 9), sp = TRUE) %>%
  st_as_sf() %>%
  rename(model.eps = 9)

moisture.comparison <- may.moisture.comparison %>%
  rbind.data.frame(sept.moisture.comparison) %>%
  gather(key = 'Depth', value = 'hydrosense.eps', eps.12cm:eps.6cm)

# write.table(moisture.comparison, 'C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Remote Sensing/Soil_Moisture_Model/Richard_run_using_Bircher/model_field_comparison.txt', row.names = FALSE, sep = '\t')

ggplot(moisture.comparison, aes(x = hydrosense.eps, y = model.eps)) +
  geom_point() +
  facet_grid(Month~Depth)

# ggsave('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Remote Sensing/Soil_Moisture_Model/Richard_run_using_Bircher/model_field_comparison.pdf')
