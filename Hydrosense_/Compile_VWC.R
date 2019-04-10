####################################################################################################################
###        This code compiles VWC ground measurements for CiPEHR/DryPEHR and ABoVE transects                     ###
###                              Code by HGR 4/2019                                                              ###
####################################################################################################################

### load libraries #################################################################################################
library(tidyverse)
library(sf)
####################################################################################################################

### load data ######################################################################################################
soil.moisture.cipehr <- read.csv("C:/Users/hrodenhi/Documents/Internship/Soil_Moisture/Soil_Sensors/CiPEHR soil sensor_2016-10-01_to_2017-09-30_daily.csv")
soil.moisture.drypehr <- read.csv("C:/Users/hrodenhi/Documents/Internship/Soil_Moisture/Soil_Sensors/DryPEHR soil sensor_2016-10-01_to_2017-09-30_daily.csv")
plots <- st_read("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/All_Points/All_Points_2017_SPCSAK4.shp")
abovemay17 <- read.csv('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/ABoVE/Soil_Moisture/2017_Final/061617_corrected.csv')
abovesept17 <- read.csv('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/ABoVE/Soil_Moisture/2017_Final/09192017_corrected.csv')
####################################################################################################################

