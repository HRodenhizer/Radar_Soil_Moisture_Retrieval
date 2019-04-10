##############################################################################
###     2017 Radar Flight CiPEHR/DryPEHR Soil Moisture Compiling Code      ###
###                       Code by HGR 6/2018                               ###
##############################################################################

# Load packages
library(tidyverse)
library(sf)

# Load data
weather <- read.csv("C:/Users/hrodenhi/Documents/Internship/Soil_Moisture/Hobo_Weather/Hobo Daily_2016-10-01_to_2017-09-30.csv")
soil.moisture.cipehr <- read.csv("C:/Users/hrodenhi/Documents/Internship/Soil_Moisture/Soil_Sensors/CiPEHR soil sensor_2016-10-01_to_2017-09-30_daily.csv")
soil.moisture.drypehr <- read.csv("C:/Users/hrodenhi/Documents/Internship/Soil_Moisture/Soil_Sensors/DryPEHR soil sensor_2016-10-01_to_2017-09-30_daily.csv")
WTD.cipehr <- read.csv("C:/Users/hrodenhi/Documents/Internship/Soil_Moisture/WTD/CiPEHR_WTD_2017.csv")
WTD.drypehr <- read.csv("C:/Users/hrodenhi/Documents/Internship/Soil_Moisture/WTD/DryPEHR_WTD_2017.csv")
plots <- st_read(dsn = "C:/Users/hrodenhi/Documents/Internship/GPS/All_Points/All_Points_2017_SPCSAK4.shp")

# Select needed data and combine cipehr and drypehr into one file for each datastream
soil.moisture.cipehr.sub <- soil.moisture.cipehr %>%
  select(-SW, - WW) %>%
  filter(as.Date(date) == '2017-06-17' | as.Date(date) == '2017-09-19') %>%
  mutate(plot = as.character(plot))

soil.moisture.sub <- soil.moisture.drypehr %>%
  select(-warm, - dry) %>%
  filter(as.Date(date) == '2017-06-17' | as.Date(date) == '2017-09-19') %>%
  mutate(plot = as.character(plot)) %>%
  rbind.data.frame(soil.moisture.cipehr.sub) %>%
  mutate(index = ifelse(plot == '1' | plot == '4',
                        1,
                        ifelse(plot == '3' | plot == '2',
                               2,
                               ifelse(plot == '5' | plot == '8',
                                      3,
                                      ifelse(plot == '6' | plot == '7' | plot == 'C',
                                             4,
                                             ifelse(fence == 1 & plot == '2.5' | fence == 2 & plot == '2.5' | fence == 4 & plot == '2.5',
                                                    5,
                                                    ifelse(fence == 1 & plot == '4.5' | fence == 2 & plot == '4.5',
                                                           6,
                                                           ifelse(fence == 3 & plot == '2.5' | fence == 6 & plot == '2.5',
                                                                  7,
                                                                  ifelse(fence == 3 & plot == '4.5' | fence == 6 & plot == '4.5',
                                                                         8,
                                                                         ifelse(fence == 4 & plot == '4.5',
                                                                                9,
                                                                                ifelse(fence == 5 & plot == '2.5',
                                                                                       10,
                                                                                       ifelse(fence == 5 & plot == '4.5',
                                                                                              11,
                                                                                              ifelse(plot == 'B',
                                                                                                     12,
                                                                                                     13)))))))))))),
         date = as.character(date))

WTD.cipehr.sub <- WTD.cipehr %>%
  select(-Block, -WW) %>%
  filter(as.Date(Date, format = '%Y/%m/%d') == '2017/06/17' | as.Date(Date, format = '%Y/%m/%d') == '2017/09/19') %>%
  mutate(Well = as.character(Well))

WTD.sub <- WTD.drypehr %>%
  select(-Block, -WW, -Dry) %>%
  filter(as.Date(Date, format = '%Y/%m/%d') == '2017/06/17' | as.Date(Date, format = '%Y/%m/%d') == '2017/09/19') %>%
  mutate(Well = ifelse(Well == 'b' | Well == 'd',
                       str_c(Well, str_sub(Well.Location, start = 1, end = 1), sep = "_"),
                       as.character(Well)),
         Date = as.character(Date)) %>%
  select(-Well.Location) %>%
  rbind.data.frame(WTD.cipehr.sub) %>%
  mutate(index = ifelse(Well == '1',
                        1,
                        ifelse(Well == '2',
                               2,
                               ifelse(Well == '3',
                                      3,
                                      ifelse(Well == '4',
                                             4,
                                             ifelse(Fence == 1 & Well == '2.5' | Fence == 2 & Well == '2.5' | Fence == 4 & Well == '2.5',
                                                    5,
                                                    ifelse(Fence == 1 & Well == '4.5' | Fence == 2 & Well == '4.5',
                                                           6,
                                                           ifelse(Fence == 3 & Well == '2.5' | Fence == 6 & Well == '2.5',
                                                                  7,
                                                                  ifelse(Fence == 3 & Well == '4.5' | Fence == 6 & Well == '4.5',
                                                                         8,
                                                                         ifelse(Fence == 4 & Well == '4.5',
                                                                                9,
                                                                                ifelse(Fence == 5 & Well == '2.5',
                                                                                       10,
                                                                                       ifelse(Fence == 5 & Well == '4.5',
                                                                                              11,
                                                                                              ifelse(Well == 'b_N' | Well == 'b_S',
                                                                                                     12,
                                                                                                     ifelse(Well == 'd_N' | Well == 'd_S',
                                                                                                            13,
                                                                                                            NA))))))))))))),
         date = str_replace_all(Date, "/", "-")) %>%
  select(-Date) %>%
  rename(DOY = Doy, fence = Fence) %>%
  group_by(date, DOY, fence, index) %>%
  summarise(WTD = mean(WTD, na.rm = TRUE))

# join WTD to soil moisture
moisture <- soil.moisture.sub %>%
  left_join(WTD.sub, by = c('date', 'DOY', 'fence','index')) %>%
  arrange(DOY, fence, plot) %>%
  select(-index)

moisture.sub <- moisture %>%
  filter(Depth == 'T_five') %>%
  select(-Depth, -N)

# format points for join with moisture file
flux.plots <- plots %>%
  filter(Type == 'flux') %>%
  select(Name) %>%
  mutate(fence = as.integer(str_sub(Name, start = 1, end = 1)),
         plot = toupper(str_sub(Name, start = 3, end = 3))) %>%
  select(-Name)

# join moisture data to locations
moisture.for.radar <- flux.plots %>%
  merge(moisture.sub, by = c('fence', 'plot'))

# write shapefile of soil moisture and points
st_write(moisture.for.radar, "C:/Users/hrodenhi/Documents/Internship/Remote Sensing/L-Band/Moisture_Data_Field/moisture_for_radar.shp")
st_write(flux.plots, "C:/Users/hrodenhi/Documents/Internship/Project_prerun/plot_locations.shp")
