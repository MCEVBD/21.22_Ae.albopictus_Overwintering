#' ---
#' title: 09 Visulization of snow and temperature
#' author: Katie M Susong
#' date: 26-May-2022
#' ---
#' 

#' Overview
#' =========
#' 
#' impact of snow and cover on tire temperature 
#' 
#' 
#' Plots v time 
#' =================
#'    1. Depth on ground v time 
#'    2. Depth on tires v time
#'    3. depth on ground v times,  colored cover of tires (seperated and not seperated by location) 
#'    4. depth on tire v time,  colored cover of tires (seperated and not seperated by location) 
#'    5. Depth on ground and SNODAS v time 
#'    6. Air and tire temp v time 
#' CONCLUSIONS: SPO had more snow on the ground particularly in late winter/ early 
#' spring. The is a differnece between tire and air temperature at Spon, Han adn Arl 
#' there may also be some at Dek. Particularly during cold periods. At least 100mm of
#' snow are needed on the ground for both the side and top of tires to be covered. 
#' 
#' Comparative Plots
#' ================   
#'    1. Air and tire temperature (color by cover)
#'    2. Air and tire temperature (color scale by snow)
#'  CONCLUSIONS: strong trends Full cover of tires produces the greatest insulation
#'  
#'  NEXT STEP: use the 18/19 glm (susong, tucker et al 2022) to predict values for 21/22
#'  and compare
#'


#libraries

library(ggplot2)
library(lubridate)
library(dplyr)

#### Import ####

data <- read.csv('00_Data/21.22_snow_temperature.csv')
#format corrections
data$number   <- as.factor (data$number)
data$Date     <- ymd (data$Date)
data$location <- factor (data$location, levels = c("Car", "Bon", "Dek", "Arl", "Han", "Spo"))
data          <- filter (data, Date < "2022-04-14" )

#### Plots through time ####

#'
#' **Plot 1:** Depth on ground v time 
#' 

ggplot(data, aes(Date, depth_G, col = location))+
  geom_point() +
  geom_smooth(span = 0.3) +
  ylim(0,300)
#'
#' **Plot 2:** Depth on tires v time 
#'

ggplot(data, aes(Date, depth_T, col = location))+
  geom_point() +
  geom_smooth(span = 0.09) +
  ylim(0,300)

#'
#' ** Plot 3a ad 3b:** depth on ground v times,  colored cover of tires (seperated and not seperated by location) 
#'

ggplot(data, aes(Date, depth_G, col = cover))+
  geom_point() +
  facet_wrap(~location, nrow = 6) +
  ylim(0,300)

ggplot(data, aes(Date, depth_G, col = cover))+
  geom_point() +
  ylim(0,300)

#'
#' ** Plot 4a ad 4b:** depth on tire v time,  colored cover of tires (seperated and not seperated by location) 
#'

ggplot(data, aes(Date, depth_T, col = cover))+
  geom_point() +
  facet_wrap(~location, nrow = 6) +
  ylim(0,300)

ggplot(data, aes(Date, depth_T, col = cover))+
  geom_point() +
  ylim(0,300)
 
#'
#' **Plot 5:** Depth on ground and SNODAS through time 
#' 

ggplot(data, aes(Date, snodas))+
  geom_line(col = "black") +
  geom_line(aes(Date, depth_G), col = "red") +
  facet_wrap(~location,nrow = 6)

#'
#' **Plot 6:** Air and tire temp through time 
#' 

ggplot(data, aes(Date, MeanT_Tire))+
  geom_line(col = "black") +
  geom_line(aes(Date, MeanT_Air) ,col = "red")+
  facet_wrap(~location,nrow = 6)

data %>%
  filter(Date >= "2021-12-01", Date <= "2022-02-28") %>%
  ggplot( aes(Date, MeanT_Tire))+
  geom_line(col = "black") +
  geom_line(aes(Date, MeanT_Air) ,col = "red")+
  facet_wrap(~location,nrow = 6) +
  geom_hline(yintercept = -2.5,  linetype= "dashed")
#### Comparative Plots ####

#' **Plot 1:** Air and tire temperature (color by cover)
#' 

ggplot(data, aes(MeanT_Air, MeanT_Tire, col = cover))+
  geom_point() 

#' **Plot 2:** Air and tire temperature (color scale by snow)
#'    
ggplot(data, aes(MeanT_Air, MeanT_Tire, col = depth_G))+
  geom_point() +
  geom_smooth()

ggplot(data, aes(MeanT_Air, MeanT_Tire, col = depth_G))+
  geom_point() +
  facet_wrap(~location)
