#' ---
#' title: Tire v Soil temp
#' author: Katie M Susong
#' date: 25-Oct-22
#' ---
#' 

#' Overview
#' ========
#'
#' At each site the soil temperature was recorded as well as the temperature with in tire. Consequently the two values
#' (soil which represents the traditional subnivium and our tire sites) can be compated 
#¢ 
#¢ Timeline 
#' =======
#' 
#' **25-Oct-22**:  imported and formated hourly data -> Inital plots created.
#'                  daily data + snow imported -> initial plot. **KMS** 
#'


#libraries

library(ggplot2)
library(lubridate)
library(dplyr)
library(cowplot)

#### Import ####

## hourly data ( no snow )
data <- read.csv('00_Data/21.22_temperature.csv')
#format corrections
data$number   <- as.factor (data$number)
data$Date     <- ymd_hms (data$Date)
data$location <- factor (data$location, levels = c("Car", "Bon", "Dek", "Arl", "Han", "Spo"))
# filters
data          <- filter (data, Date < "2022-04-14" ) # removal all recordings after field removal
data          <- filter (data, Soil != "NA") # limit to the tire at each site with an assciated soil measurment. 

## Daily data with Snow
dia <- read.csv('00_Data/21.22_snow_temperature.csv')
#format corrections
dia$number   <- as.factor (dia$number)
dia$Date     <- ymd (dia$Date)
dia$location <- factor (dia$location, levels = c("Car", "Bon", "Dek", "Arl", "Han", "Spo"))
dia          <- filter (dia, Date < "2022-04-14" )


#### Inital Plot ####

#'### Hourly data
#'
ggplot(data) +
  geom_line(aes(Date, Air_Temp), col = "black") +
  geom_line(aes(Date, Tire), col = "blue") +
  geom_line(aes(Date, Soil), col = "green") +
  facet_wrap(~ number) # numbers run N (1) to S (13)
  
ggplot(data) +
  geom_line(aes(Date, Air_Temp), col = "black") +
  geom_line(aes(Date, Tire), col = "blue") +
  geom_line(aes(Date, Soil), col = "green") +
  facet_wrap(~ location)

#' Looking at these two plots it is clear that the soil temperature is more stable ( campared to air and tire temp) 
#' regardless of season (aka temperature and snowcover). Note that at Spo (number 1-2) the winter soil temperature 
#' is the most stable (lickely due to stable subnivium development). Sites with warmer air temperature but no 
#' snow cover (location: Car, Bon, Dek; number: greater than 7) had colder soil temperature than Spo. It is possible
#' that there is an offset for tire+snow temp compared to the sol temperature (which is recorded at weather
#' (monitoring sites) but the trend is not obvioulsy any clearer than that oof air temperature

#'### Daily data
#'

ggplot(dia) +
  geom_line(aes(Date, MeanT_Air), col = "black") +
  geom_line(aes(Date, MeanT_Tire), col = "Red") +
  geom_line(aes(Date, MeanT_Soil), col = "green") +
  facet_wrap(~number)

ggplot(dia, aes(cover, (MeanT_Soil - MeanT_Tire))) +
  geom_boxplot()

{a <- ggplot(dia, aes(snodas, (MeanT_Soil - MeanT_Tire))) +
  geom_point()
b <- ggplot(dia, aes(depth_G, (MeanT_Soil - MeanT_Tire))) +
  geom_point()
c <- ggplot(dia, aes(depth_T, (MeanT_Soil - MeanT_Tire))) +
  geom_point()

plot_grid(a, b,c)}

{a <- ggplot(dia, aes(MeanT_Soil, MeanT_Tire, col = snodas )) +
  geom_point()
b <- ggplot(dia, aes(MeanT_Soil, MeanT_Tire, col = depth_G )) +
  geom_point()
c <- ggplot(dia, aes(MeanT_Soil, MeanT_Tire, col = depth_T )) +
  geom_point()

plot_grid(a, b,c)}


  #' Daily average data confirmes the findings of the hourly data, namely that sol temperature is more stable than 
  #' tire temperatures. There may be a slght tredn that increasing snow cover (measured on the ground and possiblely
  #' on the tire) decreases the difference between soil and tire temp. This is not seem when looking at snowdas 
  #' measures. 

