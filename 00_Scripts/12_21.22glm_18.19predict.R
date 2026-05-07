#' ---
#' title: " 12 21/22 glm predict"
#' author: "Katie Susong"
#' date: "01 June 2022"
#' ---

#' Overview
#' ========
#' 
#' The 21/22 glm will be used to predict the 18/19 mean Tire T and compared to the actual values
#' 
#' To do this the 21/22 data needs a few format adjustment to match 18/19 data. 
#' The format changes are lsited below:
#'    1. Factor of SNOWDAS created  
#'    2. remane MeanT_Air to MeanT_Ambient
#'    3. rename location to site (NOTE: in 18/19 study 3 tires where at 3 sites per location
#'    where as in 21/22 all tires were located at a sigle site at each location. As such "site" 
#'    from 18/19 is best reflected by "location" in 21/22)

# Libraries #
library(lubridate)
library(dplyr)
library(lme4)
library(ggplot2)

#### Import data ####
#18/19 data
DIA <- read.csv("~/Documents/CBS_PhD/albopictus_OW/Data/SnowSite_07012021_kms.csv")
# format correction
DIA$date <- ymd(DIA$date) # correct data format
DIA$Location <- factor(DIA$Location , 
                       levels=c("New Berlin", "NWMAD","SCCMAD" ,"Kankakee", "Champaign-Urbana")) #fix location order to STD 

# 21/22 data
data <- read.csv('00_Data/21.22_snow_temperature.csv')
# format corrections
data$number   <- as.factor (data$number)
data$Date     <- ymd (data$Date)
data$location <- factor (data$location, levels = c("Car", "Bon", "Dek", "Arl", "Han", "Spo"))
data          <- filter (data, Date < "2022-04-14" )


#### Change data(21/22) df format and colnames to match DIA(18/19) df ####

# add catagorical snow values @ 100mm bins
data$snow_FAC10 <- cut(data$snodas, breaks = c(-1, 0,100,200,300,400), labels = c("none", ">100", ">200", ">300", ">400"))
# rename MeanT_Air
data <- rename(data, MeanT_Ambient = "MeanT_Air")
# rename location
data <- rename(data, site = "location")

#### 21/22 selected mixed effect regression ####

siteR <- lmer(MeanT_Tire ~ MeanT_Ambient + snow_FAC10 + (MeanT_Ambient*snow_FAC10) + (1|site), data = data)

#### predict 18/19 mean daily tire temperature ####

DIA$predict <- predict(siteR, DIA, type = "response",allow.new.levels = T)

#### Plot Predict ####

hist(DIA$MeanT_Tire)
hist(DIA$predict)

ggplot(DIA, aes( MeanT_Tire, predict,col = MeanT_Ambient))+
  geom_point()

ggplot(DIA, aes(date, predict)) +
  geom_line(col = "red" ) + 
  geom_line(aes(date, MeanT_Tire)) +
  facet_wrap(~site)

ggplot(DIA, aes(MeanT_Ambient, ((MeanT_Tire)-(predict))))+
  geom_point()







