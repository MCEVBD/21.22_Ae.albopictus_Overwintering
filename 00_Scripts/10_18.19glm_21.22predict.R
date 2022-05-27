#' ---
#' title: " 10 18/19 glm predict"
#' author: "Katie Susong"
#' date: "27 May 2022"
#' ---

#' Overview
#' ========
#' 
#' The 18/19 glm will be used to predict the 21/22 mean Tire T and compared to the actual values
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

##### Run 18/19 model ####
mixed.model2 <- lmer(MeanT_Tire ~ MeanT_Ambient + relevel(snow_FAC10, ref = 5)+ (1|site), data = DIA)
