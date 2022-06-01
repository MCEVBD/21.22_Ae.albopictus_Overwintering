#' ---
#' title: " 13 combine glm Snow and tmeperature analysis"
#' author: "Katie Susong"
#' date: "01 June 2022"
#' ---

#' Overview
#' ========
#' 
#' The 21/22 and 18/19 data will be merged and subset to compair the predictive ablity
#' 
#' To do this the 21/22 data needs a few format adjustment to match 18/19 data. 
#' The format changes are lsited below:
#'    1. Factor of SNOWDAS created  
#'    2. remane MeanT_Air to MeanT_Ambient
#'    3. rename location to site (NOTE: in 18/19 study 3 tires where at 3 sites per location
#'    where as in 21/22 all tires were located at a sigle site at each location. As such "site" 
#'    from 18/19 is best reflected by "location" in 21/22)
#'    4. Date changed to date

# Libraries #
library(lubridate)
library(dplyr)
library(lme4)
library(ggplot2)
library(sjPlot) #for plotting lmer and glmer mods


#### Import data ####
#18/19 data
DIA <- read.csv("~/Documents/CBS_PhD/albopictus_OW/Data/SnowSite_07012021_kms.csv")
# format correction
DIA$date <- ymd(DIA$date) # correct data format
DIA$Location <- factor(DIA$Location , 
                       levels=c("New Berlin", "NWMAD","SCCMAD" ,"Kankakee", "Champaign-Urbana")) #fix location order to STD 
DIA$site <- as.factor(DIA$site)
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
# rename Date 
data <- rename(data, date = "Date")

#### Merge DFs ####

data.sub <- subset(data, select = c("date", 
                                    "MeanT_Ambient",
                                    "MeanT_Tire",
                                    "site",
                                    "snow_FAC10"))
dia.sub <- subset(DIA, select = c("date", 
                                   "MeanT_Ambient",
                                   "MeanT_Tire",
                                   "site",
                                   "snow_FAC10"))
both <- rbind(data.sub, dia.sub)

#### Subset larger df ####
# Split Data into Training and Testing in R 
sample_size = floor(0.7*nrow(both))
set.seed(3495)

# randomly split data in r
picked = sample(seq_len(nrow(both)),size = sample_size)
development =both[picked,]
holdout =both[-picked,]


#### mixed effect regression ####
siteR <- lmer(MeanT_Tire ~ MeanT_Ambient + snow_FAC10 + (MeanT_Ambient*snow_FAC10) + (1|site), data = development)
summary(siteR)
tab_model(siteR)

#### predict from holdout df ####

holdout$predict <- predict(siteR, holdout, type = "response",allow.new.levels = T)

#### Plot Predict ####

hist(holdout$MeanT_Tire)
hist(holdout$predict)

ggplot(holdout, aes( MeanT_Tire, predict,col = MeanT_Ambient))+
  geom_point()

ggplot(holdout, aes(MeanT_Ambient, ((MeanT_Tire)-(predict))))+
  geom_point()



data <- rename(data, site = "location")