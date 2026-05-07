---
  #' title: " 11 snow insulation (temperature) glm"
  #' author: "Katie Susong"
  #' date: "27 May 2022"
  #' ---
  #' 
  #' 
  #' Overview
  #' =======
  #' 
  #' Selection of regression for tire temperature prediction using the 2021/2022 data. 
  #' 
  #' selected model: snowFACplus <- glm(MeanT_Tire ~ MeanT_Ambient + snow_FAC10 + MeanT_Ambient:snow_FAC10, data=data) 
  #' 
  #' script includes: tables and forest plots 
  #' 
  #' **KMS**
  

rm (list  = ls())

# Libraries #
library(AICcmodavg)
library(lubridate)
library(dplyr)
library(lme4)
library(ggplot2)
library(sjPlot) #for plotting lmer and glmer mods

#### 21/22 data ####
setwd("~/Documents/CBS_PhD/Ae.albo_OW_2021/21.22_Ae.albopictus_Overwintering")
data <- read.csv('00_Data/21.22_snow_temperature.csv')
# format corrections
data$number   <- as.factor (data$number)
data$Date     <- ymd (data$Date)
data$location <- factor (data$location, levels = c("Car", "Bon", "Dek", "Arl", "Han", "Spo"))
data          <- filter (data, Date < "2022-04-14" ) # post collection dates

#site lat/long
lalo <- read.csv('00_Data/allsite_latlong.csv')

# Change data(21/22) df format and colnames to match DIA(18/19) df 
## b/c the first step is to repeat the 18/19 (Susong, Tucker et al. 2022) glm

# add catagorical snow values @ 100mm bins
data$snow_FAC10 <- cut(data$snodas, breaks = c(-1, 0,100,200,300,400), labels = c("none", ">100", ">200", ">300", ">400"))
# rename MeanT_Air
data <- rename(data, MeanT_Ambient = "MeanT_Air")
# rename location
data <- rename(data, site = "location")

data <- merge(data,lalo)
#### Distribution plot #### 
hist(data$MeanT_Tire)

#### Model Creation ####

# repeat 18/19 model (Susong, Tucker et al. 2022)
m18.19 <- lmer(MeanT_Tire ~ MeanT_Ambient + snow_FAC10 + (1|site), data = data)

#null model
null <- glm(MeanT_Tire ~ 1, data = data)

air <- glm(MeanT_Tire ~ MeanT_Ambient, data = data)
lat <- glm(MeanT_Tire ~ lat, data = data)

onlysnowFAC <- glm(MeanT_Tire ~ snow_FAC10, data=data)

# snow - catagorical (lv. = 5) models 
snowFAC <- glm(MeanT_Tire ~ MeanT_Ambient + snow_FAC10, data=data)
snowFaclat <- glm(MeanT_Tire ~ MeanT_Ambient + snow_FAC10 + lat, data= data)

# snowFACsiteR = m18.19
snowFACsiteRdate <- lmer(MeanT_Tire ~ MeanT_Ambient + snow_FAC10 + Date + (1|site), data = data)

snowFACplus <- glm(MeanT_Tire ~ MeanT_Ambient + snow_FAC10 + MeanT_Ambient:snow_FAC10, data=data) ## selected model ##
snowFACplusLaLo <- glm(MeanT_Tire ~ MeanT_Ambient + snow_FAC10 + MeanT_Ambient:snow_FAC10 + lat + lon, data=data) 
snowFACplussiteR <- lmer(MeanT_Tire ~ MeanT_Ambient + snow_FAC10 + (MeanT_Ambient*snow_FAC10) + (1|site), data = data)
snowFACplussiteRdate <- lmer(MeanT_Tire ~ MeanT_Ambient + snow_FAC10 + (MeanT_Ambient*snow_FAC10)+ Date + (1|site), data = data)




#### Summaries/Tables ####

summary(m18.19)
tab_model(m18.19)

summary(air)
summary(snowFAC)
summary(lat)


summary(snowFaclat)
summary(snowFACsiteRdate)

summary(snowFACplus)
summary(snowFACplussiteR)
summary(snowFACplussiteRdate)


#### Selection ####


# Building AIC table 

models_noR <- list(air,lat, null, onlysnowFAC,
                   snowFAC, snowFaclat, snowFACplus, snowFACplusLaLo )  # models with no random effect
models_R <- list(m18.19, snowFACplussiteR, snowFACplussiteRdate,
                 snowFACsiteRdate)       # models with random effects 
model.names_noR <- c("air","lat", "null", "onlysnowFAC", "snowFAC", "snowFaclat", "snowFACplus", "snowFACplusLaLo" )
model.names_R <- c("m18.19", "snowFACplussiteR", "snowFACplussiteRdate", "snowFACsiteRdate")


aictab(cand.set = models_noR, modnames = model.names_noR)
aictab(cand.set = models_R, modnames = model.names_R)

#### Figures ####

# model tables #
tab_model(m18.19)
tab_model(snowFACplus)

# 18/19 model forest plot 
plot_model(m18.19,
           title = "",
           axis.labels = c("Snow, >300 mm","Snow, 200-299 mm","Snow, 100-199 mm",
                           "Snow, 1-99 mm","Mean Ambient temperature", "Intercept"),
           colors = c("red", "black"),
           show.intercept = T,
           vline.color = "grey70") +
  theme_linedraw() +
  geom_vline(xintercept = 0) +
  theme(text = element_text(size = 14, family="Arial"))  

# snowFacplus foorest plot
plot_model(snowFACplus,
           title = "",
           axis.labels = c("Mean Ambient temperature * Snow, >300 mm",
                           "Mean Ambient temperature* Snow, 200-299 mm",
                           "Snow * Mean Ambient temperature, 100-199 mm",
                           "Snow * Mean Ambient temperature, 1-99 mm",
                           "Snow, >300 mm","Snow, 200-299 mm","Snow, 100-199 mm",
                           "Snow, 1-99 mm","Mean Ambient temperature", "Intercept"),
           colors = c("red", "black"),
           show.intercept = T,
           vline.color = "grey70") +
  theme_linedraw() +
  geom_vline(xintercept = 0) +
  theme(text = element_text(size = 14, family="Arial"))  
