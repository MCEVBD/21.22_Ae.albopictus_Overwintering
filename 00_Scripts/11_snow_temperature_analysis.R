#' ---
#' title: " 11 snow insulation (temperature) glm"
#' author: "Katie Susong"
#' date: "27 May 2022"
#' ---
#' 
#' 
#' Overview
#' =======
#' 
#' This data offers the ablity to compair two independent sets of data. In 10_18.19glm_predict.R 
#' I used the glm from Susong, Tucker et al. 2022 to predict the tire temperature. What I found
#' was promising. My next set will be to repeat the same glm formula for the 21/22 data.
#' 
#' **1.** Repeat 18/19 (Susong, Tucker et al. 2022) glm
#' 
#' 

# Libraries #
library(lubridate)
library(dplyr)
library(lme4)
library(ggplot2)
library(sjPlot) #for plotting lmer and glmer mods

#### 21/22 data ####
data <- read.csv('00_Data/21.22_snow_temperature.csv')
# format corrections
data$number   <- as.factor (data$number)
data$Date     <- ymd (data$Date)
data$location <- factor (data$location, levels = c("Car", "Bon", "Dek", "Arl", "Han", "Spo"))
data          <- filter (data, Date < "2022-04-14" ) # post collection dates

#### Change data(21/22) df format and colnames to match DIA(18/19) df ####
# b/c the first step is to repeat the 18/19 (Susong, Tucker et al. 2022) glm

# add catagorical snow values @ 100mm bins
data$snow_FAC10 <- cut(data$snodas, breaks = c(-1, 0,100,200,300,400), labels = c("none", ">100", ">200", ">300", ">400"))
# rename MeanT_Air
data <- rename(data, MeanT_Ambient = "MeanT_Air")
# rename location
data <- rename(data, site = "location")


#### Repeat 18/19 (Susong, Tucker et al. 2022) glm ####
m18.19 <- lmer(MeanT_Tire ~ MeanT_Ambient + relevel(snow_FAC10, ref = 1)+ (1|site), data = data)
summary(m18.19)
tab_model(m18.19)
plot(m18.19)
plot_model(m18.19,
           title = "",
           axis.labels = c("Snow, >300 mm","Snow, 200-299 mm","Snow, 100-199 mm",
                           "Snow, 1-99 mm","Mean Ambient temperature", "Intercept"),
           colors = c("red", "black"),
           show.intercept = T,
           vline.color = "grey70") +
  theme_linedraw() +
  geom_vline(xintercept = 0) +
  theme(text = element_text(size = 15, family="Arial"))  


