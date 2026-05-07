#' ---
#' title: "11 snow insulation (temperature) glm"
#' author: "Katie Susong"
#' date: "27 May 2022"
#' ---
#'
#' Overview
#' =======
#'
#' Model selection for tire temperature prediction using the 2021/2022 data.
#'
#' This script compares a set of candidate models for mean daily tire temperature using
#' ambient temperature, categorical SNODAS snow depth bins, and (optionally) site as a
#' random effect, plus simple extensions (date and latitude).

rm(list = ls())

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

#### Change data(21/22) df format and colnames to match DIA(18/19) df ####

# add catagorical snow values @ 100mm bins
data$snow_FAC10 <- cut(data$snodas, breaks = c(-1, 0,100,200,300,400), labels = c("none", ">100", ">200", ">300", ">400"))
# rename MeanT_Air
data <- rename(data, MeanT_Ambient = "MeanT_Air")
# rename location
data <- rename(data, site = "location")

#### Distribution plot #### 
hist(data$MeanT_Tire)

#### Model Creation ####

# Repeat 18/19 model structure (Susong, Tucker et al. 2022)
m18.19 <- lmer(MeanT_Tire ~ MeanT_Ambient + relevel(snow_FAC10, ref = "none") + (1|site), data = data)

#### Independent regression development ####

#### 1. Null model ####
null <- glm(MeanT_Tire ~ 1, data = data)

#### 2. Air ####
air <- glm(MeanT_Tire ~ MeanT_Ambient, data = data)

#### 3. Snow ####
snow <- glm(MeanT_Tire ~ MeanT_Ambient + snow_FAC10, data = data)

#### 4. Snow + air interaction ####
snowplus <- glm(MeanT_Tire ~ MeanT_Ambient + snow_FAC10 + MeanT_Ambient:snow_FAC10, data = data)

#### 5. Add site as random effect ####
siteR <- lmer(MeanT_Tire ~ MeanT_Ambient + snow_FAC10 + (MeanT_Ambient*snow_FAC10) + (1|site), data = data)

#### 6. Random effect without interaction ####
siteRmin <- lmer(MeanT_Tire ~ MeanT_Ambient + snow_FAC10 +  (1|site), data = data)

#### 7. Add date ####
datemin <- lmer(MeanT_Tire ~ MeanT_Ambient + snow_FAC10 + Date + (1|site), data = data)

#### 8. Add date to interaction model ####
dateplus <- lmer(MeanT_Tire ~ MeanT_Ambient + snow_FAC10 + (MeanT_Ambient*snow_FAC10) + Date + (1|site), data = data)

#### 9. Include latitude (uses `y` column) ####
lat <- glm(MeanT_Tire ~ MeanT_Ambient + snow_FAC10 + y, data = data)

#### Summaries / Tables ####
summary(m18.19)
tab_model(m18.19)

summary(null)
summary(air)
summary(snow)
summary(snowplus)

summary(siteR)
summary(siteRmin)
summary(datemin)
summary(dateplus)

summary(lat)

#### Selection (AIC tables) ####
models_noR <- list(null, air, snow, snowplus, lat)
model.names_noR <- c("null", "air", "snow", "snowplus", "lat")
aictab(cand.set = models_noR, modnames = model.names_noR)

models_R <- list(m18.19, siteR, siteRmin, datemin, dateplus)
model.names_R <- c("m18.19", "siteR", "siteRmin", "datemin", "dateplus")
aictab(cand.set = models_R, modnames = model.names_R)

#### Figures ####
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
