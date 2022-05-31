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
#' A.  Repeat 18/19 (Susong, Tucker et al. 2022) glm
#' 
#' B. Independent model development
#'     pdf of MeanT_Tire is normal
#'
#'   1. Null model (null): MeanT_Tire ~ 1
#'     - AIC: 15960
#'     - Diagnostic plots: 
#'   2. Include MeanT_Ambient (air): MeanT_Tire ~ MeanT_Ambient
#'     - AIC: 8364.4
#'     - log Lik.' -4179.184 (df=3)
#'     - Diagonostic plots: 
#'   3. Include SNODAS (snow): MeanT_Tire ~ MeanT_Ambient + snow_FAC10
#'     - AIC: 8086.5
#'     - 'log Lik.' -4036.231 (df=7)
#'     - Diagonostic plots: 
#'   4. air and SNODAS intereaction (snowplus): MeanT_Tire ~ MeanT_Ambient + snow_FAC10 + (MeanT_Ambient*snow_FAC10)
#'     - AIC: 7059
#'     - 'log Lik.' -3518.48 (df=11)
#'     - I am questioning the values and the biolgical relevence of including this intereaction. I think that 
#'     it is creating problems b/c snow and air temp are correlated loosely 
#'   5. add site as random effect (siteR) :MeanT_Tire ~ MeanT_Ambient + snow_FAC10 + (MeanT_Ambient*snow_FAC10) + (1|site)
#'     - AIC: 7020.697
#'     - 'log Lik.' -3498.348 (df=12)
#'   6. siteR minus air:snow interaction (siteRmin): MeanT_Tire ~ MeanT_Ambient + snow_FAC10 + (1|site)
#'     - AIC: 8045.815
#'     - 'log Lik.' -4014.908 (df=8)
#'   7. add date to min model (datemin): MeanT_Tire ~ MeanT_Ambient + snow_FAC10 + date + (1|site) 
#'     - AIC:8058.785
#'     - 'log Lik.' -4020.393 (df=9)
#'     **reduced fit**
#'   8. add date to plus model (dateplus): MeanT_Tire ~ MeanT_Ambient + snow_FAC10 + (MeanT_Ambient*snow_FAC10) + date + (1|site) 
#'     - AIC:7016.85
#'     - 'log Lik.' -3495.425 (df=13)
#'     **minimal fit improvment** compared to snowplus
#'   9. inlcude lat (lat): MeanT_Tire ~ MeanT_Ambient + snow_FAC10 + y
#'      - AIC : 8085.64
#'      - 'log Lik.' -4034.81 (df=8)
#'      ** model fit NOT improved **
 
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

#### Independed regression development ####

#### Null model ####
null <- glm(MeanT_Tire ~ 1, data = data)
summary(null)
plot(null)

#### Air ####
air <- glm(MeanT_Tire ~ MeanT_Ambient, data = data)
summary(air)
logLik(air)
plot(air)

#### snow ####
snow <- glm(MeanT_Tire ~ MeanT_Ambient + snow_FAC10, data=data)
summary(snow)
logLik(snow)
plot(air)
plot_model(snow)

#### snowplus ####
snowplus <- glm(MeanT_Tire ~ MeanT_Ambient + snow_FAC10 + MeanT_Ambient:snow_FAC10, data=data)
summary(snowplus)
logLik(snowplus)
plot(snowplus)
tab_model(snowplus)
plot_model(snowplus)
anova(snow, snowplus)

#### siteR ####
siteR <- lmer(MeanT_Tire ~ MeanT_Ambient + snow_FAC10 + (MeanT_Ambient*snow_FAC10) + (1|site), data = data)
summary(siteR)
AIC(siteR)
logLik(siteR)
tab_model(siteR)

#### siteRmin ####
siteRmin <- lmer(MeanT_Tire ~ MeanT_Ambient + snow_FAC10 +  (1|site),data= data)
summary(siteRmin)
AIC(siteRmin)
logLik(siteRmin)

#### datemin ####
datemin <- lmer(MeanT_Tire ~ MeanT_Ambient + snow_FAC10 + Date + (1|site) , data = data)
summary(datemin)
AIC(datemin)
logLik(datemin)

#### dateplus ####
dateplus <- lmer(MeanT_Tire ~ MeanT_Ambient + snow_FAC10 + (MeanT_Ambient*snow_FAC10)+ Date + (1|site), data = data)
summary(dateplus)
AIC(dateplus)
logLik(dateplus)
plot_model(dateplus)

#### lat ####
lat <- glm(MeanT_Tire ~ MeanT_Ambient + snow_FAC10 + y, data= data)
summary(lat)
AIC(lat)
logLik(lat)
plot_model(lat)
