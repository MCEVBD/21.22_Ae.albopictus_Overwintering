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
#' (soil which represents the traditional subnivium and our tire sites) can be compared 
#¢ 
#¢ Timeline 
#' =======
#' 
#' **25-Oct-22**:  imported and formatted hourly data -> Inital plots created.
#'                  daily data + snow imported -> initial plot. **KMS** 
#'
#' **26-Oct-22**:  person corr, 
#'                 regression analysis including various snow cover metrics: 
#'                        1. MeanT_Tire ~ MeanT_Soil
#'                        2. MeanT_Tire ~ MeanT_Soil + snodas
#'                        3. MeanT_Tire ~ MeanT_Soil + coverM
#'                        4. MeanT_Tire ~ MeanT_Soil + snow_FAC10
#'                        5. MeanT_Tire ~ MeanT_Soil + (snow_FAC10:cover)  **KMS**
#'                        
#' **7-Nov-22**: regression analysis -> additional varaibles/models created. AIC and LogLik calculated 
#'                        6. 5. MeanT_Tire ~ MeanT_Soil + (snow_FAC10:cover) + cover **KMS**


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
dia$cover    <- factor (dia$cover, levels = c("none", "side", "top", "both"))


#### Inital Plot ####

#'### Hourly data
#'
ggplot(data) +
  geom_line(aes(Date, Air_Temp), col = "black") +
  geom_line(aes(Date, Tire), col = "blue") +
  geom_line(aes(Date, Soil), col = "green") +
  #facet_wrap(~ number) + # numbers run N (1) to S (13) 
  ylab("Hourly temperature (??C)") 
  


  
ggplot(data) +
  geom_line(aes(Date, Air_Temp), col = "black") +
  geom_line(aes(Date, Tire), col = "blue") +
  geom_line(aes(Date, Soil), col = "green") +
  facet_wrap(~ location)

#' Looking at these two plots it is clear that the soil temperature is more stable ( compared to air and tire temp) 
#' regardless of season (aka temperature and snow cover). Note that at Spo (number 1-2) the winter soil temperature 
#' is the most stable (likely due to stable subnivium development). Sites with warmer air temperature but no 
#' snow cover (location: Car, Bon, Dek; number: greater than 7) had colder soil temperature than Spo. It is possible
#' that there is an offset for tire+snow temp compared to the sol temperature (which is recorded at weather
#' (monitoring sites) but the trend is not obviously any clearer than that of air temperature

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
d <- ggplot(dia, aes(snow_FAC10, (MeanT_Soil - MeanT_Tire))) +
  geom_boxplot()

plot_grid(a, b,c,d)}

{a <- ggplot(dia, aes(MeanT_Soil, MeanT_Tire, col = snodas )) +
  geom_point()
b <- ggplot(dia, aes(MeanT_Soil, MeanT_Tire, col = depth_G )) +
  geom_point()
c <- ggplot(dia, aes(MeanT_Soil, MeanT_Tire, col = depth_T )) +
  geom_point()
d <- ggplot(dia, aes(MeanT_Soil, MeanT_Tire, col = snow_FAC10 )) +
  geom_point()
plot_grid(a, b,c,d)}


#' Daily average data confirms the findings of the hourly data, namely that sol temperature is more stable than 
#' tire temperatures. There may be a slight trend that increasing snow cover (measured on the ground and possibly
#' on the tire) decreases the difference between soil and tire temp. This is not seen when looking at snodas 
#' measures. 

#### Correlation ####
# Pearson correlation test
cor.test(dia$MeanT_Tire, dia$MeanT_Soil, method = "pearson")

#### Regression ####

# add factor (100mm bin) snodas variable for analysis
dia$snow_FAC10 <- cut(dia$snodas, breaks = c(-1, 0,100,200,300,400), labels = c("none", ">100", ">200", ">300", ">400"))

model1   <- lm(MeanT_Tire ~ MeanT_Soil, data = dia)
model2   <- lm(MeanT_Tire ~ MeanT_Soil + snodas, data = dia)
model3   <- lm(MeanT_Tire ~ MeanT_Soil + cover, data = dia) 
model4   <- lm(MeanT_Tire ~ MeanT_Soil + snow_FAC10, data = dia) 
model4.1 <- lm(MeanT_Tire ~ MeanT_Soil + snow_FAC10 + cover, data = dia) 
model5   <- lm(MeanT_Tire ~ MeanT_Soil + (snow_FAC10:MeanT_Soil) + snow_FAC10, data = dia) 
model6   <- lm(MeanT_Tire ~ MeanT_Soil + (snow_FAC10:MeanT_Soil) + snow_FAC10 + cover, data = dia) 

# model summary 

summary(model1)
summary(model2)
summary(model3)
summary(model4)
summary(model4.1)
summary(model5)
summary(model6)

# fit values 

AIC(model1, model2, model3, model4, model4.1, model5, model6)

logLik(model1)
logLik(model2)
logLik(model3)
logLik(model4)
logLik(model4.1)
logLik(model5)
logLik(model6)

summary(model1)$r.squared
summary(model2)$r.squared
summary(model3)$r.squared
summary(model4)$r.squared
summary(model4.1)$r.squared
summary(model5)$r.squared
summary(model6)$r.squared

#' Soil and tire temperature are correlated (per. corr = 0.88558). The inclusion of snow variables improve that fit 
#' of lms. These lms indicate that at low snow and high snow the difference between the tire and soil is the smallest.
#' It is possible that solar radiation at low snow and insulation at high snow are the contributing factors. Cover 
#' extent improves model fit. the top of the tire being covered did not significantly change the mean tire temp while 
#' the side or both the top and side being covered was significant. 
#' 
#' The use of soil temperature instead of air temperature to predict tire temperature is not suggested as the fit of
#' the lm is lower, particularly for the lm that only contains easily sources macro-varaibles. 
#' 