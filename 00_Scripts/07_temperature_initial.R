#' ---
#' title: "07__Temperature_inital"
#' author: "Katie M Susong"
#' date: "19-May-2022"
#' ---
#' 

#' Overview
#' =========
#' 
#'  Investigation of temperature, within and outside the temperatures
#' 
#'  Plots of the temperature over time
#'    1. Air and tire temperature over time by location and E/W 
#'    2. Bon location only 
#'    3. difference in air and tire temperarure
#'  CONCLUSIONS: The difference in air and tire temperature increases from as sites go north. 
#'  the greatest difference is seen in JAN and FEB
#'  
#'  Temperature Plots
#'    1. Tire v Air temperature  
#'  
#'  Difference Plots
#'    1. Boxplot, Diff by location
#'    2. Boxplot, Diff by location for JAN
#'    3. Boxplot, Diff by location for DJF  
#'    4. Histogram of diff 
#'  CONCLUSIONS: the differnece looks significant by location, partcularly in JAN or DJF. To test this 
#'  a glm with a normal distribution was used to determine the significance of location to difference 
#'  in tire and air temperature. This glm indicates that differnce is significantly differnce between 
#'  locations. 
#' 

# Libraries
library(lubridate)
library( ggplot2)
library(dplyr)

### Import Data ###
dia  <- read.csv("00_Data/21.22_temperature_NO.Arl3.csv")
# Format corrections
dia$DateTime <- ymd_hms(dia$DateTime)
dia$Date     <- ymd(dia$Date)
dia$location <- factor(dia$location, levels = c("CTR", "Car", "Bon", "Dek", "Arl", "Han", "Spo"))


############# PLOTS ###########

#' Temperture over time 
#'======================
#'
#' **Plot 1:** Air and tire temperature over time by location and E/W
#' 

dia %>%
  filter(location != "CTR") %>%
  ggplot( aes(DateTime, Air_Temp))+
  geom_line(col = "red")+
  geom_line(aes(DateTime, Tire), col = "blue") + 
  facet_grid(rows = vars(ABC), cols = vars(location))

#' **Plot 2:** Bon. air and tire temperature over time
#' 

dia %>%
  filter(location == "Bon") %>%
  ggplot( aes(DateTime, Air_Temp))+
  geom_line(col = "red")+
  geom_line(aes(DateTime, Tire), col = "blue") + 
  facet_grid(rows = vars(ABC), cols = vars(location))

#' **PLot 3:** Difference in air temperature over time 
#' 

dia %>%
  filter(location != "CTR") %>%
  ggplot( aes(DateTime, Diff)) +
  geom_line(col = "red") + 
  facet_grid( rows = vars(location))

#' Temperature Plots 
#' ==================
#' 
#' **Plot 1:** Air v Tire temp 
#' 
dia %>%
  filter(DateTime < "2022-05-10 24:00:00") %>%
  ggplot(aes(Tire,Air_Temp, col = location))+
  geom_point()




#' Difference Plots
#' ============
#' 
#' **Plot 1:** Boxplot, Diff by location
#' 


ggplot(dia, aes(location, Diff)) + 
  geom_boxplot()

#' **Plot 2:** Boxplot, Diff by location for JAN
#' 

dia %>%
  filter(DateTime > "2021-12-31 24:00:00", DateTime < "2022-02-01 00:00:00") %>%
  ggplot(aes(location, Diff)) + 
  geom_boxplot()

#' **Plot 3:** Boxplot, Diff by location for DJF
#' 

dia %>%
  filter(DateTime > "2021-11-31 24:00:00", DateTime < "2022-03-01 00:00:00") %>%
  ggplot(aes(location, Diff)) + 
  geom_boxplot()

#' **Plot 4:** histogram of diff
#' 

hist(dia$Diff)

model <- glm(Diff ~ location,  data = filter(dia, location != "CTR"))
summary(model) 
plot(model)

model2 <- glm(Diff ~ location + Date, data = filter(dia, location != "CTR"))
summary(model2)
