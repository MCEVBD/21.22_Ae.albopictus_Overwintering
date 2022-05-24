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
#'  Difference Plots
#'    1. Boxplot, Diff by location
#'    2. Boxplot, Diff by location for JAN
#'    3. Boxplot, Diff by location for DJF   
#' 

# Libraries
library(lubridate)
library( ggplot2)
library(dplyr)

### Import Data ###
dia  <- read.csv("00_Data/21.22_temperature_NO.Arl3.csv")
# Format corrections
dia$DateTime <- ymd_hms(dia$DateTime)
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


#' Other Plots
#' ============
#' 
#' **Plot 1:** Boxplot, Diff by location
#' 


ggplot(dia, aes(location, Diff)) + 
  geom_boxplot()

#' **Plot 2:** Boxxplot, Diff by location for JAN
#' 

dia %>%
  filter(DateTime > "2021-12-31 24:00:00", DateTime < "2022-02-01 00:00:00") %>%
  ggplot(aes(location, Diff)) + 
  geom_boxplot()

#' **Plot 3:** Boxxplot, Diff by location for DJF

dia %>%
  filter(DateTime > "2021-11-31 24:00:00", DateTime < "2022-03-01 00:00:00") %>%
  ggplot(aes(location, Diff)) + 
  geom_boxplot()




