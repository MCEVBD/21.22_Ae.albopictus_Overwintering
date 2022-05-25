#' ---
#' title: "02B add daily temperature to all snow file"
#' author: "Katie M Susong"
#' date: "05-May-2022"
#' ---
#' 

#' Overview
#' ========
#' 
#' Before completing regressions of tire temperature realted to snow. 
#' 
#' Variabled added:
#' 
#'   mean daily T ( air and tire)
#'   max daily  T ( air and tire)
#'   min daily  T ( air and tire)
#'   daily Diff ( mean, max, min) 
#'   
#'   

## libraries ##
library(lubridate)
library(dplyr)
## Import data ##

# temperature data
dia  <- read.csv("00_Data/21.22_temperature.csv")
# format corrections
dia$DateTime <- mdy_hm(dia$DateTime)
dia$number   <- as.factor(dia$number)
dia$Date     <- mdy(dia$Date)

# snow data
snow <-  read.csv("00_Data/21.22_all_snow.csv")
# format corrections
snow$Date <- mdy(snow$Date)



#### Daily temperature values ####

temp <- dia %>%
  group_by(number,Date) %>%
  dplyr::summarise(MeanT_Air = mean(Air_Temp), 
                   MeanT_Tire = mean(Tire),
                   MinT_Air = min(Air_Temp),
                   MinT_Tire = min(Tire),
                   MaxT_Air = max(Air_Temp),
                   MaxT_Tire = max(Tire))
  
  
  
  
  
  