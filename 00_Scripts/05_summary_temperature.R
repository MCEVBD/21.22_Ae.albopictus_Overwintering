#' ---
#' title: "05 Temperature summary calculation"
#' author: "Katie M Susong"
#' date: "09-May-2022"
#' ---

#' Temperature summary values 
#' ===========================
#' 
#' To understand the correlation between mosqutio survival and the temperature 
#' within and outisde the tire summery temperature values are calculated. The values 
#' calacuated here are:
#'      - Mean Jan. temperature
#'      - Diff. in Mean Jan. temperature 
#'      - Mean DJF  temperature
#'      - Diff. in Mean DJF mean temperature 
#'      - Consecutive hours below -12C
#'      - Number of Days with temperatures below -12C
#'      - Date of First Frost 
#' 


# libraries
library(dplyr)
library(lubridate)

# import data 
data  <- read.csv("00_Data/21.22_temperature.csv")
# formart correction
data$DateTime <- ymd_hms(data$DateTime)
hatch <- read.csv("00_Data/21.22_section_survival_data.csv")

############ Mean Jan. Temperature #############
### TIRE and AIR ###

Tt<- data %>% 
  filter(DateTime > "2021-12-31 24:00:00", DateTime < "2022-02-01 00:00:00") %>%  # filter to JAN
  group_by(number) %>%
  dplyr::summarise(JANmeanT = mean(Tire),JANmeanA = mean(Air_Temp)) 





