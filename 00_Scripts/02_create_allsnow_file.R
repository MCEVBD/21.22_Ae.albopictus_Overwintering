#' ---
#' title: "02_format_snowdata"
#' author: "Katie M Susong"
#' date: "05-May-2022"
#' ---
#' 

#' Format and generate single snow/SNODAS file
#' ===========================================
#' 
#' The 2021-2022 study of overwintering in Ae. alvopictus included two sources of snow related 
#' data:
#'    - Camera mounted on 2M stakes with marked (2cm) depth stakes to record snow depth daily
#'    - SNODAS daily depth approximations, NOAA data assumilation (https://nsidc.org/data/g02158)
#'    
#' These two sources will be merges into a single data file containing the following variables:
#'    - location
#'    - x and y (lat/long)
#'    - ground depth (daily)
#'    - depth on top of tire (daily)
#'    - y/n is the side/top of the tire covered (daily)
#'    - SNODAS depth approximation (daily)
#'    

# lilbraries
library(lubridate)

############# Import individual datasets ##########

# Observed snow data
obs.snow <- read.csv("00_Data/21.22_snowdepth.csv")
# correct date structure
obs.snow$date <- mdy(obs.snow$date)

#SNODAS data 
snodas <- read.csv("00_Data/21.22_SNODAS.csv")
# correct date str
snodas$date <- mdy(snodas$date)

# site data (used to add x, y)
site <- read.csv("00_Data/21.22_studysite_location.csv")


######## merge observed data and SNODAS ###########

ALL <- merge(obs.snow, snodas,c("location","date"))  # merge completed by date and location

######## add lat/long to ALL file ##############

ALL <- merge(ALL,site, "location" )


######### write ALL snowfile to 00_Data #######

write.csv(ALL, "00_Data/21.22_all_snow.csv")


             