#' Temperature file generation
#' ===========================
#' 
#' The following sources are combine into a single temperaure file:
#'      - 13 HOBO temperature monitors contained with in both tires located at each site 
#'            - original variables: DateTime, Tire, Tire_RH
#'      - 06 HOBO monitors with external probe located in the west tires at each site
#'            - original varaibles: DateTime, Tire_b, Soil
#'      - 06 hourly weather data files for the weather monitoring stations located at each site
#'             - original varaibles: Air_Temp, RH, Solar radiation, precip, (Date, time or DateTime)
#'      
#' The resulting dataframe is in a "wide" format and contains the following variables:
#'      - location:    the 6 study sites
#'      - number:      1-13, tire number N. to S., E. to W.
#'      - DateTime:    ymd_hhmm
#'      - ABC:         E, W or S (only at Arl) tire location within location
#'      - Air_Temp:    weather station 2m temperature 
#'      - Tire:        HOBO recorded temperature recorded within the tire
#'      - Tire_b:      HOBO recorded by probe+ monitor, in base of tire 
#'      - Soil:        HOBO recorded temperature in soil 1m from the W tire
#'      - Diff:        difference in temp between Air_Temp and Tire
#'      - DiffS:       difference between Air_Temp and Soil
#'      - DiffST:      difference between Tire and Soil 
#'      
#' Arl_T2 will be exculded from this data tabulation due to a monitor fail    


#lilbraries
library(tidyr)
library(plyr)
library(dplyr)
library(lubridate)


######### Import raw site files ##############
## RAW files are not included in the git files and version controling 

# Move working directory inside Raw files folder
setwd("~/Documents/CBS_PhD/Ae.albo_OW_2021/21.22_Ae.albopictus_Overwintering/00_Data/01_RAW_temperature")
# generate list of file names
filenames <- gsub("\\.csv$","", list.files(pattern="\\.csv$"))
# Import files
for(i in filenames){
  assign(i, read.csv(paste(i, ".csv", sep="")))
}
# correct working directory to Project level 
setwd("~/Documents/CBS_PhD/Ae.albo_OW_2021/21.22_Ae.albopictus_Overwintering")


############# Merge soil temperature files with tire files ##########
# The extsoil montiors are located within the W tire at each site
# before adding site specific varaible the Tire, Tire_b and Soil variables will be merged
# merge dataframes will be stored as (location)_T2, as this is the correct location 
## again Arl_T2 will not be included as that HOBO monitor falled

#Both T1 and T2 are using the same Soil temperature. T1 does not have a tire_b ( bottom ) temperature 

#T1 merge, remove Tire_b

Spo_T1 <- merge(Spo_T1,Spo_extSoil,by="DateTime", all = T)   # merge T1 and extSoil
Spo_T1 <- subset(Spo_T1, select = -c(Tire_b))                # remove Tire_b from T1 file

Han_T1 <- merge(Han_T1,Han_extSoil,by="DateTime", all = T)
Han_T1 <- subset(Han_T1, select = -c(Tire_b))

Del_T1 <- merge(Del_T1,Del_extSoil,by="DateTime", all = T)
Del_T1 <- subset(Del_T1, select = -c(Tire_b))

Bon_T1 <- merge(Bon_T1,Bon_extSoil,by="DateTime", all = T)
Bon_T1 <- subset(Bon_T1, select = -c(Tire_b))

Car_T1 <- merge(Car_T1,Bon_extSoil,by="DateTime", all = T)
Car_T1 <- subset(Car_T1, select = -c(Tire_b))

# T2 merge, maintain Tire_b
Spo_T2 <- merge(Spo_T2,Spo_extSoil,by="DateTime", all = T)
Han_T2 <- merge(Han_T2,Han_extSoil,by="DateTime", all = T)
Del_T2 <- merge(Del_T2,Del_extSoil,by="DateTime", all = T)
Bon_T2 <- merge(Bon_T2,Bon_extSoil,by="DateTime", all = T)
Car_T2 <- merge(Car_T2,Bon_extSoil,by="DateTime", all = T)



######## Add needed variables to files ########
## location, number, ABC
## fucntion save in 01_FUNCTIONS.R script 
source('00_Scripts/01_FUNCTIONS.R')

Arl_T1      <- temp.variable.add(Arl_T1, "Arl", "5", "E", "4760")
Arl_T3      <- temp.variable.add(Arl_T3, "Arl", "7", "S", "4760")
Arl_extSoil <- temp.variable.add(Arl_extSoil, "Arl", "6","W", "4736")

Bon_T1      <- temp.variable.add(Bon_T1, "Bon", '10', 'E', '4736')
Bon_T2      <- temp.variable.add(Bon_T2, "Bon", '11', 'W', '4736')

Car_T1      <- temp.variable.add(Car_T1, 'Car', '12', 'E', '4736')
Car_T2      <- temp.variable.add(Car_T2, 'Car', '13', 'W', '4736')

CTR         <- temp.variable.add(CTR, "CTR", 'NA', 'NA', '4760')

Del_T1      <- temp.variable.add(Del_T1, 'Del', '8', 'E', '4736')
Del_T2      <- temp.variable.add(Del_T2, 'Del', '9', 'W', '4736')

Han_T1      <- temp.variable.add(Han_T1, 'Han', '3', 'E', '4760')
Han_T2      <- temp.variable.add(Han_T2, 'Han', '4', 'W', '4760')

Spo_T1      <- temp.variable.add(Spo_T1, 'Spo', '1', 'E', '4760')
Spo_T2      <- temp.variable.add(Spo_T2, 'Spo', '2', 'W', '4760')


######## R-bind files togauther ########

# plyr::rbind.fill() is used b/c there are unique varaibles in some dfs that need to be carried through
Temp <- rbind.fill(CTR,
                   Arl_T1, Arl_T3, Arl_extSoil,
                   Bon_T1, Bon_T2,
                   Car_T1, Car_T2,
                   Del_T1, Del_T2,
                   Han_T1, Han_T2,
                   Spo_T1, Spo_T2)

############ Correct DataTime format in Temp ############

Temp$DateTime <- mdy_hm(Temp$DateTime)

#########################################################
######### Import raw weather station files ##############
## RAW files are not included in the git files and version controling 
## IL and WI weather stations have differnt file formates so they will handle seperatly 

# add IL stations (Already located in a single file)
IL_stations <- read.csv("00_Data/01_RAW_temperature/Station/IL_stations.csv")

# add WI stations, currently in 3 files

# Move working directory inside Raw files folder
setwd("~/Documents/CBS_PhD/Ae.albo_OW_2021/21.22_Ae.albopictus_Overwintering/00_Data/01_RAW_temperature/Station/WI_stations")
# generate list of file names
filenames <- gsub("\\.csv$","", list.files(pattern="\\.csv$"))
# Import files
for(i in filenames){
  assign(i, read.csv(paste(i, ".csv", sep="")))
}
# correct working directory to Project level 
setwd("~/Documents/CBS_PhD/Ae.albo_OW_2021/21.22_Ae.albopictus_Overwintering")


############### Add location code to IL stations #############

IL_stations$location <- "Del"
IL_stations$location[IL_stations$Station == "Bondville"]  <- "Bon"
IL_stations$location[IL_stations$Station == "Carbondale"] <- "Car"

########### Correct DateTime format in IL stations #############

IL_stations$DateTime <- mdy_hm(IL_stations$DateTime)

############ Add location code to WI stations ##########

Arl_station$location <- "Arl"
Han_station$location <- "Han"
Spo_station$location <- "Spo"


############# Merge date and time WI stations ##########

# function (edit.date) corrects format @ 24hrs and adds DateTime element

Arl_station <- edit.date(Arl_station)
Han_station <- edit.date(Han_station)
Spo_station <- edit.date(Spo_station)

############## R bind all Station dataframes ############

# plyr::rbind.fill() is used b/c there are unique varaibles in some dfs that need to be carried through
Temp_stations <- rbind.fill(IL_stations,
                   Arl_station,
                   Han_station,
                   Spo_station)

#########################################################
############### Merge tire and station data #############
#' To merge the tire data and station I will shift the tire 
#' data to a tidy format and add a the station data. (6am 04-May-22)
#' 
#' I was wrong. I think I need to merge the two dataframe first and 
#' them move it from wide to long to wide to replicate the station 
#' data across both tires (E and W) (7am 04-May-22)
#' 
#' Method two did not do what I wanted it to so I will try using a 
#' merge function (7am 04-May-22) NOTE TO SELF: think simple first!

# merge matching DateTime and location, this will result in the station data being repeated for each tire at that station.
Temp.ALL <- merge(Temp, Temp_stations, by = c("location", "DateTime"))


############ Clean up unneeded variables ###############
# left over date, time, station varaibles removed

Temp.ALL.b <- subset(Temp.ALL, select = -c(date, time, Station))

# clean up col variable order
colnames(Temp.ALL.b)
ColName <- c("location", "number", "ABC", "DateTime",   
             "Air_Temp", "Tire", "Tire_b", "Soil",
             "RH", "Tire_RH",
             "Solar", "lwsZ_pwet", "pcpn", "soil_Z", "dwpt", "stmp_05cm")  # create reordered col list
Temp.ALL.b <-Temp.ALL.b[,ColName]  #reorder using list


############## Add new calculated variables to df ###############

# Diff = Tire_temp - Air_temp ( + = warmer tire)
Temp.ALL.b$Diff    <- Temp.ALL.b$Tire - Temp.ALL.b$Air_Temp 

# Diff.S = soil_temp - Air_temp (+ = warmer soil)
Temp.ALL.b$Diff.S  <- Temp.ALL.b$Soil - Temp.ALL.b$Air_Temp

#Diff.ST = tire_temp - soil_temp (+ = warmer tire)
Temp.ALL.b$Diff.ST <- Temp.ALL.b$Tire - Temp.ALL.b$Soil

#Diff.RH = tireRH - AirRH (+ = wetter tire)
Temp.ALL.b$Diff.RH <- Temp.ALL.b$Tire_RH - Temp.ALL.b$RH

# Diff.b = Tire_bottom_temp - Air_temp ( + = warmer tire)
Temp.ALL.b$Diff.b    <- Temp.ALL.b$Tire_b - Temp.ALL.b$Air_Temp 


