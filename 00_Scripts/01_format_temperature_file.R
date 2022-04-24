#' Temperature file generation
#' ===========================
#' 
#' The following sources are combine into a single temperaure file:
#'      - 13 HOBO temperature monitors contained with in both tires located at each site 
#'      - 06 HOBO monitors with external probe located in the west tires at each site
#'      - 06 hourly weather data files for the weather monitoring stations located at each site
#'      
#' The resulting dataframe is in a "wide" format and contains the following variables:
#'      - location:    the 6 study sites
#'      - number:      1-13, tire number N. to S., E. to W.
#'      - DateTime:    ymd_hhmmss
#'      - ABC:         E, W or S ( only at Arl) tire location within location
#'      - Ambient:     weather station 2m temperature 
#'      - Tire:        HOBO recorded temperature recorded within the tire
#'      - Tire_b:      HOBO recorded by probe+ monitor, in base of tire 
#'      - Soil:        HOBO recorded temperature in soil 1m from the W tire
#'      - Diff:        difference in temp between Ambient and Tire
#'      - DiffP:       difference between Ambient and Probe
#'      - DiffPT:      difference between Tire and Probe 
#'      
#' Arl_T2 will be exculded from this data tabulation due to a monitor fail    


#lilbraries


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
# The extsoil montiors are located within the W tire ate each site
# before adding site specific varaible the Tire, Tire_b and Soil variables will be merged
# merge dataframes will be stored as (location)_T2, as this is the correct location 
## again Arl_T2 will not be included as that HOBO monitor falled

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




