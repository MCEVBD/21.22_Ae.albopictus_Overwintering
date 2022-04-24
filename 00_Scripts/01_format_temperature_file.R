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

######## Add needed variables to files ########
## location, number, ABC
## fucntion save in 01_FUNCTIONS.R script 
source('00_Scripts/01_FUNCTIONS.R')

Arl_T1      <- temp.variable.add(Arl_T1, "Arl", "5", "E", "4760")
