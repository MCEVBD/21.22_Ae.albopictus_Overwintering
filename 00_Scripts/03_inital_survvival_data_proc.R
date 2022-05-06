#' ---
#' title: "03 Inital Survial Visulization (Pre temperature analysis)"
#' author: "Katie M. Susong"
#' date: "05-May-2022"
#' --- 

#' Initial formating and visulization of surival data
#' ==================================================
#' 
#'  New varaibles generated:
#'     - Tire_num
#'     - ABC (orintations)
#'     - per.sur
#'     - rel.per.sur
#'     - per.red
#'     
#' visulizations : 
#'      -
#'      -
#'      -


#libraries
library(readr)

######### Import data #######

data <- read_csv("00_Data/21.22_section_survival_data.csv",
                 col_types = cols(dead.hatch1 = col_integer(),dead.hatch2 = col_integer(), dead.hatch3 = col_integer(), dead.hatch4 = col_integer(),
                                  eeg1b = col_integer(), egg1 = col_integer(),
                                  live.hatch1 = col_integer(), live.hatch2 = col_integer(), live.hatch3 = col_integer(), live.hatch4 = col_integer(),
                                  sheet = col_factor(levels = c("A", "B", "C")), 
                                  site = col_factor(levels = c("Spo","Han", "Arl", "Dek", "Bon", "Car")), 
                                  x = col_number(), y = col_number()))

########### Extract new variables ###########

# 1) Tire_number
# extract characters 
data$tire_num <- substr(data$loc.id,8,9) # numbers refer to start and stop character number


# 2) ABC (east, west or south tire)
data$ABC <- "E"
data$ABC[data$tire_num == "T2"] <- "W"
data$ABC[data$tire_num == "T3"] <- "S"


