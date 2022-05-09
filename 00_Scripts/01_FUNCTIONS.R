## 01 FUNCTIONS
##================##


#######Required Packages#########
library(dplyr)
library(lubridate)


# function adds the location/tire specific varaibles to the the  raw HOBO temperarture files 
temp.variable.add<- function(data, tireloc, tirenum, tirea, total){
  data$location <- rep(tireloc,total)
  data$number <- rep (tirenum, total)
  data$ABC <- rep(tirea, total)
  return(data)
}


# Function corrects date and time str adn makes a single DateTime col.
## currently each day at 24:00 did not pharse and create a DateTime varaible b/c the format of 24:00:00 is differnt (second are included). I need to correct that format to hm before pharsing. 
edit.date <- function(df){
  df <- df %>%
    mutate(time = recode(time,'24:00:00' = "24:00")) # correct recorded time to match format 
  df$DateTime <- mdy_hm(paste(df$date, df$time))     # create Datetime col.
  return(df)
}
  


# FUCNTION: F to C 

FtoC <- function(Far){
  (Far - 32) * 5/9 
}
