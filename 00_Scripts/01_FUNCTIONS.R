## 01 FUNCTIONS
##================##



# function adds the location/tire specific varaibles to the the  raw HOBO temperarture files 
temp.variable.add<- function(data, tireloc, tirenum, tirea, total){
  data$location <- rep(tireloc,total)
  data$number <- rep (tirenum, total)
  data$ABC <- rep(tirea, total)
  return(data)
}


# Function corrects date and time str adn makes a single DateTime col.
#FAILED
edit.date <- function(df){
  df[df$time == 24:00:00] <- 24:00
  df$DateTime <- mdy_hm(paste(df$date, df$time)) # create Datetime col.
  return(df)
}
  
