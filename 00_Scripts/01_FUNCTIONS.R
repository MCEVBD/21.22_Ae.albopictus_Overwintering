## 01 FUNCTIONS
##================##



# function adds the location/tire specific varaibles to the the  raw HOBO temperarture files 
temp.variable.add<- function(data, tireloc, tirenum, tirea, total){
  data$location <- rep(tireloc,total)
  data$number <- rep (tirenum, total)
  data$ABC <- rep(tirea, total)
  return(data)
}

