#' ---
#' title: "05b Temperature summary calculation FOR Arl_T3"
#' author: "Katie M Susong"
#' date: "18-May-2022"
#' ---

#' Temperature summary values 
#' ===========================
#' 
#' To understand the correlation between mosqutio survival and the temperature 
#' within and outisde the tire summery temperature values are calculated. The values 
#' calacuated here are:
#'      - Mean Jan. temperature
#'      - Diff. in Mean Jan. temperature *
#'      - Mean DJF  temperature
#'      - Diff. in Mean DJF mean temperature *
#'      - Min tmeperature 
#'      - Diff in min tmeperature *
#'      - Consecutive hours below -12C
#'      - Number of Days  with temperatures below -12C
#'      - Date of First Frost 
#'      - GDD 
#'          - GDD_10: measure  of  the  magnitude  by  which  daily  average 
#'          temperatures exceed a baseline temperature of 10°C.
#'          (per Johnson 2017 Jour.Med.Ent.)
#'          - GDD_n12:  exceeded baseline temperature of 10°C. ( considered non-leathel days)
#'  
#'      
#'  * for diff. calculations positive number indicates warmer temperatures in the tire. 


# libraries
library(dplyr)
library(tidyr)
library(lubridate)
library(chillR)

### import data ###

#temperature data
data  <- read.csv("00_Data/21.22_temperature_Arl3.csv")
# format correction
data$DateTime <- mdy_hm(data$DateTime)
data$Date     <- mdy(data$Date)
data$number   <- as.factor(data$number)
data$sheet    <- as.factor(data$sheet)

# hatch data
hatch <- read.csv("00_Data/21.22_section_survival_data.csv")
# format correction
hatch$number <- as.factor(hatch$number)
hatch$sheet  <- as.factor(hatch$sheet)


# filter dataframe to only Arl_T3 (the summary values for it will need to created seperatly)
hatch <- filter(hatch, number == "7")

############ Mean Jan. Temperature #############
### TIRE and AIR ###

Tt<- data %>% 
  filter(DateTime > "2021-12-31 24:00:00", DateTime < "2022-02-01 00:00:00") %>%  # filter to JAN
  group_by(sheet) %>%
  dplyr::summarise(JANmeanT = mean(Tire),JANmeanA = mean(Air_Temp)) 
#merge with summary DF
hatch <- merge(hatch, Tt,"sheet")
# correct format
hatch$JANmeanT <- round( as.numeric(hatch$JANmeanT), 1)
hatch$JANmeanA <- round( as.numeric(hatch$JANmeanA), 1)

############ Diff. in mean Jan. temperature ############

hatch$JANmeanD <- -(abs(hatch$JANmeanT) - abs(hatch$JANmeanA))

############ Winter Mean Temperature ############
### TIRE and AIR ###

Tt<-data %>% 
  filter(DateTime > "2021-11-30 24:00:00", DateTime < "2022-03-01 00:00:00") %>%  # filter to DJF
  group_by(sheet) %>%
  dplyr::summarise(DJFmeanT = mean(Tire),DJFmeanA = mean(Air_Temp))               # calcualte mean 
#merge with summary DF
hatch <- merge(hatch, Tt,"sheet")
# correct format
hatch$DJFmeanT <- round (as.numeric(hatch$DJFmeanT), 2)
hatch$DJFmeanA <- round( as.numeric(hatch$DJFmeanA), 2)

############ Diff. in mean DJF temperature ############

hatch$DJFmeanD <- -(abs(hatch$DJFmeanT) - abs(hatch$DJFmeanA))


############ Min. Temp in Tires #################

Tt <- data %>%
  group_by(sheet) %>%                               # group by location
  dplyr::summarise(MinT = min(Tire, na.rm = T), 
                   MinA = min(Air_Temp, na.rm = T))  # min temperature

#merge with summary DF
hatch <- merge(hatch, Tt,"sheet")
# correct format
hatch$MinT <- round (as.numeric(hatch$MinT), 2)
hatch$MinA <- round (as.numeric(hatch$MinA), 2) 

############ Diff in Mean temperataure #############

hatch$MinD <- hatch$MinT - hatch$MinA 

############ Date of First Frost #############
### Tire ###

Tt <- data %>%
  group_by(sheet) %>%               # group by location
  filter(Tire < 0) %>%               # filter to temperatures below frost temperature
  dplyr::summarise(FFrostT = min(DateTime)) # Find the earliest date 

#merge with summary DF
hatch <- merge(hatch, Tt,"sheet", all.x = TRUE)

### AIR ###

Tt <- data %>% 
  group_by(sheet) %>%               # group by location
  filter(Air_Temp < 0) %>%            # filter to temperatures below frost temperature
  dplyr::summarise(FFrostA = min(DateTime)) # Find the earliest date 

#merge with summary DF
hatch <- merge(hatch, Tt,"sheet", all.x =  TRUE)


############ Number of Days below -12 ############
### TIRE ###
#'
#' Given that lab experiments have found that there is a critical 
#' threshold at -12C where that eggs die.
#' 

data$Date <- date(data$DateTime)
Tt <- data %>%
  group_by(sheet) %>%
  filter(Tire < -11.5) %>%
  dplyr::summarise( DaysB12T = n_distinct(Date))

#merge with summary DF
hatch <- merge(hatch, Tt,"sheet", all.x =T)

### AIR ###

Tt <- data %>%
  group_by(sheet) %>%
  filter(Air_Temp < -11.5) %>%
  dplyr::summarise(DaysB12A = n_distinct(Date))

#merge with summary DF
hatch <- merge(hatch, Tt,"sheet", all.x = T)

# correct format
hatch$DaysB12T <- as.numeric(hatch$DaysB12T)
hatch$DaysB12A <- as.numeric(hatch$DaysB12A)



############ Max Consecutive hours below -12 #######
### Tire ###


# empty DF
ColdDays <- data.frame("sheet"= rep(NA,13), "MAXHrsB12conT" = rep(NA,13))
for (x in list(levels(data$sheet)) {
  Tt <- data %>%
    filter (sheet == x)                        # filter by site sheet
  Tt$below0 <- ifelse(Tt$Tire < -12, TRUE, FALSE) # create bool. variable
  r <- rle(Tt$below0)                           # tally T/F in a row 
  rBelow <- r$lengths[c(FALSE,TRUE)]            # extract only T in a row
  MaxBelow <-round(max(rBelow), 0)              # convert to days, round
  ColdDays[x,] <- c(x, MaxBelow)                # add to DF
}

#merge with summary DF
hatch <- merge(hatch, ColdDays,"sheet", all.x = TRUE)

### Air ###

# empty DF
ColdDays <- data.frame("sheet"= rep(NA,13), "MAXHrsB12conA" = rep(NA,13))
for (x in list(levels(data$sheet)) {
  Tt <- data %>%
    filter (sheet == x)  %>%                   # filter by site sheet
    drop_na(Air_Temp)                           # remove na values  
  Tt$below0 <- ifelse(Tt$Air_Temp < -12, TRUE, FALSE) # create bool. variable
  r <- rle(Tt$below0)                           # tally T/F in a row 
  rBelow <- r$lengths[c(FALSE,TRUE)]            # extract only T in a row
  MaxBelow <-round(max(rBelow), 0)              # convert 3 hour blocks to hours, round
  ColdDays[x,] <- c(x, MaxBelow)                # add to DF
}

#merge with summary DF
hatch <- merge(hatch, ColdDays,"sheet", all.x = T)



############ Freze- Thaw Events #############
### Tire ###

# empty DF
FreezeThaw <- data.frame("sheet"= rep(NA,13), "FreThaT" = rep(NA,13))
for (x in sheets) {
  Tt <- data %>%
    filter (sheet == x)                        # filter by site sheet
  Tt$below0 <- ifelse(Tt$Tire < 0, TRUE, FALSE) # create bool. variable
  r <- rle(Tt$below0)                           # tally T/F in a row 
  events <- as.numeric(length(r$lengths))
  FreezeThaw[x,] <- c(x, events)
}

#merge with summary DF
hatch <- merge(hatch, FreezeThaw,"sheet", all.x = T)


### AIR ###

# empty DF
FreezeThaw <- data.frame("sheet"= rep(NA,13), "FreThaA" = rep(NA,13))
for (x in list(levels(data$sheet)) {
  Tt <- data %>%
    filter (sheet == x)                        # filter by site sheet
  Tt$below0 <- ifelse(Tt$Air_Temp < 0, TRUE, FALSE) # create bool. variable
  r <- rle(Tt$below0)                           # tally T/F in a row 
  events <- as.numeric(length(r$lengths))
  FreezeThaw[x,] <- c(x, events)
}
#merge with summary DF
hatch <- merge(hatch, FreezeThaw,"sheet", all.x = T)


############ Growing Degrees Days ########

## winter GDDs ###
DJF <- filter(data, Date > "2021-11-30", Date < "2022-03-01")  # filter data set to DJF
sheets <- levels(DJF$sheet) # store sheets for list 
## For loop to caluate each tires cummulative GDD (Tbase == 10)
for (i in sheets) {
  DJF$GDD_10_DJF[DJF$sheet == i] <- GDD(DJF$Tire[DJF$sheet == i], summ = T, Tbase = 10)
  hatch$GDD_10_DJF[hatch$sheet == i] <-  max(DJF$GDD_10_DJF[DJF$sheet == i])
}
## For loop to caluate each tires cummulative GDD (Tbase == -12)
for (i in sheets) {
  DJF$GDD_n12_DJF[DJF$sheet == i] <- GDD(DJF$Tire[DJF$sheet == i], summ = T, Tbase = -12)
  hatch$GDD_n12_DJF[hatch$sheet == i] <-  max(DJF$GDD_n12_DJF[DJF$sheet == i])
}

# Merge DJF into large hourly data df

# make smaller data frame to merge to data
DJF.s <- data.frame("DateTime"    = DJF$DateTime,
                    "sheet"      = DJF$sheet,
                    "GDD_10_DJF"  = DJF$GDD_10_DJF, 
                    "GDD_n12_DJF" = DJF$GDD_n12_DJF)
data <- merge(data, DJF.s, by = c("DateTime", "sheet"), all.x = T)


### Full GDDs ###
## For loop to caluate each tires cummulative GDD (Tbase == -12)
for (i in sheets) {
  data$GDD_10_FULL[data$sheet == i] <- GDD(data$Tire[data$sheet == i], summ = T, Tbase = 10)
  hatch$GDD_10_FULL[hatch$sheet == i] <-  max(data$GDD_10_FULL[data$sheet == i])
}
## For loop to caluate each tires cummulative GDD (Tbase == -12)
for (i in sheets) {
  data$GDD_n12_FULL[data$sheet == i] <- GDD(data$Tire[data$sheet == i], summ = T, Tbase = -12)
  hatch$GDD_n12_FULL[hatch$sheet == i] <-  max(data$GDD_n12_FULL[data$sheet == i])
}


############ Saving files ###############

# write.csv(data, "00_Data/21.22_temperature_NO.Arl3.csv")
# write.csv(hatch, "00_Data/21.22_hatch_temperature_summary_N).Arl3.csv")


