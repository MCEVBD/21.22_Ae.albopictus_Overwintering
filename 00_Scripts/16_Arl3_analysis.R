#' ---
#' title: "16 Arl3 survival"
#' author: "Katie M Susong"
#' date: "8-June-22"
#' ---

#' Overview
#' ========
#' 
#' At the Arl site a 3rd tire was plced and eggs were pulled every ~three weeks to see
#' when survival was negatively impacted. 
#' 
#' PULL DATES: 
#' 
#'    Section #     Pull date
#'    -------    -------------
#'    
#'    - B.12       30 NOV 21
#'    
#'    - C.1        21 DEC 21
#'    
#'    - A.9        11 JAN 22
#'    
#'    - C.6        01 FEB 22
#'    
#'    - B.4        21 FEB 22
#'    
#'    - A.5        18 MAR 22
#'    
#' Relative survival 
#' ----------------
#' 
#' Due to the limited nature of the study only a single section was pulled for each time 
#' period. To account for variantion between the sheets the sections were taken from a 
#' relative percent survival was calcualted. This is equal to survial at the site/suvival 
#' of the control.  
#' 
#' 


#libraries#
library(lubridate)
library(dplyr)
library(ggplot2)
library(knitr)
#### Import Data ####
rm (list  = ls())
# hourly eviromental data
setwd("~/Documents/CBS_PhD/Ae.albo_OW_2021/21.22_Ae.albopictus_Overwintering")
envi <- read.csv("00_Data/21.22_temperature_Arl3.csv")
#format
envi$number    <- as.factor(envi$number)
envi$DateTime  <- ymd_hms(envi$DateTime)
envi$pull.date <- ymd(envi$pull.date)
envi$Date <- date(envi$DateTime)
envi <- dplyr::rename(envi, id=sheet)
envi$id <- factor(envi$id, c("B.12", "C.1", "A.9", "C.6", "B.4", "A.5"))

# hatch data
hth <- read.csv("00_Data/21.22_hatch_temperature_summary.csv")
# format
hth$number <- as.factor(hth$number)
hth$FFrostA <- mdy_hm(hth$FFrostA)
hth$FFrostT <- mdy_hm(hth$FFrostT)
# filter to Arl3 only 
hth <- filter(hth, number == "7" | number == "0")
# add pull date 
hth$pull.date[hth$id == "B.12"] <- "2021-11-30"
hth$pull.date[hth$id == "C.01"] <- "2021-12-21"
hth$pull.date[hth$id == "A.07"] <- "2022-01-11"
hth$pull.date[hth$id == "C.06"] <- "2022-02-01"
hth$pull.date[hth$id == "B.04"] <- "2022-02-21"
hth$pull.date[hth$id == "A.05"] <- "2022-03-18"

hth$pull.date <- ymd(hth$pull.date)    


#### Create relative survial variable for survival ####

# calaculated by the survial at th site / survial of the control for the same sheet
hth$rel.per.sur.live[hth$number == "7" & hth$sheet == 'A'] <-  
  hth$per.sur.live[hth$number == "7" & hth$sheet == 'A'] / 
  hth$per.sur.live[hth$number == "0" & hth$sheet == 'A']

hth$rel.per.sur.live[hth$number == "7" & hth$sheet == 'B'] <- 
  hth$per.sur.live[hth$number == "7" & hth$sheet == 'B'] / 
  hth$per.sur.live[hth$number == "0" & hth$sheet == 'B']

hth$rel.per.sur.live[hth$number == "7" & hth$sheet == 'C'] <- 
  hth$per.sur.live[hth$number == "7" & hth$sheet == 'C'] / 
  hth$per.sur.live[hth$number == "0" & hth$sheet == 'C']
              
#### add realative survial to envi df ####

# is not maintianing all observation 
all<- merge(envi, hth[,c("id", "rel.per.sur.live")], by = "id" )

#### Plots ####

#### Plot temperature profile ###
ggplot(envi, aes(DateTime, Tire, col = id)) +
  geom_line() +
  facet_wrap(~id)

#### Plot pull date v survial ###

ggplot(hth, aes(pull.date, rel.per.sur.live))+
  geom_point()

#' From pull 1 to pull 3 relative survial decreased from ~1.5 to 0. consequently these
#' three time period will be studied to understand the threshold reached that ended
#'  survival 
#' 

#### extract key time periods ####


{key <- envi %>%
  filter( id == "A.9")

p1 <- (filter(key, DateTime < "2021-11-30 12:00:00"))
p2 <- (filter(key, DateTime > "2021-11-30 12:00:00" & DateTime < "2021-12-21 12:00:00"))
p3 <- (filter(key, DateTime > "2021-12-21 12:00:00" & DateTime < "2022-01-11 12:00:00"))

p1$condition <- 1
p2$condition <- 2
p3$condition <- 3

key <- rbind(p1,p2,p3)}
key$condition <- as.numeric(key$condition)
key$conditionF <- as.factor(key$condition)
#### Intoductory plots ####

ggplot(key, aes(condition,Tire)) +
  geom_boxplot()

ggplot(key, aes(DateTime, Tire, col = condition))+
  geom_line()

ggplot(key, aes(DateTime, Air_Temp))+
  geom_line(col = "red")+
  geom_line(aes(DateTime, Tire), col = "blue") +
  facet_grid( rows = vars(condition))+
  ylab("Temperature")

#### create summary values for each pull ####

## Max, min and mean TIRE and AIR temperature ##
summary <-key %>%
  group_by(condition) %>%
  dplyr::summarise(TIREmaxT = max(Tire),AIRmaxT = max(Air_Temp),
                   TIREminT = min(Tire),AIRminT = min(Air_Temp),
                   TIREmeanT = mean(Tire), AIRmeanT = mean(Air_Temp))

## Days with temperatures below -12C ##
 S2 <- key %>%
  group_by(condition) %>%
  filter(Tire < -11.5) %>%
  dplyr::summarise( DaysB12T = n_distinct(Date))

## Hours below -12C ##
# empty DF
TIREn12 <- data.frame("condition"= rep(NA,3), "MAXHrsB12conT" = rep(NA,3))
for (x in 1:3) {
  Tt <- key %>%
    filter (condition == x)                        # filter by site number
  Tt$below0 <- ifelse(Tt$Tire < -12, TRUE, FALSE) # create bool. variable
  r <- rle(Tt$below0)                           # tally T/F in a row 
  rBelow <- r$lengths[c(FALSE,TRUE)]            # extract only T in a row
  MaxBelow <-round(max(rBelow), 0)              # convert to days, round
  TIREn12[x,] <- c(x, MaxBelow)                # add to DF
}

# empty DF
AIRn12 <- data.frame("condition"= rep(NA,3), "MAXHrsB12conA" = rep(NA,3))
for (x in 1:3) {
  Tt <- key %>%
    filter (condition == x)  %>%                   # filter by site number
    drop_na(Air_Temp)                           # remove na values  
  Tt$below0 <- ifelse(Tt$Air_Temp < -12, TRUE, FALSE) # create bool. variable
  r <- rle(Tt$below0)                           # tally T/F in a row 
  rBelow <- r$lengths[c(FALSE,TRUE)]            # extract only T in a row
  MaxBelow <-round(max(rBelow), 0)              # convert 3 hour blocks to hours, round
  AIRn12[x,] <- c(x, MaxBelow)                # add to DF
}

## Freeze thaw events

TIREFreezeThaw <- data.frame("condition"= rep(NA,3), "FreThaT" = rep(NA,3))
for (x in 1:3) {
  Tt <- key %>%
    filter (condition == x)                        # filter by site number
  Tt$below0 <- ifelse(Tt$Tire < 0, TRUE, FALSE) # create bool. variable
  r <- rle(Tt$below0)                           # tally T/F in a row 
  events <- as.numeric(length(r$lengths))
  TIREFreezeThaw[x,] <- c(x, events)
}

### AIR ###

# empty DF
AIRFreezeThaw <- data.frame("condition"= rep(NA,3), "FreThaA" = rep(NA,3))
for (x in 1:3) {
  Tt <- key %>%
    filter (condition == x)                        # filter by site number
  Tt$below0 <- ifelse(Tt$Air_Temp < 0, TRUE, FALSE) # create bool. variable
  r <- rle(Tt$below0)                           # tally T/F in a row 
  events <- as.numeric(length(r$lengths))
  AIRFreezeThaw[x,] <- c(x, events)
}
# merge dfs
# broken summary <- merge(summary, S2, TIREn12, AIRn12, TIREFreezeThaw, AIRFreezeThaw, by.y= condition)

ggplot(key, aes(Tire, Air_Temp, col = conditionF))+
  geom_point()

ggplot(key, aes(Diff, Air_Temp, col = conditionF))+
  geom_point() +
  geom_vline(xintercept = 0)

ggplot(key, aes(Diff, Tire, col = conditionF))+
  geom_point()+
  geom_vline(xintercept = 0)

key %>%
  filter(condition == "3") %>%
  filter(Tire>14.5 & Tire<15.5)

key %>%
  filter(Diff> 10)
  
  
