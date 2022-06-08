#' ---
#' title: "01b Arl Tire 3 temperature format"
#' author: "Katie Susong"
#' date: "18 May 2022"
#' ---

#' Overview
#' ========
#' 
#' The site in Arl contianed a 3rd tire that housed 6 egg sheets. starting in NOV. 
#' every ~21 days a single egg sheet was pulled from the site and stored at CTR 
#' conditons. A sinle temperature monitor was placed within the tire and withthe CTR
#' so the two temperatures must be spliced togauther on on the day of removal for each tire. 
#' 


# libraries
library(dplyr)
library(lubridate)

#### Import Data set ####
rm (list  = ls())
# hourly temperature data for all sites
data           <- read.csv("00_Data/21.22_temperature.csv")
data$number    <- as.factor(data$number)
data$DateTime  <- ymd_hms(data$DateTime)


#### create Arl_3 and CTR small dataframes
Arl3 <- filter(data, number == "7")
CTR  <- filter(data, number == "0")


#' Each of the 6 egg sheets in the Arl_3 tire will need a different temp. based on the pull date 
#' PULL DATES: 
#'    Sheet #     Pull date
#'    -------    -------------
#'    - B.12       30 NOV 21
#'    - C.1        21 DEC 21
#'    - A.9        11 JAN 22
#'    - C.6        01 FEB 22
#'    - B.4        21 FEB 22
#'    - A.5        18 MAR 22
#'    


#' The plan: 
#' 
#'    1.  create 2 tem dfs with that end and begin at the pull date
#'    2.  merge and save as as a new df
#'    3.  create lable to denote sheet and pull date 
#'    4.  repeat with other sheets 
#'    5.  merge all sheet dfs 
#'    6.  save 
#'    

#### PULL 1: 30 NOV 21 ####
# generate temp. df
temp.b <- filter (Arl3, DateTime < "2021-11-30 12:00:00" )  # days before the sheet was pulled
temp.a <- filter (CTR, DateTime >  "2021-11-30 12:00:00" )  # days after the sheet was pulled
# merge 2 temp.dfs 
pull1  <- rbind(temp.b,temp.a)
# add pull date and sheet number 
pull1$pull.date <- ymd("2021-11-30")
pull1$sheet     <- "B.12"

#### PULL 2: 21 DEC 21 ####
# generate temp. df
temp.b <- filter (Arl3, DateTime < "2021-12-21 12:00:00" )  # days before the sheet was pulled
temp.a <- filter (CTR, DateTime >  "2021-12-21 12:00:00" )  # days after the sheet was pulled
# merge 2 temp.dfs 
pull2  <- rbind(temp.b,temp.a)
# add pull date and sheet number 
pull2$pull.date <- ymd("2021-12-21")
pull2$sheet     <- "C.1"

#### PULL 3: 11 JAN 22 ####
# generate temp. df
temp.b <- filter (Arl3, DateTime < "2022-01-11 12:00:00" )  # days before the sheet was pulled
temp.a <- filter (CTR, DateTime >  "2022-01-11 12:00:00" )  # days after the sheet was pulled
# merge 2 temp.dfs 
pull3  <- rbind(temp.b,temp.a)
# add pull date and sheet number 
pull3$pull.date <- ymd("2022-01-11")
pull3$sheet     <- "A.9"

#### PULL 4:  01 FEB 22 ####
# generate temp. df
temp.b <- filter (Arl3, DateTime < "2022-02-01 12:00:00" )  # days before the sheet was pulled
temp.a <- filter (CTR, DateTime >  "2022-02-01 12:00:00" )  # days after the sheet was pulled
# merge 2 temp.dfs 
pull4  <- rbind(temp.b,temp.a)
# add pull date and sheet number 
pull4$pull.date <- ymd("2022-02-01")
pull4$sheet     <- "C.6"

#### PULL 5:  21 FEB 22 ####
# generate temp. df
temp.b <- filter (Arl3, DateTime < "2022-02-21 12:00:00" )  # days before the sheet was pulled
temp.a <- filter (CTR, DateTime >  "2022-02-21 12:00:00" )  # days after the sheet was pulled
# merge 2 temp.dfs 
pull5  <- rbind(temp.b,temp.a)
# add pull date and sheet number 
pull5$pull.date <- ymd("2022-02-21")
pull5$sheet     <- "B.4"

#### PULL 6:  18 MAR 22 ####
# generate temp. df
temp.b <- filter (Arl3, DateTime < "2022-03-18 12:00:00" )  # days before the sheet was pulled
temp.a <- filter (CTR, DateTime >  "2022-03-18 12:00:00" )  # days after the sheet was pulled
# merge 2 temp.dfs 
pull6  <- rbind(temp.b,temp.a)
# add pull date and sheet number 
pull6$pull.date <- ymd("2022-03-18")
pull6$sheet     <- "A.5"


#### Merge all pull dfs####

Arl3_pull <- rbind(pull1, pull2, pull3, pull4, pull5, pull6)

# save data
# write.csv(Arl3_pull, "00_Data/21.22_temperature_arl3.csv")



#