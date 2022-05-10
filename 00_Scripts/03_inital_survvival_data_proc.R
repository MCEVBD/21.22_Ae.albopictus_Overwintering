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
#'     - avg.egg
#'     - total.live
#'     - total.dead
#'     - total.hatch
#'     - pro.live
#'     - per.sur.live
#'     - rel.per.sur.live
#'     - per.red.live
#'     
#' visulizations : 
#'      -
#'      -
#'      -


#libraries
library(readr)
library(ggplot2)
library(dplyr)

######### Import data #######

data <- read_csv("00_Data/21.22_section_survival_data.csv",
                 col_types = cols(dead.hatch1 = col_integer(),dead.hatch2 = col_integer(), dead.hatch3 = col_integer(), dead.hatch4 = col_integer(),
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

############## Calculate new varaibles ############

## Avg. egg number (per section)

data$egg1  <- as.numeric(data$egg1)  # fix str
data$egg1b <- as.numeric(data$egg1b)

data$avg.egg <- signif((data$egg1 + data$egg1b) / 2, digits = 3) # signif() rounds to # of digits

## Surviavl/ hatch variables

# total live hatch
data$total.live <-  data$live.hatch1 + data$live.hatch2 + data$live.hatch3 + data$live.hatch4
# total dead hatch
data$total.dead <-  data$dead.hatch1 + data$dead.hatch2 + data$dead.hatch3 + data$dead.hatch4
# total hatch 
data$total.hatch <- data$total.live + data$total.dead
# proportion of hatch live
data$pro.live <- data$total.live/data$total.hatch

## Percent survival varaibles 

#percent.survival
data$per.sur.live <- data$total.live /  data$avg.egg

# relative percent survival 
sheet <- c("A", "B", "C")  # list of sheet names 
data$rel.per.sur.live <- NA     # emepty varaible 
for (i in sheet) {
  data$rel.per.sur.live[data$sheet == i] <- data$per.sur[data$sheet == i] / data$per.sur.live[data$sheet == i  & data$loc.id == "CTR"]
}

# percent reduction in survial
data$per.red.live <- NA
for (i in sheet) {
  data$per.red.live[data$sheet == i] 
}
########### write csv #########

#write.csv(data, "00_Data/21.22_section_survival_data.csv")

########### Plots ############
# for all plots tire 7 (Atil_WI_T3) will be excluded as those egg sheets where part of a staggerd collection

## proportion live to dead hatch by site
data %>%
  filter(number != 7) %>%
  ggplot(aes(site, pro.live)) +
  geom_boxplot()

## percent survial by site
data %>%
  filter(number != 7) %>%
  ggplot(aes(site, per.sur.live)) +
  geom_boxplot()

## relative percent survial by site
data %>%
  filter(number != 7) %>%
  ggplot(aes(site, rel.per.sur.live)) +
  geom_boxplot()

## relative percent survial by lat
data %>%
  filter(number != 7) %>%
  ggplot(aes( x, rel.per.sur.live)) +
  geom_point()

