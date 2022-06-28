# libraries
library(dplyr)
library(tidyr)
library(lubridate)
library(chillR)
library(ggplot2)

#### import data ####
rm (list  = ls())
#temperature data
data  <- read.csv("00_Data/21.22_temperature.csv")
# format correction
data$DateTime <- ymd_hms(data$DateTime)
data$number   <- as.factor(data$number)
data$Month    <- month(data$DateTime)
data$Month    <- as.factor(data$Month)

############  Wind #########

#calcaute average monthly wind speed
ggplot(data, aes(location,wspd)) +
  geom_boxplot() +
  facet_wrap(~Month)

ggplot(data, aes(DateTime, wspd, col = location))+
  geom_line()+
  facet_wrap(~location)

avg.wspd <- data %>%
  group_by(number,Month) %>%
  dplyr::summarise(mon.avg.wspd = mean(wspd,na.rm = TRUE), 
                   std.ma.wspd = (sd(wspd, na.rm = T)))


###### Solar ####

ggplot(data, aes(location,Solar)) +
  geom_boxplot() +
  facet_wrap(~Month)

ggplot(data, aes(DateTime, Solar, col = location))+
  geom_line()+
  facet_wrap(~location)

plot(data$DateTime[data$location == "Spo"], (data$Solar[data$location == "Spo"]), type = "line")
