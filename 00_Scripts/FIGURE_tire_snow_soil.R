#' ---
#' title: FIGURE: tire, snow, soil diff
#' author: Katie M Susong
#' date: 08-April-2024
#' ---
#'
#' Overview
#' ========
#' 


#libraries

library(ggplot2)
library(lubridate)
library(dplyr)
library(tidyr)
library(cowplot)
library(forcats)

#### Import ####
setwd("~/Documents/CBS_PhD/Ae.albo_OW_2021/21.22_Ae.albopictus_Overwintering")

## hourly data ( no snow )
data <- read.csv('00_Data/21.22_temperature.csv')
#format corrections
data$number   <- as.factor (data$number)
data$Date     <- ymd_hms (data$Date)
data$location <- factor (data$location, levels = c("Car", "Bon", "Dek", "Arl", "Han", "Spo"))
# filters
data          <- filter (data, Date < "2022-04-14" ) # removal all recordings after field removal
data          <- filter (data, Soil != "NA") # limit to the tire at each site with an assciated soil measurment. 

## Daily data with Snow
dia <- read.csv('00_Data/21.22_snow_temperature.csv')
#format corrections
dia$number   <- as.factor (dia$number)
dia$Date     <- ymd (dia$Date)
dia$location <- factor (dia$location, levels = c("Car", "Bon", "Dek", "Arl", "Han", "Spo"))
dia          <- filter (dia, Date < "2022-04-14" )
dia$cover    <- factor (dia$cover, levels = c("none", "side", "top", "both"))

# add factor (100mm bin) snodas variable for analysis
dia$snow_FAC10 <- cut(dia$snodas, breaks = c(-1, 0,100,200,300,400), labels = c("none", ">100", ">200", ">300", ">400"))

Supp_fig3 <- dia %>%
  pivot_longer(cols = starts_with("MeanT"), 
               names_to = "loc", 
               names_prefix = "MeanT_",
               values_to = "MeanT") %>%
  filter(loc != "Diff") %>%
  mutate(loc = fct_relevel(loc, "Soil", "Tire", "Air")) %>%
  ggplot(aes(Date, MeanT, col = loc)) +
  geom_smooth() + 
  facet_wrap(~location, nrow = 1) +
  theme_cowplot() +
  labs(x = "Time (Date)",
       y = "Temperature (??C)")
  
# save high res. version of plot
setwd("~/Documents/CBS_PhD/Ae.albo_OW_2021/03_figures_presentation/Figures")
ggsave("Supp_fig3.pdf", plot = Supp_fig3)


  


  
