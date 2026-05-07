#' ---
#' title: FIGURE: Time v Snow 
#' author: Katie M Susong
#' date: 09-Jan-2023
#' ---
#'
#'#' Overview
#' ========
#' 
#' Figure - Snow and time; by site
#' goal: show the significant difference between snow at the various sites through time 
#¢ 
#¢ Timeline 
#' ======= 
#' 
#' **09-Jan-2023**: inital figures made **KMS**
#' 

rm (list  = ls())

# Libraries
# data formatting
library(dplyr)
library(lubridate)
#ploting
library(ggplot2)
library(cowplot)
library(ggridges)


### import data ###
# set wd 
setwd("~/Documents/CBS_PhD/Ae.albo_OW_2021/21.22_Ae.albopictus_Overwintering")
data <- read.csv("00_Data/21.22_snow_temperature.csv")

# correct data str
data$Date <- ymd(data$Date)
data$location <- factor (data$location, levels = c("Spo","Han","Arl","Dek", "Bon", "Car" ))
# create new numeric location varaibles 
location.list <- levels(data$location)
for (i in c(1:6)) {
  data$loc.ID[data$location == location.list[i]] <- i 
}


## Fig 1: SNODAS + facet 

fig1 <-  data %>%
  filter(Date >= "2021-12-15", Date <= "2022-02-15") %>%
  ggplot(aes(Date, snodas, col = location))+
  geom_line(size = 1) +
  facet_grid(vars(location)) +
  theme_linedraw() +
  ylab("Snowdepth (mm), from SNODAS") +
  theme(axis.text=element_text(size=12),
       axis.title=element_text(size=14,face="bold")) +
  theme(legend.position = "none") +
  ### THIS ADDS THE GREY BOX FOR ###
  annotate("rect",  # Defines the shape (in this case rectangle)
           xmin = as.Date ("2022/01/01"), xmax = as.Date("2022/02/01"), # the stop and start of the box on x
           ymin =-Inf, ymax = Inf , # the y lims, set to inf and -inf so whole range is colored
           alpha = .5, # sets transparency 
           fill = "Grey70") # the color 

# save high res. version of plot
setwd("~/Documents/CBS_PhD/Ae.albo_OW_2021/03_figures_presentation/Figures")
ggsave("time_v_snow_facet.pdf", plot = fig1, width = 10, height = 5, units = "in", dpi = 500)

#Fig 2: SNODAS 

  fig2 <- data %>%
  filter(Date >= "2021-12-15", Date <= "2022-02-15") %>%
  ggplot(aes(Date, snodas, col = location))+
  geom_line(size = 1) +
  theme_linedraw() +
  ylab("Snowdepth (mm), from SNODAS") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  theme(legend.position = c(0.12,0.75),legend.text=element_text(size=12),
        legend.title=element_text(size=14,face= "bold")) +
  ### THIS ADDS THE GREY BOX FOR ###
  annotate("rect",  # Defines the shape (in this case rectangle)
           xmin = as.Date ("2022/01/01"), xmax = as.Date("2022/02/01"), # the stop and start of the box on x
           ymin =-Inf, ymax = Inf , # the y lims, set to inf and -inf so whole range is colored
           alpha = .5, # sets transparacy 
           fill = "Grey70") # the color 

# save high res. version of plot
setwd("~/Documents/CBS_PhD/Ae.albo_OW_2021/03_figures_presentation/Figures")
ggsave("time_v_snow.pdf", plot = fig2, width = 10, height = 5, units = "in", dpi = 500)

  

## Fig 2 : ridgeline
ggplot(data, aes(snodas, location)) +
  geom_density_ridges() + 
  theme_ridges() + 
  theme(legend.position = "none")

ggplot(data, aes(Date,loc.ID, height = snodas, fill = location)) +
  geom_ridgeline(alpha = 0.5) + 
  theme_ridges() +
  theme(legend.position = "none")



