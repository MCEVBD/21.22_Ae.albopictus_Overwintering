#' ---
#' title: FIGURE: snow and temp diff
#' author: Katie M Susong
#' date: 09-Jan-2023
#' ---
#'
#'#' Overview
#' ========
#' 
#' Figure - Snow and temerature; by site
#' goal: show the insulation provided by snow  
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


#FIG: mean diff and Snow 
## does not show insulation well
ggplot(data, aes(snodas, MeanT_Diff))+
  geom_point()



#FIG: tire and ex. temp
fig <- data %>%
  filter(Date >= "2021-12-15", Date <= "2022-02-15") %>%
  ggplot() +
  geom_line(aes(Date, MeanT_Air), linetype = 2) +
  geom_line(aes(Date, MeanT_Tire), linetype = 1) +
  facet_grid(vars(location)) +
  theme_linedraw() +
  ylab("Mean Temperature (??C)") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))


# save high res. version of plot
setwd("~/Documents/CBS_PhD/Ae.albo_OW_2021/03_figures_presentation/Figures")
ggsave("tire_ext_temp.pdf", plot = fig, width = 10, height = 5, units = "in", dpi = 500)
 
  
# Fig 2 : point tire v external temperature + snow depth 
fig2 <- data %>%
  filter(Date >= "2021-12-15", Date <= "2022-02-15") %>%
  ggplot(aes(MeanT_Air, MeanT_Tire, col = snodas, size = )) +
  geom_point() +
  labs(color = "Snow Depth (mm)") + 
  geom_abline( slope = 1, linetype = 2) +
  theme_linedraw() +
  xlab("Mean External Temperature (??C)") +
  ylab("Mean Tire Temperature (??C)") +
  ylim(-20,0) +
  xlim (-20,0) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  theme(legend.position = c(0.12,0.80),legend.text=element_text(size=12),
        legend.title=element_text(size=12,face= "bold"))
  
# save high res. version of plot
setwd("~/Documents/CBS_PhD/Ae.albo_OW_2021/03_figures_presentation/Figures")
ggsave("tire_ext_temp_point.pdf", plot = fig2, width = 10, height = 5, units = "in", dpi = 500)

