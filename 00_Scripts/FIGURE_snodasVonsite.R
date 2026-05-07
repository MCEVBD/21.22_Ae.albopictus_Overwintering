#' ---
#' title: FIGURE: snow V snodas
#' author: Katie M Susong
#' date: 09-Jan-2023
#' ---
#'
#'#' Overview
#' ========
#' 
#' Figure - Snow v snodas
#' goal: show that snow and snodas trend similarly
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

#FIG

fig <-  ggplot(data) +
  geom_line(aes(Date, snodas, col = location ), size = 1, linetype = 1) +
  geom_line(aes(Date, depth_G, col = location), size = 1, linetype = 2) +
  facet_grid(vars(location)) +
  theme_minimal() + 
  ylab("Snow depth (mm)") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12,face="bold")) +
   theme(legend.position = "none")


ggplot(data) +
  geom_line(aes(Date, snodas, col = location ), size = 1, linetype = 1) +
  geom_line(aes(Date, depth_G, col = location), size = 1, linetype = 2) +
  facet_grid(vars(location)) +
  theme_cowplot() + 
  ylab("Snow depth (mm)") +
  theme(legend.position = "none")


# save high res. version of plot
#setwd("~/Documents/CBS_PhD/Ae.albo_OW_2021/03_figures_presentation/Figures")
#ggsave("snodas_snow_time.pdf", plot = fig, width = 10, height = 5, units = "in", dpi = 500)

setwd("~/Documents/CBS_PhD/Ae.albo_OW_2021/02_Manuscript/Figures")
ggsave("snodas_snow_time.pdf", plot = fig, width = 8, height = 5, units = "in", dpi = 500)


#FIG - Point

 fig2 <- ggplot(data, aes(snodas, depth_G, col = location))+
  geom_point() +
  geom_abline( slope = 1, linetype = 4) +
  geom_smooth(method = "lm", col = "black", linetype = 2 ) +
  theme_linedraw() +
  xlab("SNODAS, Snow depth (mm)") +
  ylab("On-Site, Snow depth (mm)") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  ylim(0,400) +
   xlim(0,410) +
  labs(color = "Location") +
  theme(legend.position = c(0.1,0.76),legend.text=element_text(size=12),
         legend.title=element_text(size=12,face= "bold"))


# save high res. version of plot
setwd("~/Documents/CBS_PhD/Ae.albo_OW_2021/03_figures_presentation/Figures")
ggsave("snodas_snow.pdf", plot = fig2, width = 10, height = 5, units = "in", dpi = 500)
