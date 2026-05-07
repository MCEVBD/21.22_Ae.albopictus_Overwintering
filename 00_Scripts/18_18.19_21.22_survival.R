#' ---
#' title: "18 Combine 18/19 + 21/22 survival Analysis"
#' author: "Katie M Susong"
#' date: "14-June-22"
#' ---




#libraries
library(dplyr)
#### Import Data ####
rm (list  = ls())
setwd("~/Documents/CBS_PhD/Ae.albo_OW_2021/21.22_Ae.albopictus_Overwintering")
s21.22 <- read.csv("00_Data/21.22_hatch_temperature_summary_NO.Arl3.csv")
# format corrections
s21.22$number <- as.factor(s21.22$number)
s21.22 <- subset(s21.22, select = -c(X.1, X))
s21.22 <- dplyr::rename(s21.22, Location =site)
s21.22$year <- "2022"

#18/19 data
setwd("~/Documents/CBS_PhD")
s18.19 <- read.csv( "albopictus_OW/Data/OW_SUR_site_20220113.csv")
s18.19 <- dplyr::rename(s18.19, per.sur.live =PerSur)
s18.19 <- dplyr::rename(s18.19, MAXHrsB12conT =HrsB12conT)
s18.19 <- dplyr::rename(s18.19, MAXHrsB12conA =HrsB12conA)
s18.19$year <- "2019"

# merge data 
all <- bind_rows(s18.19,s21.22)


# plots
hist(all$per.sur.live)
