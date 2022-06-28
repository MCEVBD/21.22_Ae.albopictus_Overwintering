#' ---
#' title: "06 Temperature and survival intial investigation"
#' author: "Katie M Susong"
#' date: "10-May-2022"
#' ---


#' Temperaure and Survival inital Investigation 
#' ============================================
#' 
#'  Investigation plots of temperature related to survival 
#'  
#'     - Boxplot of survial at sites
#'         1. percent survival, ordered N -> S 
#'         2. percent survivial, ordered lowest JanmeanT -> highest JanmeanT 
#'     - Plots of Temperature by site
#'         1. JANMeanT byt site (ordered N -> S)
#'     - Plots of Survival and Temperature
#'         1. JANmeanT and per.sur.live, col by site
#'         2. Survivial and GDD (10??C) during DJF
#'         3. Survivial and GDD (10??C) FULL
#'         4. Survivial and MinT
#'         5. Survivial and date of first Frost (Tire)
#'         6. Survivial and DaysB12T
#'         7. Survivial and MAX hrs below -12
#'         8. Survival and JANTempD
#'       


#### libraries ####
library(dplyr)
library(ggplot2)
library(lubridate)
library(viridis)

rm (list  = ls())
setwd("~/Documents/CBS_PhD/Ae.albo_OW_2021/21.22_Ae.albopictus_Overwintering")
#### import data ####
hts <- read.csv("00_Data/21.22_hatch_temperature_summary_NO.Arl3.csv")
# format data
hts$number  <- as.factor(hts$number)
hts$site    <- factor(hts$site, levels = c("CTR", "Car", "Bon", "Dek", "Arl", "Han", "Spo"))
hts$FFrostT <- ymd_hms(hts$FFrostT)
# fix "inf" values to NA
hts$MinT[hts$number == '6'] <- NA
hts$MinD[hts$number == '6'] <- NA
###### Add survial y/n variable to hts #########

hts$sur[hts$per.sur.live != 0] <- "Survival"
hts$sur[hts$per.sur.live == 0] <- "None"
hts$sur <- as.factor(hts$sur)

######### Summary Values #####

summary <- hts %>%
    group_by(site) %>%
    dplyr::summarise( per.sur.live = mean(per.sur.live),
                      JANmeanT = mean(JANmeanT, na.rm = T),
                      JANmeanA = mean(JANmeanA),
                      MinT = mean(MinT, na.rm = T),
                      MinA = mean(MinA, na.rm = T),
                      JANmeanD = mean(JANmeanD,na.rm = T),
                      MinD = mean(MinD, na.rm=T),
                      DaysB12T = mean(DaysB12T,na.rm = T),
                      DaysB12A = mean(DaysB12A),
                      hrB12Tcon = mean(MAXHrsB12conT,na.rm = T),
                      hrB12Acon = mean(MAXHrsB12conA),
                      FreThaA = mean(FreThaA),
                      FreThaT = mean(FreThaT,na.rm = T))

sur.summary <- hts %>%
  filter(site != "NA") %>%
  group_by(sur) %>%
  dplyr::summarise( per.sur.live = mean(per.sur.live),
                    JANmeanT = mean(JANmeanT, na.rm = T),
                    JANmeanA = mean(JANmeanA),
                    MinT = mean(MinT, na.rm = T),
                    MinA = mean(MinA, na.rm = T),
                    JANmeanD = mean(JANmeanD,na.rm = T),
                    DaysB12T = mean(DaysB12T,na.rm = T),
                    DaysB12A = mean(DaysB12A),
                    hrB12Tcon = mean(MAXHrsB12conT,na.rm = T),
                    hrB12Acon = mean(MAXHrsB12conA),
                    FreThaA = mean(FreThaA),
                    FreThaT = mean(FreThaT,na.rm = T))

# add survival T/F
summary$sur <- NA
summary$sur[summary$per.sur.live != 0] <- "Survival"
summary$sur[summary$per.sur.live == 0] <- "None"

# write summary table:
# write.csv(summary, "00_Data/21.22_bysite.hatch_temperature_summary_NO.Arl3.csv")
# write.csv(sur.summary, "00_Data/21.22_bysurvial.hatch_temperature_summary_NO.Arl3.csv")

############# PLOTS ###########

#' Boxplot of survial at sites
#'==============================
#'
#' **Plot 1:** percent survival, ordered N -> S  
#' 
ggplot(hts, aes(site, per.sur.live))+
  geom_boxplot() 

#' **Plot 2:** percent survivial, ordered lowest JanmeanT -> highest JanmeanT 
#' 
ggplot(hts, aes(reorder(site, JANmeanT), per.sur.live))+
  geom_boxplot()


#' Plots of Temperature by site
#' ============================
#' 

#' **Plot 1:** JANMeanT byt site (ordered N -> S)
#'
ggplot(hts, aes(site, JANmeanT))+
  geom_boxplot()

#' Plots of Survival and Temperature
#' ===================================
#'

#' **Plot 1:** JANmeanT and per.sur.live, col by site
#' 
ggplot(hts, aes( JANmeanT,per.sur.live, col = site)) +
  geom_point()

#' **Plot 2:** Survivial and GDD (10??C) during DJF
#' 
ggplot(hts, aes(GDD_10_DJF, per.sur.live, col = site)) +
  geom_point()

#' **Plot 3:** Survivial and GDD (10??C) FULL
#' 
ggplot(hts, aes(GDD_10_FULL, per.sur.live, col = site)) +
  geom_point()

#' **Plot 4:** Survivial and MinT
#' 
ggplot(hts, aes( MinT,per.sur.live, col = site)) +
  geom_point()

#' **Plot 5:** Survivial and date of first Frost (Tire)
#' 
ggplot(hts, aes( FFrostT,per.sur.live, col = site)) +
  geom_point()

#' **Plot 6:** Survivial and DaysB12T
#' 
ggplot(hts, aes( DaysB12T,per.sur.live, col = site)) +
  geom_point()

#' **Plot 7:** Survivial and MAX hrs below -12
#' 
ggplot(hts, aes( MAXHrsB12conT,per.sur.live, col = site)) +
  geom_point()

#' **Plot 8:** Differance in mean JAN temp and survival 
#' 
ggplot(hts, aes( JANmeanD,per.sur.live, col = site)) +
  geom_point()