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
#'         2.
#'     - Plots of Survival and Temperature
#'         1. JANmeanT and per.sur.live, col by site
#'         2. Survivial and GDD (10??C) during DJF
#'         3. Survivial and GDD (10??C) FULL
#'         4. Survivial and MinT
#'         5. Survivial and date of first Frost (Tire)
#'         6. Survivial and DaysB12T
#'         7. Survivial and MAX hrs below -12
#'         
#'       


# libraries
library(ggplot2)
library(lubridate)
# import data
hts <- read.csv("00_Data/21.22_hatch_temperature_summary_NO.Arl3.csv")
# format data
hts$number  <- as.factor(hts$number)
hts$site    <- factor(hts$site, levels = c("CTR", "Car", "Bon", "Dek", "Arl", "Han", "Spo"))
hts$FFrostT <- mdy_hm(hts$FFrostT) 


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
  geom_point()

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
  geom_boxplot() +
  geom_point()

#' **Plot 3:** Survivial and GDD (10??C) FULL
#' 
ggplot(hts, aes(GDD_10_FULL, per.sur.live, col = site)) +
  geom_boxplot()+
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



