#' ---
#' title: "18 Combine 18/19 + 21/22 survival Analysis"
#' author: "Katie M Susong"
#' date: "14-June-22"
#' ---



#libraries
library(dplyr)
library(ggplot2)
library(lme4)
library(sjPlot)
#### Import Data ####
rm (list  = ls())
setwd("~/Documents/CBS_PhD/Ae.albo_OW_2021/21.22_Ae.albopictus_Overwintering")
s21.22 <- read.csv("00_Data/21.22_hatch_temperature_summary_NO.Arl3.csv")
# format corrections
s21.22$number <- as.factor(s21.22$number)
s21.22 <- subset(s21.22, select = -c(X.1, X))
s21.22 <- dplyr::rename(s21.22, Location =site)
s21.22$year <- "2022"

s21.22$rel.per.sur.live  <- s21.22$per.sur.live/ 0.185


#18/19 data
setwd("~/Documents/CBS_PhD")
s18.19 <- read.csv( "albopictus_OW/Data/OW_SUR_site_20220113.csv")
s18.19 <- dplyr::rename(s18.19, per.sur.live =PerSur)
s18.19 <- dplyr::rename(s18.19, MAXHrsB12conT =HrsB12conT)
s18.19 <- dplyr::rename(s18.19, MAXHrsB12conA =HrsB12conA)

s18.19$rel.per.sur.live  <- s18.19$per.sur.live/ 0.128
s18.19$year <- "2019"

# merge data 
all <- bind_rows(s18.19,s21.22)


#### plots ####
hist(all$per.sur.live)
hist(all$rel.per.sur.live)

ggplot(all, aes(JANmeanT, JANmeanA, fill = per.sur.live, shape = year)) +
  geom_point() +
  ylim(-15,10) +
  xlim (-15,10)

ggplot(all,aes(JANmeanT, per.sur.live, shape = year))+
  geom_point() +
  xlim (-10,10)

ggplot(all,aes(JANmeanT, rel.per.sur.live, shape = year, color = JANmeanD))+
  geom_point(size = 4) +
  xlim (-10,10) +
  theme_classic() + 
  xlab( "Mean Daily Temperature, Internal (??C)") +
  ylab( "Relative Percent Survial") +
  scale_color_gradientn(name = "Difference from Mean Daily External Temperature (??C)", 
                        colors = c("grey", "black")) +
  theme(legend.position = "top")


#### Analysis ####

plot(all$JANmeanT,all$rel.per.sur.live)
plot(all$MinT,all$rel.per.sur.live)
plot(all$DaysB12T, all$rel.per.sur.live)
plot(all$MAXHrsB12conT, all$rel.per.sur.live)


mixed1 <- lmer(rel.per.sur.live ~ DaysB12T + (1|year) + (1|Location), data = all )
summary(mixed1)
tab_model(mixed1)


mixed2 <- lmer(rel.per.sur.live ~ MAXHrsB12conT + (1|year) + (1|Location), data = all )
summary(mixed2)
tab_model(mixed2)


mixed3 <- lmer(rel.per.sur.live ~ JANmeanT + (1|year) + (1|Location), data = all )
summary(mixed3)
tab_model(mixed3)


