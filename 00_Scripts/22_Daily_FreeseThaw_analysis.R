#' ---
#' title: "22 Daily Freese-Thaw- Analysis"
#' author: "Katie M Susong"
#' date: "22-June-2022"
#' ---


#' Overview
#' =========
#' 
#' Cover calaculated via game camera images is used to determine the min about of 
#' snow needed to fully cover tires in open spacees and if cover significantly 
#' contributes to tire insulation 
#' 

rm (list  = ls())
# Libraries
library(dplyr)
library(lme4)
library(lubridate)
library(sjPlot)

#### Functions ####
#' **Overdispersal Function**
#' 
#'  A not signficant p-value indicates that the ratio is not different from 0 and that 
#'  the function is not overdispersed. 
#'   
overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}
#### Import Data ####

setwd("~/Documents/CBS_PhD/Ae.albo_OW_2021/21.22_Ae.albopictus_Overwintering")
data <- read.csv('00_Data/21.22_snow_temperature.csv')
#format corrections
data$number   <- as.integer (data$number)
data$Date     <- ymd (data$Date)
data$location <- factor (data$location, levels = c("Car", "Bon", "Dek", "Arl", "Han", "Spo"))
data          <- filter (data, Date < "2022-04-14" )
data$cover    <- factor(data$cover, levels = c("none", "side", "top", "both"))
data$snow_FAC10 <- cut(data$snodas, breaks = c(-1, 0,100,200,300,400), labels = c("none", ">100", ">200", ">300", ">400"))


# calculate daily freese-thaw 

## TIRE ##
# empty DF
FreezeThawT <- data.frame("number"= rep(NA,13), "FreThaT" = rep(NA,13))
num <- c(1:13)
for (x in num) {
  Tt <- data %>%
    filter (number == x)                        # filter by site number
  Tt$below0 <- ifelse(Tt$MeanT_Tire < 0, TRUE, FALSE) # create bool. variable
  r <- rle(Tt$below0)                           # tally T/F in a row 
  events <- as.numeric(length(r$lengths))
  FreezeThawT[x,] <- c(x, events)
} 

FreezeThawT[6,] <- c(6, NA)
# correct num = 6 to NA
### AIR ###

# empty DF
FreezeThaw <- data.frame("number"= rep(NA,13), "FreThaA" = rep(NA,13))
for (x in 1:13) {
  Tt <- data %>%
    filter (number == x)                        # filter by site number
  Tt$below0 <- ifelse(Tt$MeanT_Air < 0, TRUE, FALSE) # create bool. variable
  r <- rle(Tt$below0)                           # tally T/F in a row 
  events <- as.numeric(length(r$lengths))
  FreezeThaw[x,] <- c(x, events)
}
#merge with summary DF
FreezeThaw <- merge(FreezeThawT, FreezeThaw,"number", all.x = T)
FreezeThaw$diff <- FreezeThaw$FreThaA - FreezeThaw$FreThaT

#write.csv(FreezeThaw, "00_Data/21.22_dailyFTevents.csv")
