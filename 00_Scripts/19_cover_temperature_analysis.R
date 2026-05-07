#' ---
#' title: "19 Snow, Cover, and Temperature- Analysis"
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
data$number   <- as.factor (data$number)
data$Date     <- ymd (data$Date)
data$location <- factor (data$location, levels = c("Car", "Bon", "Dek", "Arl", "Han", "Spo"))
data          <- filter (data, Date < "2022-04-14" )
data$cover    <- factor(data$cover, levels = c("none", "side", "top", "both"))
data$snow_FAC10 <- cut(data$snodas, breaks = c(-1, 0,100,200,300,400), labels = c("none", ">100", ">200", ">300", ">400"))
data$onsite_FAC10 <- cut(data$depth_G, breaks = c(-1, 0,100,200,300,400), labels = c("none", "<100", "<200", "<300", ">300"))




#### Needed Plots ####
# set plot window for diagnostic plots and hitograms
par(mfrow=c(2,2))


#' The difference is not normally distributed. IT has a positive (right skewdness),
#' and contains negative numbers
hist(data$MeanT_Diff)
hist(data$MeanT_Air)
hist(data$MeanT_Tire)
hist(data$snodas)

####CORR###
# Kruskal-Wallis test

## cover and diff 
kruskal.test( MeanT_Diff ~ cover, data)

### depth_T ~ Max T ###

dt_max.m21.22.i <- glm(MaxT_Tire ~ MaxT_Ambient + depth_T + (MaxT_Ambient*depth_T) , data = data)
dt_max.m21.22 <- glm(MaxT_Tire ~ MaxT_Ambient + depth_T , data = data)
dt_max.m21.22.nosnow <- glm(MaxT_Tire ~ MaxT_Ambient , data = data)
dt_max.m21.22.null <- glm(MaxT_Tire ~ 1 , data = data)

performance(dt_max.m21.22.null)
performance(dt_max.m21.22.nosnow)
performance(dt_max.m21.22)
performance(dt_max.m21.22.i) 

check_model(dt_max.m21.22.i)
summary(dt_max.m21.22.i)

### depth_T ~ Min T ###

dt_min.m21.22.i <- glm(MinT_Tire ~ MinT_Ambient + depth_T + (MinT_Ambient*depth_T) , data = data)
dt_min.m21.22 <- glm(MinT_Tire ~ MinT_Ambient + depth_T , data = data)
dt_min.m21.22.nosnow <- glm(MinT_Tire ~ MinT_Ambient , data = data)
dt_min.m21.22.null <- glm(MinT_Tire ~ 1 , data = data)

performance(dt_min.m21.22.null)
performance(dt_min.m21.22.nosnow)
performance(dt_min.m21.22)
performance(dt_min.m21.22.i) 

check_model(dt_min.m21.22.i)
summary(dt_min.m21.22.i)

#### MODELS ####

#'### Model 1: Differnece in Mean daily temperature ~ snow cover extent
#'
cover.glm <- glm(MeanT_Diff ~cover, data= data)
summary(cover.glm)
plot(cover.glm)
overdisp_fun(cover.glm)
tab_model(cover.glm)
#' The amount of tire covered significantly contibuted to differnce between the 
#' tire and air temperature. 
#' 
#' function is overdispearsed
#' 

cover.glmb <- lm(MeanT_Diff ~ cover + MeanT_Air , data= data)
summary(cover.glmb)
AIC(cover.glmb)
plot(cover.glmb)
overdisp_fun(cover.glmb)


#'### Model 2: Mean daily tire temperature ~ Mean daily air temperature + cover
#'
cover.in.glm <- glm(MeanT_Tire ~ MeanT_Air + cover, data = data)
summary(cover.in.glm)
#' Model has resonable fit
plot(cover.in.glm)
#' model is currently overdisp
overdisp_fun(cover.in.glm)


cover.in.glm2 <- glm(MeanT_Tire ~ MeanT_Air + cover + MeanT_Air:cover, data = data)
#' Improved fit from previous models indicated by AICc = -1025.881
summary(cover.in.glm2)
#' Model has good fit
plot(cover.in.glm2)
# 'Model is overdisp
overdisp_fun(cover.in.glm2)

cover.in.glm2b <- glm(MeanT_Tire ~ MeanT_Air + cover + MeanT_Air:cover, data = data)
#' Improved fit from previous models indicated by AICc = -1025.881
summary(cover.in.glm2)
#' Model has good fit
plot(cover.in.glm2)
# 'Model is overdisp
overdisp_fun(cover.in.glm2)


cover.in.glm3 <- lmer(MeanT_Tire ~ MeanT_Air + cover + MeanT_Air:cover + (1|number), data = data)
#' Improved fit from previous models indicated by AICc = -1025.881
summary(cover.in.glm3)
#' Model has good fit
plot(cover.in.glm3)
# 'Model is overdisp
overdisp_fun(cover.in.glm3)
tab_model(cover.in.glm3)
plot_model(cover.in.glm3)



#### Summary Data ####
# tally of the number of obs. in each sample 
data%>%
  group_by(cover) %>%
  group_by(snow_FAC10) %>%
  dplyr::summarise( cover = n_distinct(cover))

# 
data%>%
  group_by(cover) %>%
  summarise(n = , n(),
            min_gound =min(depth_G, na.rm = T),
            min_snodas = min(snodas, na.rm = T),
            mean_ground = mean(depth_G, na.rm = T),
            sd_ground = sd(depth_G, na.rm = T),
            mean_snodas = mean(snodas, na.rm = T),
            sd_snodad = sd(snodas, na.rm = T),
            med_ground = median(depth_G, na.rm = T),
            med_snodas = median(snodas, na.rm = T),
            diff_temp = mean(MeanT_Diff, na.rm = T), 
            sd_diff_temp = sd(MeanT_Diff, na.rm = T))

