#' ---
#' title: "14 Inital survival Analysis"
#' author: "Katie M Susong"
#' date: "03-June-22"
#' ---

#'Overview
#'========
#'
#' Inital plots and correlation for survival/hatch rate.
#' 
#' 
#' Plots and Analysis
#' -------------------
#' 
#'    1. histogram of percent survival
#'        - the ditribution is skewed to 0, given bounding at 0 and 1 pdf = beta
#'    2. histograms of GDDs 
#'        - no clear distribution
#'    3. GDDs v per.sur.live
#'        - -12??C is very similar a cross all sites, 10C may be informative 
#'      a. null model: per.sur.live ~ 1 
#'           - Log-likelihood: 271.2 on 2 Df
#'      b. GDDdjf10:  GDDs (at 10C) was not a significant predictor of surival 
#'      c. GDDfull10: GDDs (at 10C) was not a significant predictor of surival 
#'    4. Mean Jan T v per.sur.live
#'  
#'    


#libraries# 
library(ggplot2)
library(betareg)
library(sjPlot)

#### Import data ####
data <- read.csv("00_Data/21.22_hatch_temperature_summary_NO.Arl3.csv")
# format corrections
data$number <- as.factor(data$number)
data <- subset(data, select = -c(X.1, X))


#### 1. histogram of percent survival ####
hist(data$per.sur.live)

#### 2. histogram of differnt GDDs ####
par(mfrow=c(2,2))
hist(data$GDD_10_DJF,breaks = 10)
hist(data$GDD_n12_DJF)
hist(data$GDD_10_FULL)
hist(data$GDD_n12_DJF)
par(mfrow=c(1,1))
#### 3. GDDs v survival ####

ggplot(data, aes(GDD_10_DJF, per.sur.live, col = site)) +
  geom_point()+
  xlim(c(0,600))

ggplot(data, aes(GDD_n12_DJF, per.sur.live, col = site)) +
  geom_point() +
  xlim(0,5000)

ggplot(data, aes(GDD_10_FULL, per.sur.live, col = site)) +
  geom_point()

ggplot(data, aes(GDD_n12_FULL, per.sur.live, col = site)) +
  geom_point()


# Models
###
# to used a beta distribution all 0s need to be made non-significantly not 0
data$per.sur.live <- data$per.sur.live + 0.000000001
data$GDD_10_DJF <- data$GDD_10_DJF + 0.000000001
data$JANmeanT <- data$JANmeanT + 0.000000001

# null model 
null <- betareg(per.sur.live ~ 1, data = data)
summary(null)
plot(null)

# GDD (djf 10??C)
GDDdjf10 <- betareg(per.sur.live ~ GDD_10_DJF, data = data)
summary(GDDdjf10)
tab_model(GDDdjf10)

# GDD (full 10??C)
GDDfull10 <- betareg(per.sur.live ~ GDD_10_FULL, data = data)
summary(GDDfull10)
plot(GDDfull10)
tab_model(GDDfull10)

#### 4. Survival v Mean Jan tire temperature ####

ggplot(data, aes(JANmeanT, per.sur.live, col = site))+
  geom_point()

janT <- betareg(per.sur.live ~ JANmeanT, data = data)
summary(GDDfull10)
plot(GDDfull10)
tab_model(GDDfull10)


#### 5. Survial v DaysB12T

ggplot(data, aes(DaysB12T, per.sur.live, col = site))+
  geom_point()

#### 5. Survial v hrB12Tcon

ggplot(data, aes(MAXHrsB12conT, per.sur.live, col = site))+
  geom_point()
