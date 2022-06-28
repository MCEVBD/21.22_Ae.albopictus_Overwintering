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
#'           - the ditribution is skewed to 0, given bounding at 0 and 1 pdf = beta
#'    2. histograms of GDDs 
#'           - no clear distribution
#'    3. GDDs v per.sur.live
#'           - -12??C is very similar a cross all sites, 10C may be informative 
#'        a. null model: per.sur.live ~ 1 
#'           - Log-likelihood: 271.2 on 2 Df
#'        b. GDDdjf10:  GDDs (at 10C) was not a significant predictor of surival 
#'        c. GDDfull10: GDDs (at 10C) was not a significant predictor of surival 
#'    4. Mean Jan T v per.sur.live
#'
#'    5. 

rm (list  = ls())

#libraries# 
library(ggplot2)
library(betareg)
library(sjPlot)
library(patchwork)

#### Functions ####
overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}


#### Import data ####
setwd("~/Documents/CBS_PhD/Ae.albo_OW_2021/21.22_Ae.albopictus_Overwintering")
# import data avg. to the tire level 
tire <- read.csv("00_Data/21.22_bytire.hatch_temperature_summary_NO.Arl3.csv")
# format corrections
tire <- subset(tire, select = -c(X))
tire$site    <- factor(tire$site, levels = c("", "Car", "Bon", "Dek", "Arl", "Han", "Spo"))
tire$loc.id <- factor(tire$loc.id, levels = c("CTR",
                                              "IL_Car_T1", "IL_Car_T2",
                                              "IL_Bon_T1", "IL_Bon_T2",
                                              "IL_Dek_T1", "IL_Dek_T2",
                                              "WI_Arl_T1", "WI_Arl_T2",
                                              "WI_Han_T1", "WI_Han_T2",
                                              "WI_Spo_T1" ,"WI_Spo_T2"))

# import data avg. to the site
site <- read.csv("00_Data/21.22_bysite.hatch_temperature_summary_NO.Arl3.csv")
# format
site$site    <- factor(site$site, levels = c("CTR", "Car", "Bon", "Dek", "Arl", "Han", "Spo"))
site <- subset(site, select = -c(X))



#'## 1. histogram of percent survival
#'
#' Postitive skewed pdf. Large number of 0s. could be rare event zero inflation. (Ref. for 
#' zero inflation [here](https://espace.library.uq.edu.au/data/UQ_9053/tm_ele_8_05.pdf?Expires=1655991548&Key-Pair-Id=APKAJKNBJ4MJBJNC6NLQ&Signature=BFqhZraaKjEPZ-bfYlqtT7R2wdjr9rRiS3ZKp8Gc16QDLOOKaGW3o3btT5feWJALXUJnwfPRDWuwyv0qjHxHDB9UPCHGZMnUq0rUBBpBbQaDo2lukL~hEG2BA9v4nU1uDdrBbKb8S2-F8Re-4BeukFj9upWsoLwgJMprW0L32pJEdvZxE1q42AqhE1Ct3VzrtE9Qp1EJgzQQpaCUsTnF2DKOHmQBOXF2qX6N0qEkS1OITfivynX~S~1vrj0hYkYZF1qoEqmZV-OKnX7DOHSKj1-SbYJHB0G~e2W6V85dFGWKjDFzds2UiyexKcHTpQlNeh1FIuUIFGTOnUceK0Tbrw__) )

hist(tire$per.sur.live)

#'## 2. histogram of differnt GDDs 
#' 
#' minimal distribution. high or low value
par(mfrow=c(2,2))
hist(tire$GDD_10_DJF)
hist(tire$GDD_n12_DJF)
hist(tire$GDD_10_FULL)
hist(tire$GDD_n12_DJF)
par(mfrow=c(1,1))

#'## 3. GDDs v survival 
#'


p1 <- ggplot(tire, aes(GDD_10_DJF, per.sur.live, col = site)) +
  geom_point()+
  ggtitle("GDD_10_DJF")

p2 <- ggplot(tire, aes(GDD_n12_DJF, per.sur.live, col = site)) +
  geom_point() +
  ggtitle("GDD_n12_DJF")

p3 <- ggplot(tire, aes(GDD_10_FULL, per.sur.live, col = site)) +
  geom_point() +
  ggtitle("GDD_10_FULL")

p4 <- ggplot(tire, aes(GDD_n12_FULL, per.sur.live, col = site)) +
  geom_point() +
  ggtitle("GDD_n12_FULL")

p1 +p3 + p2+ p4
#' FIGURE NOTES:
#' 
#' The -12 C GDDs seem to be extreamly similar and are unlikely to be significant. there 
#' may be some significance to the 10 C varaibles, the lack of survial in Bon regardless of GDD
#' may limit this. 

#'### Models
###
# return to 2,2 plots of model test plots 
par(mfrow=c(2,2))

#' **Null model**
# null model 
null <- glm(per.sur.live ~ 1, data = tire)
summary(null)

#' **GDD (set to temperatures => 10 ??C) during DJF**
# GDD (djf 10??C)
GDDdjf10 <- glm(per.sur.live ~ GDD_10_DJF, data = tire)
summary(GDDdjf10)
tab_model(GDDdjf10)
#measures of fitness 
plot(GDDdjf10)
overdisp_fun(GDDdjf10)
#' *model fitness*
#' 
#' The R vs F shows not distint patterns. The Nor. Q-Q has some deviation from the normal pdf a
#' the higher quantiles. There is some slight heteroscedasticity. Obervations 4 and 5 are influental caes.
#' 4 and 5 are the tires at Carbondale, this could be due to the much warmer conditions there as compared to 
#' the cold and survival-less northern sites
#' 
#' The nodel is no overdispered
tab_model(GDDdjf10)

#' **GDD (set to temperatures => 10 ??C) during full time**
# GDD (full 10??C)
GDDfull10 <- glm(per.sur.live ~ GDD_10_FULL, data = tire)
summary(GDDfull10)
plot(GDDfull10)
overdisp_fun(GDDfull10)
#' *model fitness*
#' 
#' The R v F plot shows some pattern but seems to be okay. The Nor. Q-Q looks good. There may be slight 
#' heteroscedasiticity. Observation 4 and 1 drive the trend (Bon and Car)
#' 
#' is not overdisp.
#' 
tab_model(GDDfull10)

#' **GDD (set to temperatures => -12 C) during DJF time**
# GDD (full 10??C)
GDDdjfn12 <- glm(per.sur.live ~ GDD_n12_DJF, data = tire)
summary(GDDdjfn12)
plot(GDDdjfn12)
overdisp_fun(GDDdjfn12)
#' *model fitness*
#' 
#' The R v F plot shows some pattern but seems to be okay. The Nor. Q-Q looks okay~. The upper
#' observations are not great.  There may be slight heteroscedasiticity. Observation 9 (Arl 2)
#' 
#' is not overdisp.
tab_model(GDDdjfn12)

#' **GDD (set to temperatures => -12 C) during Full time**
# GDD (full 10??C)
GDDfulln12 <- glm(per.sur.live ~ GDD_n12_FULL, data = tire)
summary(GDDfulln12)
plot(GDDfulln12)
overdisp_fun(GDDfulln12)
#' *model fitness*
#' 
#' The R v F plot shows some pattern but seems to be okay. The Nor. Q-Q looks okay~. The upper
#' observations are not great.  There may be slight heteroscedasiticity. Observation 9 (Arl 2) 
#' had a large influance. 
#' 
tab_model(GDDfulln12)

#### 4. Survival v Mean Jan tire temperature ####

ggplot(tire, aes(JANmeanT, per.sur.live, col = site))+
  geom_point()

#### 5. Survial v DaysB12T

ggplot(tire, aes(DaysB12T, per.sur.live, col = site))+
  geom_point()

#### 5. Survial v hrB12Tcon

ggplot(tire, aes(MAXHrsB12conT, per.sur.live, col = site))+
  geom_point()
