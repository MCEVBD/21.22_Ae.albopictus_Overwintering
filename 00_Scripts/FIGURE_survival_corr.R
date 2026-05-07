#' ---
#' title: FIGURE: Survival Correlations
#' author: Katie M Susong
#' date: 02-Aug-2022
#' ---
#' 

rm (list  = ls())
# Libraries
#data formatting
library(dplyr)
library(tidyr)
library(lubridate)
#general plotting
library(ggplot2)
library(cowplot)

#### Import data ####
setwd("~/Documents/CBS_PhD/Ae.albo_OW_2021/21.22_Ae.albopictus_Overwintering")
# import data avg. to the tire level 
tire <- read.csv("00_Data/21.22_bytire.hatch_temperature_summary_NO.Arl3.csv")
# format corrections
tire <- subset(tire, select = -c(X))
tire$site    <- factor(tire$site, levels = c("Spo","Han","Arl","Dek", "Bon", "Car", ""))
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
site$site <-factor(site$site, levels = c("Spo","Han","Arl","Dek", "Bon", "Car"))
site <- subset(site, select = -c(X))

# filter control out of df
site_n <- site %>%
  filter( site != "CTR")
tire_n <- tire %>%
  filter( loc.id != "CTR")


#'### Plot 1: GDDS ( 10 C) impact
#'

p1 <- ggplot(tire_n,aes(GDD_10_DJF, per.sur.live)) + 
  geom_point(aes(col = site), size = 3) +
  geom_smooth(method = "lm", col = "black", linetype = "dashed") +
  theme_linedraw() +
  geom_hline( yintercept = 0) +
  ylab ( "Percent Hatch Survival") +
  xlab ( "DJF Growing Degree Days (set at 10 ??C), within the Tire") +
  theme(legend.position = "none")+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))
    

#'### Plot 2: Mean Jan T 
#'

p2 <- ggplot(tire_n,aes(JANmeanT, per.sur.live)) + 
  geom_point(aes(col = site), size = 3) +
  geom_smooth(method = "lm", col = "black", linetype = "dashed") +
  theme_linedraw() +
  geom_hline( yintercept = 0) +
  ylab ( "Percent Hatch Survival") +
  xlab ( "Mean Daily January Temperature (??C), within the Tire") +
  theme(legend.position = "none") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

#'### Plot :Days Below -12 C 
#'

p3 <- ggplot(tire_n,aes(DaysB12T, per.sur.live)) + 
  geom_point(aes(col = site), size = 3) +
  geom_smooth(method = "lm", col = "black", linetype = "dashed") +
  theme_linedraw() +
  geom_hline( yintercept = 0) +
  ylab ( "Percent Hatch Survival") +
  xlab ( "Days with a temperature below -12 ??C, within the Tire") +
  theme(legend.position = "none") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

#'### Plot :Days Below -12 C 
#'

p4 <- ggplot(tire_n[c(1:7, 9:12),],aes(FreThaT, per.sur.live)) + 
  geom_point(aes(col = site), size = 3) +
  geom_smooth(method = "lm", col = "black", linetype = "dashed") +
  theme_linedraw() +
  ylab ( "Percent Hatch Survival") +
  xlab ( "Freese-Thaw events, within the Tire") +
  geom_hline( yintercept = 0) +
  theme(legend.position = "none") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

p.full <- plot_grid(  p1, p2, p3, p4) 

# save high res. version of plot
setwd("~/Documents/CBS_PhD/Ae.albo_OW_2021/03_figures_presentation/Figures")
ggsave("survival_corr.pdf", plot = p.full, width = 12, height = 6, units = "in", dpi = 500)
