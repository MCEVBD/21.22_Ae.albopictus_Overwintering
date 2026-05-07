#' ---
#' title: " 21 Glm, max daily temperature analysis"
#' author: "Katie Susong"
#' date: "30 May 2023"
#' ---

#' Overview
#' ========
#' 
#'         

rm (list  = ls())


# Libraries #
library(lubridate)
library(dplyr)
library(lme4)

library(cowplot)
library(ggplot2)
library(sjPlot) #for plotting lmer and glmer mods

library(performance)
library(AICcmodavg)

#### Import data ####
#18/19 data
DIA <- read.csv("~/Documents/CBS_PhD/albopictus_OW/Data/SnowSite_20230530_kms.csv")
# format correction
DIA$date <- ymd(DIA$date) # correct data format
DIA$Location <- factor(DIA$Location , 
                       levels=c("New Berlin", "NWMAD","SCCMAD" ,"Kankakee", "Champaign-Urbana")) #fix location order to STD 
DIA$snow_FAC10 <- factor(DIA$snow_FAC10,
                         levels = c("none", ">100", ">200", ">300",">400" ))
DIA$snow_FAC10 <- factor(DIA$snow_FAC10,
                         levels = c("none", "<100", "<200", "<300", ">300"))
DIA$site <- as.factor(DIA$site)


# 21/22 data
data <- read.csv('00_Data/21.22_snow_temperature.csv')
# format corrections
data$number   <- as.factor (data$number)
data$Date     <- ymd (data$Date)
data$location <- factor (data$location, levels = c("Car", "Bon", "Dek", "Arl", "Han", "Spo"))
data          <- filter (data, Date < "2022-04-14" )


#### Change data(21/22) df format and colnames to match DIA(18/19) df ####

# add catagorical snow values @ 100mm bins
data$snow_FAC10 <- cut(data$snodas, breaks = c(-1, 0,100,200,300,400), labels = c("none", "<100", "<200", "<300", ">300"))
data$onsite_FAC10 <- cut(data$depth_G, breaks = c(-1, 0,100,200,300,400), labels = c("none", "<100", "<200", "<300", ">300"))

#summary of snodas vs on site
table(data$onsite_FAC10 == data$snow_FAC10)

data %>%
  group_by(site) %>%
  summarize(agreement_count = sum(onsite_FAC10 == snow_FAC10))
# rename MeanT_Air
data <- rename(data, MeanT_Ambient = "MeanT_Air",
               MinT_Ambient = "MinT_Air",
               MaxT_Ambient = "MaxT_Air")
# rename location
data <- rename(data, site = "location")
# rename Date 
data <- rename(data, date = "Date")

#### Merge DFs ####

data.sub <- subset(data, select = c("date", 
                                    "MeanT_Ambient",
                                    "MeanT_Tire",
                                    "MinT_Ambient", 
                                    "MinT_Tire", 
                                    "MaxT_Ambient",
                                    "MaxT_Tire", 
                                    "site",
                                    "snow_FAC10"))
dia.sub <- subset(DIA, select = c("date", 
                                  "MeanT_Ambient",
                                  "MeanT_Tire",
                                  "MinT_Ambient", 
                                  "MinT_Tire", 
                                  "MaxT_Ambient",
                                  "MaxT_Tire", 
                                  "site",
                                  "snow_FAC10"))
both <- rbind(data.sub, dia.sub)

both$DiffT_Tire <- both$MaxT_Tire - both$MinT_Tire
both$DiffT_Ambient <- both$MaxT_Ambient - both$MinT_Ambient

#### 21/22 onsite analysis ####

##MAX##
ON_max.m21.22.i <- glm(MaxT_Tire ~ MaxT_Ambient + onsite_FAC10 + (MaxT_Ambient*onsite_FAC10) , data = data)
ON_max.m21.22 <- glm(MaxT_Tire ~ MaxT_Ambient + onsite_FAC10 , data = data)
ON_max.m21.22.nosnow <- glm(MaxT_Tire ~ MaxT_Ambient , data = data)
ON_max.m21.22.null <- glm(MaxT_Tire ~ 1 , data = data)

#model selection

performance(ON_max.m21.22.null)
performance(ON_max.m21.22.nosnow)
performance(ON_max.m21.22)
performance(ON_max.m21.22.i) #11560.119

check_model(ON_max.m21.22.i)

tab_model(ON_max.m21.22)


##MIN##
ON_min.m21.22.i <- glm(MinT_Tire ~ MinT_Ambient + onsite_FAC10 + (MinT_Ambient*onsite_FAC10) , data = data)
ON_min.m21.22 <- glm(MinT_Tire ~ MinT_Ambient + onsite_FAC10 , data = data)
ON_min.m21.22.nosnow <- glm(MinT_Tire ~ MinT_Ambient , data = data)
ON_min.m21.22.null <- glm(MinT_Tire ~ 1 , data = data)

##AIC model selection##

performance(ON_min.m21.22.null)
performance(ON_min.m21.22.nosnow)
performance(ON_min.m21.22)
performance(ON_min.m21.22.i) # 8548.384

check_model(ON_min.m21.22.i)
tab_model(ON_min.m21.22)

##### SAVE MERGED DF #####
#write_csv(both, "00_Data/allsite_TempSnow_data.csv")

#### Subset larger df ####
# Split Data into Training and Testing in R 
sample_size = floor(0.7*nrow(both))
set.seed(3495)

# randomly split data in r
picked = sample(seq_len(nrow(both)),size = sample_size)
development =both[picked,]
holdout =both[-picked,]

#### generate Mean models ####

m18.19 <- glm(MeanT_Tire ~ MeanT_Ambient + snow_FAC10 + (MeanT_Ambient*snow_FAC10) , data = DIA)
m21.22 <- glm(MeanT_Tire ~ MeanT_Ambient + snow_FAC10 + (MeanT_Ambient*snow_FAC10) , data = data)
m.all  <- glm(MeanT_Tire ~ MeanT_Ambient + snow_FAC10 + (MeanT_Ambient*snow_FAC10) , data = development)


# Check models #



tab_model(m.all)
#### generate Max models ####

max.m18.19.i <- glm(MaxT_Tire ~ MaxT_Ambient + snow_FAC10 + (MaxT_Ambient*snow_FAC10) , data = DIA)
max.m21.22.i <- glm(MaxT_Tire ~ MaxT_Ambient + snow_FAC10 + (MaxT_Ambient*snow_FAC10) , data = data)



max.m18.19 <- glm(MaxT_Tire ~ MaxT_Ambient + snow_FAC10 , data = DIA)
max.m21.22 <- glm(MaxT_Tire ~ MaxT_Ambient + snow_FAC10 , data = data)

max.m.all.i  <- glm(MaxT_Tire ~ MaxT_Ambient + snow_FAC10 + (MaxT_Ambient*snow_FAC10) , data = development)
max.m.all  <- glm(MaxT_Tire ~ MaxT_Ambient + snow_FAC10 , data = development)

# Check models #

performance(max.m.all)
performance(max.m21.22)
performance(max.m18.19)

performance(max.m.all.i)
performance(max.m21.22.i) # 11738.861
performance(max.m18.19.i)

check_model(max.m.all)
check_model(max.m.all.i)

check_model(max.m21.22.i)
check_model(max.m18.19)


plot_models(max.m18.19, max.m21.22, max.m.all)
plot_models(max.m18.19.i, max.m21.22.i, max.m.all.i)
tab_model(max.m.all.i)

# Generate predict on holdout df #
max.holdout <- holdout
max.holdout$m18.19 <- predict(max.m18.19.i, holdout, type = "response",allow.new.levels = T)
max.holdout$m21.22 <- predict(max.m21.22.i, holdout, type = "response",allow.new.levels = T)
max.holdout$m.all  <- predict(max.m.all.i,  holdout, type = "response",allow.new.levels = T)


# Compare model estimates #
# on site v snodas #

plot_models(ON_max.m21.22.i, max.m21.22.i, max.m.all.i)

# Compare model estimates #

fig1 <- plot_models(max.m18.19, max.m21.22, max.m.all, title = "",
                    axis.labels = c("Max Ambient temperature * Snow, >300 mm",
                                    "MAx Ambient temperature* Snow, 200-299 mm",
                                    "Snow * Max Ambient temperature, 100-199 mm",
                                    "Snow * Max Ambient temperature, 1-99 mm",
                                    "Snow, >300 mm","Snow, 200-299 mm","Snow, 100-199 mm",
                                    "Snow, 1-99 mm","Max Ambient temperature", "Intercept"),
                    show.intercept = T,
                    vline.color = "grey70") +
  theme_linedraw() +
  geom_vline(xintercept = 0) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  labs(color = "Regression") +
  scale_color_discrete(labels=c('Mixed','2021/2022','2018/2019'))

# save high res. version of plot
setwd("~/Documents/CBS_PhD/Ae.albo_OW_2021/03_figures_presentation/Figures")
ggsave("compare_models_forest.pdf", plot = fig1, width = 10, height = 5, units = "in", dpi = 500)

ggplot(max.holdout, aes(MaxT_Tire, m18.19))+
  geom_point(col = "blue") +
  geom_point(aes(MaxT_Tire, m21.22), col = "red")+
  geom_point(aes(MaxT_Tire, m.all)) +
  facet_wrap(~snow_FAC10) +
  ylab("predicted T (C??)")



#### generate Min models ####

Min.m18.19.i <- glm(MinT_Tire ~ MinT_Ambient + snow_FAC10 + (MinT_Ambient*snow_FAC10) , data = DIA)
Min.m21.22.i <- glm(MinT_Tire ~ MinT_Ambient + snow_FAC10 + (MinT_Ambient*snow_FAC10) , data = data)
Min.m.all.i  <- glm(MinT_Tire ~ MinT_Ambient + snow_FAC10 + (MinT_Ambient*snow_FAC10) , data = development)


Min.m18.19 <- glm(MinT_Tire ~ MinT_Ambient + snow_FAC10 , data = DIA)
Min.m21.22 <- glm(MinT_Tire ~ MinT_Ambient + snow_FAC10 , data = data)
Min.m.all  <- glm(MinT_Tire ~ MinT_Ambient + snow_FAC10, data = development)

# Check models #

performance(Min.m.all)
performance(Min.m21.22)
performance(Min.m18.19)

performance(Min.m.all.i)
performance(Min.m21.22.i) # 8987.952
performance(Min.m18.19.i)

check_model(Min.m.all)
check_model(Min.m.all.i)

check_model(Min.m21.22.i)
check_model(Min.m18.19)


plot_models(Min.m18.19, Min.m21.22, Min.m.all)
plot_models(Min.m18.19.i, Min.m21.22.i, Min.m.all.i)
tab_model(Min.m.all.i)

# Generate predict on holdout df #
Min.holdout <- holdout
Min.holdout$m18.19 <- predict(Min.m18.19.i, holdout, type = "response",allow.new.levels = T)
Min.holdout$m21.22 <- predict(Min.m21.22.i, holdout, type = "response",allow.new.levels = T)
Min.holdout$m.all  <- predict(Min.m.all.i,  holdout, type = "response",allow.new.levels = T)


# Compare model estimates #
# on site v snodas #

plot_models(ON_min.m21.22.i, Min.m21.22.i, Min.m.all.i)


# Compare model estimates #

# save high res. version of plot
setwd("~/Documents/CBS_PhD/Ae.albo_OW_2021/03_figures_presentation/Figures")
ggsave("compare_models_forest.pdf", plot = fig1, width = 10, height = 5, units = "in", dpi = 500)

ggplot(Min.holdout, aes(MinT_Tire, m18.19))+
  geom_point(col = "blue") +
  geom_point(aes(MinT_Tire, m21.22), col = "red")+
  geom_point(aes(MinT_Tire, m.all)) +
  facet_wrap(~snow_FAC10) +
  ylab("predicted T (C??)")

Min.m.all.i$coef
confint(Min.m.all.i)
min.effects <- fixef(Min.m.all.i)



### PLOT - Pretty version of onsite vs snodas data plot ###


# i need to change the names of vaiables so they match. This means two dfs

data_onsite <- subset(data, select = c("date", 
                                       "MeanT_Ambient",
                                       "MeanT_Tire",
                                       "MinT_Ambient", 
                                       "MinT_Tire", 
                                       "MaxT_Ambient",
                                       "MaxT_Tire", 
                                       "site",
                                       "onsite_FAC10"))
data_onsite <- rename(data_onsite, Snow = onsite_FAC10) 
data_snodas <- subset(data, select = c("date", 
                                       "MeanT_Ambient",
                                       "MeanT_Tire",
                                       "MinT_Ambient", 
                                       "MinT_Tire", 
                                       "MaxT_Ambient",
                                       "MaxT_Tire", 
                                       "site",
                                       "snow_FAC10"))
data_snodas <- rename(data_snodas, Snow = snow_FAC10) 

# build selected models 

# mean model#

onsite_mean_final <- glm(MeanT_Tire ~ MeanT_Ambient + Snow + (MeanT_Ambient*Snow) , data = data_onsite)
snodas_mean_final <- glm(MeanT_Tire ~ MeanT_Ambient + Snow + (MeanT_Ambient*Snow) , data = data_snodas)


# max model #

onsite_max_final <- glm(MaxT_Tire ~ MaxT_Ambient + Snow + (MaxT_Ambient*Snow) , data = data_onsite)
snodas_max_final <- glm(MaxT_Tire ~ MaxT_Ambient + Snow + (MaxT_Ambient*Snow) , data = data_snodas)

plot_models(onsite_max_final, snodas_max_final)
a <- plot_models(onsite_max_final, snodas_max_final,title = "",
            axis.labels = c("Max External temp. * Snow: >300 mm",
                            "Snow: >300 mm",
                            "Max External temp. * Snow: 200-299 mm",
                            "Max External temp. * Snow: 100-199 mm",
                            "Max External temp. * Snow: 1-99 mm",
                            "Snow: 200-299 mm",
                            "Snow: 100-199 mm",
                            "Snow: 1-99 mm",
                            "Max External temp."),
            show.intercept = F,
            vline.color = "black") +
  theme_cowplot() +
  geom_vline(xintercept = 0) +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=10,face="bold")) +
  labs(color = "Regression") +
  scale_color_discrete(labels=c('SNODAS', 'Onsite')) +
  #theme(legend.position = c(.15,.90))
  theme(legend.position = "none")
  


onsite_min_final <- glm(MinT_Tire ~ MinT_Ambient + Snow + (MinT_Ambient*Snow) , data = data_onsite)
snodas_min_final <- glm(MinT_Tire ~ MinT_Ambient + Snow + (MinT_Ambient*Snow) , data = data_snodas)

plot_models(onsite_min_final, snodas_min_final)
b <- plot_models(onsite_min_final, snodas_min_final,title = "",
            axis.labels = c("Min. External temp. * Snow: >300 mm",
                            "Snow: >300 mm",
                            "Min. External temp.* Snow: 200-299 mm",
                            "Min. External temp.* Snow: 100-199 mm",
                            "Min. External temp.* Snow: 1-99 mm",
                            "Snow: 200-299 mm",
                            "Snow: 100-199 mm",
                            "Snow: 1-99 mm",
                            "Min. External temp."),
            show.intercept = F,
            vline.color = "black") +
  theme_cowplot() +
  geom_vline(xintercept = 0) +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=10,face="bold")) +
  labs(color = "Snow depth") +
  scale_color_discrete(labels=c('SNODAS','Onsite'))+
  theme(legend.position = c(.60,.15))
  #theme(legend.position = "none")

onsiteVsnodas <- plot_grid(a,b)

# save high res. version of plot
setwd("~/Documents/CBS_PhD/Ae.albo_OW_2021/03_figures_presentation/Figures")
ggsave("onsiteVsnodas_forest.pdf", plot = onsiteVsnodas, dpi = 500)
